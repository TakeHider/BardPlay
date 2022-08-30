package main

// 以下を実行すること
// > go mod tidy

import (
	"fmt"
	"log"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"syscall"
	"time"
	"unsafe"

	"fyne.io/fyne/app"
	"fyne.io/fyne/theme"
	"fyne.io/fyne/widget"
	"gopkg.in/ini.v1"
)

var (
	appTitle = "Bard Play"
	// ウインドウオブジェクト
	wndApp    = app.New()
	wndWindow = wndApp.NewWindow(appTitle)
	// スレッド処理用
	blnProcRunning = false
	// DLL
	dll            = syscall.NewLazyDLL("MIDIIO.dll")
	GetDeviceName  = dll.NewProc("MIDIIn_GetDeviceNameW")
	OpenMIDIIn     = dll.NewProc("MIDIIn_OpenW")
	GetMIDIMessage = dll.NewProc("MIDIIn_GetMIDIMessage")
	CloseMIDIIn    = dll.NewProc("MIDIIn_Close")

	dllWin32    = syscall.NewLazyDLL("user32.dll")
	SendMessage = dllWin32.NewProc("PostMessage")
	//SendMessage = dllWin32.NewProc("SendMessage")

	szDeviceName [32]uint8

	NOTE_ON  uint8 = 0x90
	NOTE_OFF uint8 = 0x80

	WM_KEYUP   uint32 = 0x101
	WM_KEYDOWN uint32 = 0x100

	KEY_CODE = map[string]int{"": 0}

	aryKeyMap [127]string

	EXIT_OUTRANGE = 0 // 範囲外の鍵盤が押されたら抜けるか？
	PORT_IN       = 0 // ポート番号
	KEY_MIN       = 0 // キーマッピングの最低値
	KEY_MAX       = 0 // キーマッピングの最高値
)

// メイン関数
func main() {
	var strDeviceName string // MIDIデバイス名（ウインドウに表示する）
	// キーマッピングの初期化
	initKeyMap()
	// iniファイルの読み込み
	if initIni() == 0 {
		strDeviceName = "*** INI FILE READ ERROR ***"
	}

	// デバイス名の取得
	intMIDIReady, _, _ := GetDeviceName.Call(IntPtr(PORT_IN), uintptr(unsafe.Pointer(&szDeviceName)), 32)
	if intMIDIReady != 0 {
		// MIDIデバイス名をセット
		strDeviceName = strings.TrimSpace(string(szDeviceName[:]))
	} else {
		// 戻り値が0の時はデバイス無し
		strDeviceName = "*** NO MIDI DEVICE ***"
	}
	// デバイス名(兼メッセージ)
	lblDeviceName := widget.NewLabel(strDeviceName)

	// スタートボタン
	btnStart := widget.NewButtonWithIcon("Start", theme.MediaPlayIcon(), func() {
		wndWindow.SetTitle(appTitle + " -- PLAY")

		// プロセスが動いていなかったら、実行させる
		if !blnProcRunning {
			go InputMIDI()
		}
	})

	// 止めるボタン
	btnStop := widget.NewButtonWithIcon("Stop", theme.MediaPauseIcon(), func() {
		wndWindow.SetTitle(appTitle + " -- STOP")
		blnProcRunning = false
	})
	// 終了ボタン
	btnQuit := widget.NewButtonWithIcon("Quit", theme.CancelIcon(), func() {
		wndApp.Quit()
	})

	// MIDIのデバイスが見つからない時は、ボタンをDisabledにする
	if intMIDIReady == 0 {
		btnStart.Disable() // MIDIデバイスが無いときはボタンを非活性にする
		btnStop.Disable()  // MIDIデバイスが無いときはボタンを非活性にする
	}
	// 画面の部品を乗せる
	wndWindow.SetContent(
		widget.NewHBox(lblDeviceName, btnStart, btnStop, btnQuit),
	)
	// 画面の表示
	wndWindow.ShowAndRun()
}

// MIDIイベント入力処理
func InputMIDI() {
	var byMessage [256]uint8
	var byPreNote uint8 = 0
	blnProcRunning = true

	pMIDIIn, _, _ := OpenMIDIIn.Call(uintptr(unsafe.Pointer(&szDeviceName)))
	defer CloseMIDIIn.Call(pMIDIIn)
	if pMIDIIn != 0 {
		// 安全のため 10秒だけ動かす
		for now := time.Now(); blnProcRunning && time.Since(now).Seconds() < 10; {
			// メッセージの受信
			iRet, _, _ := GetMIDIMessage.Call(pMIDIIn, uintptr(unsafe.Pointer(&byMessage)), 256)
			if iRet >= 3 {

				byStatus := byMessage[0]
				byData1 := byMessage[1]
				byData2 := byMessage[2]
				// 取り扱うイベントは ノートオンとノートオフのみ
				if byStatus == NOTE_ON || byStatus == NOTE_OFF {
					// マッピング範囲内の時
					if KEY_MIN <= byData1 && byData1 <= KEY_MAX {
						// ノートオン
						if (byMessage[0] == NOTE_ON) && (byMessage[2] != 0x00) { 
							if byPreNote != byMessage[1] && byPreNote != 0 {
								// もし他のノートが押されていたら、放しておく
								sendKeyUp(byPreNote)
							}
							if byPreNote != byMessage[1] {
								// 今と異なる状態だったら、指定されたノートを押す
								sendKeyDown(byMessage[1])
								byPreNote = byMessage[1]
							}

						// ノートオフ
						} else if (byMessage[0] == NOTE_OFF) || (byMessage[0] == NOTE_ON && byMessage[2] == 0x00) { 
							// もし他のノートが押されていたら、放しておく
							if byPreNote != byMessage[1] && byPreNote != 0 {
								sendKeyUp(byPreNote)
							}
							// 指定されたノートを離す
							sendKeyUp(byMessage[1])
							byPreNote = 0
						}
					} else {
						// 範囲外の時
						if EXIT_OUTRANGE > 0 {
							# 範囲外の音が出たときに止める指定がされていたら、ループを抜ける
							if (int(byData1)-EXIT_OUTRANGE) < KEY_MIN || (int(byData1)+EXIT_OUTRANGE) > KEY_MAX {
								blnProcRunning = false
							}
						}
					}
				}
			} else {
				time.Sleep(1)
			}
		}
		// もし何か押された状態だったら、放しておく
		if byPreNote != 0 {
			sendKeyUp(byPreNote)
			byPreNote = 0
		}
	}
	wndWindow.SetTitle(appTitle + " -- STOP")
	blnProcRunning = false
}

// キーが押されたとき
func sendKeyDown(byNote uint8) {
	var strKey string = aryKeyMap[int(byNote)]
	if strKey != "" {
		// アサインされたキーを配列に格納
		aryKey := strings.Split(strKey, " ")
		if len(aryKey) > 0 {
			// 指定された順番にキーイベントを送信
			for n := 0; n < len(aryKey); n++ {
				SendMessage.Call(0, uintptr(WM_KEYDOWN), uintptr(KEY_CODE[aryKey[n]]), 0)
				sleep(1)	// イベント送信
			}
		}
	}
}

// キーが離されたとき
func sendKeyUp(byNote uint8) {
	var strKey string = aryKeyMap[int(byNote)]
	if strKey != "" {
		// アサインされたキーを配列に格納
		aryKey := strings.Split(strKey, " ")
		if len(aryKey) > 0 {
			// 指定された逆順にキーイベントを送信
			for n := len(aryKey) - 1; n >= 0; n-- {
				SendMessage.Call(0, uintptr(WM_KEYUP), uintptr(KEY_CODE[aryKey[n]]), 0)
				sleep(1)	// イベント送信
			}
		}
	}
}

// INIファイルを読み込む
func initIni() int {
	strIniFile := "BardPlay.ini"
	// iniファイル名の取得
	_, file, _, ok := runtime.Caller(0)
	if ok {
		strIniFile = getFileNameWithoutExt(file) + ".ini"
	}
	cfg, err := ini.Load(strIniFile)
	if err != nil {
		log.Printf("Fail to read file: %v", err)
		return 0
	}
	PORT_IN = cfg.Section("CONFIG").Key("port_in").MustInt(1) -1	// .iniを共通化するため、値-1にする
	EXIT_OUTRANGE = cfg.Section("CONFIG").Key("exit_outrange").MustInt(1)
	for n := 0; n < 127; n++ {
		s := cfg.Section("MAPPING").Key(strconv.Itoa(n)).String()
		if len(s) != 0 {
			// マッピングにセット
			aryKeyMap[n] = s
			// マッピング範囲の更新
			if KEY_MIN == 0 {
				KEY_MIN = n
			}
			KEY_MAX = n
		} else {
			aryKeyMap[n] = ""
		}
	}
	return KEY_MAX - KEY_MIN
}

// ポインタ処理(数値)
func IntPtr(n int) uintptr {
	return uintptr(n)
}

// ポインタ処理(文字列)
func StrPtr(s string) uintptr {
	return uintptr(unsafe.Pointer(syscall.StringToUTF16Ptr(s)))
}

// キーマッピングの初期化
// pythonの pyautoguiに合わせたいけど、とりあえず使うものだけ
// 全量の確認方法: print(pyautogui.KEYBOARD_KEYS)
func initKeyMap() {
	KEY_CODE["backspace"] = 0x08
	KEY_CODE["tab"] = 0x09
	KEY_CODE["enter"] = 0x0D
	KEY_CODE["shift"] = 0x10
	KEY_CODE["ctrl"] = 0x11
	KEY_CODE["alt"] = 0x12
	KEY_CODE["pause"] = 0x13
	KEY_CODE["capslock"] = 0x14
	KEY_CODE["esc"] = 0x1B
	KEY_CODE["space"] = 0x20
	KEY_CODE["pageup"] = 0x21
	KEY_CODE["pagedown"] = 0x22
	KEY_CODE["end"] = 0x23
	KEY_CODE["home"] = 0x24
	KEY_CODE["left"] = 0x25
	KEY_CODE["up"] = 0x26
	KEY_CODE["right"] = 0x27
	KEY_CODE["down"] = 0x28
	KEY_CODE["printscrn"] = 0x2C
	KEY_CODE["insert"] = 0x2D
	KEY_CODE["delete"] = 0x2E
	KEY_CODE["0"] = 0x30
	KEY_CODE["1"] = 0x31
	KEY_CODE["2"] = 0x32
	KEY_CODE["3"] = 0x33
	KEY_CODE["4"] = 0x34
	KEY_CODE["5"] = 0x35
	KEY_CODE["6"] = 0x36
	KEY_CODE["7"] = 0x37
	KEY_CODE["8"] = 0x38
	KEY_CODE["9"] = 0x39
	KEY_CODE["a"] = 0x41
	KEY_CODE["b"] = 0x42
	KEY_CODE["c"] = 0x43
	KEY_CODE["d"] = 0x44
	KEY_CODE["e"] = 0x45
	KEY_CODE["f"] = 0x46
	KEY_CODE["g"] = 0x47
	KEY_CODE["h"] = 0x48
	KEY_CODE["i"] = 0x49
	KEY_CODE["j"] = 0x4A
	KEY_CODE["k"] = 0x4B
	KEY_CODE["l"] = 0x4C
	KEY_CODE["m"] = 0x4D
	KEY_CODE["n"] = 0x4E
	KEY_CODE["o"] = 0x4F
	KEY_CODE["p"] = 0x50
	KEY_CODE["q"] = 0x51
	KEY_CODE["r"] = 0x52
	KEY_CODE["s"] = 0x53
	KEY_CODE["t"] = 0x54
	KEY_CODE["u"] = 0x55
	KEY_CODE["v"] = 0x56
	KEY_CODE["w"] = 0x57
	KEY_CODE["x"] = 0x58
	KEY_CODE["y"] = 0x59
	KEY_CODE["z"] = 0x5A
	KEY_CODE["f1"] = 0x70
	KEY_CODE["f2"] = 0x71
	KEY_CODE["f3"] = 0x72
	KEY_CODE["f4"] = 0x73
	KEY_CODE["f5"] = 0x74
	KEY_CODE["f6"] = 0x75
	KEY_CODE["f7"] = 0x76
	KEY_CODE["f8"] = 0x77
	KEY_CODE["f9"] = 0x78
	KEY_CODE["f10"] = 0x79
	KEY_CODE["f11"] = 0x7A
	KEY_CODE["f12"] = 0x7B

}
func getFileNameWithoutExt(path string) string {
	// Fixed with a nice method given by mattn-san
	return filepath.Base(path[:len(path)-len(filepath.Ext(path))])
}
