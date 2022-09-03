# BardPlayGo

Single Note MIDI Event to PC-Keyboard Event  
FF14 compatible  

**日本語の説明は下の方にあります**

## GOlang Libary

* fyne.io  
* path/filepath  
* gopkg.in/ini.v1
  
  ```cmd
  > CD <ProjectFolder>
  > go get fyne.io/fyne
  > go get gopkg.in/ini.v1
  ```

## External Libary

* MIDIIO.dll  
[https://openmidiproject.osdn.jp/MIDIIOLibrary.html](https://openmidiproject.osdn.jp/MIDIIOLibrary.html)  
[Downloading File /77139/MIDIIOLib7.1.zip - OpenMIDIProject - OSDN](https://osdn.net/projects/openmidiproject/downloads/77139/MIDIIOLib7.1.zip/)  

## Run

```cmd
> go run BardPlay.go
```

## ini File

### [CONFIG]

* port_in (default=1)  
MIDI-Port number to use.  
If you have multiple MIDI devices, try different numbers.  

* exit_outrange (default=1)  
  If you make a sound outside the range, the process will stop.  
  Enter a number greater than or equal to 1 to enable.  
  The numbers indicate how far away from the boundary values.  
* start_on_run (default=1)  
  Processing begins when the application is run.  

### [MAPPING]

A MIDI note that corresponds to a key.  
If you want to press the SHIFT key, CTRL key, etc. at the same time, put a space between them in the order you press them.  
Notes not specified here will be the range of exit_outrange.  

### other

* If change exe-file name , need change ini-file name   
  ex: BardPlay.exe -> bardplay.ini  ,  BardPlay2.exe -> bardplay2.ini  
* BardPlay.py compatible  

## Packaging

```cmd
> go build -ldflags "-H=windowsgui -s -w" BardPlay.go 
```

or

```cmd
> go install fyne.io/fyne/cmd/fyne
> fyne package -os windows -icon icon.png
```

---

日本語で解説しよう

## 『吟遊詩人の演奏 GO言語版』  
単音のMIDI入力情報を、パソコンのキーボードイベントに変えます。  
FF14対応  

## GO言語で必要なライブラリ  

標準のGO言語開発環境に加え、下記のライブラリが別途必要です。  

* fyne.io  
* path/filepath  
* gopkg.in/ini.v1  

インストール方法  

```cmd
> CD プロジェクトフォルダ
> go get fyne.io/fyne
> go get gopkg.in/ini.v1
```

## その他必要なライブラリ

* MIDIIO.dll  
  [https://openmidiproject.osdn.jp/MIDIIOLibrary.html](https://openmidiproject.osdn.jp/MIDIIOLibrary.html)  
  [Downloading File /77139/MIDIIOLib7.1.zip - OpenMIDIProject - OSDN](https://osdn.net/projects/openmidiproject/downloads/77139/MIDIIOLib7.1.zip/)  

## 実行方法

```cmd
> go run BardPlay.go
```

## iniファイル

### [CONFIG]

* port_in (default=1)  
  使用するMIDIのポート番号。  
  もし複数のMIDIデバイスを持っていたら、番号を変更してみてください。  

* exit_outrange (default=1)  
  キーマッピングされた範囲外の音が出されたら、処理を停止します。  
  0を指定するとこのオプションは機能しません。  
  1以上の数字を指定すると、指定された数だけ範囲から外れた音が出されたときに、処理を停止します。  
  1だと、範囲のすぐ外側の音から有効になります。  
  鍵盤楽器など、誤って押してしまいそうなときは、少し範囲を広げてください。  

* start_on_run (default=1)
  アプリケーション実行時に処理を開始します。  

### [MAPPING]

MIDIのノートをキーに対応させたもの。  
SHIFTキーや、CTRLキーなど、同時に押したいときは、押す順番にスペースで挟んでください。    

### その他

* 実行ファイル名を変更した時は、iniファイルの名前も変えてください。  
  例）  
　BardPlay.exe の時は bardplay.ini   
　BardPlay2.exe の時は bardplay2.ini  
* iniファイルは、Python版 BardPlay (BardPlay.py)と互換性があります。  

## EXEファイルの作り方

普通に作る際はこんな感じ。  

```cmd
> go build -ldflags "-H=windowsgui -s -w" BardPlay.go 
```

fyneのツールを入れることで、アイコン付実行ファイルを作ることもできます。  
(ただし、ファイルサイズはかなり大きくなります。)    

```cmd
> go install fyne.io/fyne/cmd/fyne
> fyne package -os windows -icon icon.png
```

今回のアイコンは`StabilityAI`で作りました。  
アイコンの呪文は「A bard girl with a violin in Japanese girl anime style white background」です。  
