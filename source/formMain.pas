unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons,threadMIDIEvent,unitMIDIIO,FormVersion;

type
  TBardPlayDelphi = class(TForm)
    cbDeviceList: TComboBox;
    btnStart: TBitBtn;
    btnExit: TBitBtn;
    btnRefresh: TBitBtn;
    ilImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
  private
    { Private 宣言 }
    MIDIEventThread : TMIDIEventThread;
    procedure getMIDIDeviceList();
    procedure WMCommand(var Msg: TWMSysCommand);message WM_SYSCOMMAND;
    procedure MIDIEventThreadTerminate(Sender: TObject);
  public
    { Public 宣言 }
  end;

 const
  MENUCMD_VERSIONINFO  = 10;  // バージョン情報

var
  BardPlayDelphi: TBardPlayDelphi;

implementation

{$R *.dfm}
ResourceString
NSG_DEVUCE_NOTFOUND = '*** MIDI Devicve Not Found ***';
MNU_VERSIONINFO = 'About..';

{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}

{----------------------------------------------------------------------------}
// フォーム作成時
procedure TBardPlayDelphi.FormCreate(Sender: TObject);
var
  hSysMenu: Integer;

begin

{$IFDEF DEBUG}
WriteLn('デバグ情報');
{$ENDIF}

  // システムメニューの追加
  hSysMenu := GetSystemMenu(Handle,False);

  // セパレータの下にバージョン情報を追加
  AppendMenu(hSysMenu, MF_SEPARATOR,0,nil);
  AppendMenu(hSysMenu, MF_STRING, MENUCMD_VERSIONINFO, PWChar(MNU_VERSIONINFO));

  // MIDIデバイス情報の更新
  getMIDIDeviceList();

  MIDIEventThread := nil;

end;

{----------------------------------------------------------------------------}
// 更新ボタンが押されたとき
procedure TBardPlayDelphi.btnRefreshClick(Sender: TObject);
begin
  // MIDIデバイス情報の更新
  getMIDIDeviceList();
end;

{----------------------------------------------------------------------------}
// 開始/終了ボタン
procedure TBardPlayDelphi.btnStartClick(Sender: TObject);
begin
  // StartとStopをスイッチする
  if btnStart.ImageIndex = 0 then
  begin
    // Startボタンが押されたときの動作
    btnStart.ImageIndex := 1;           // ボタンのアイコンをStopにする
    btnStart.Caption    := 'Stop';      // ボタンのキャプションを変更
    cbDeviceList.Enabled:= False;       // コンボボックスの選択を抑止
    // プロセスの実行
    if MIDIEventThread = nil then
    begin
      MIDIEventThread := TMIDIEventThread.Create(True);         // 一時停止状態でスレッドを作成
      MIDIEventThread.FreeOnTerminate := True;                  // スレッドが終わったらメモリを開放する
      MIDIEventThread.OnTerminate := MIDIEventThreadTerminate;  // スレッド終了時のイベントを紐づける
      MIDIEventThread.Start;                                    // スレッドの実行
   end;
  end
  else
  begin
    // Stopボタンが押されたときの動作
    btnStart.ImageIndex := 0;           // ボタンのアイコンをStartにする
    btnStart.Caption    := 'Start';     // ボタンのキャプションを変更
    cbDeviceList.Enabled:= True;        // コンボボックスの選択を有効にする
    // プロセスの停止
    if MIDIEventThread <> nil then
    begin
      // 停止命令を出す
      MIDIEventThread.Terminate;
    end;

  end;

end;

{----------------------------------------------------------------------------}
// 閉じるボタン
procedure TBardPlayDelphi.btnExitClick(Sender: TObject);
begin
  // もしスレッドが実行してたら、中止命令を出す
 if MIDIEventThread <> nil then
  begin
    MIDIEventThread.Terminate;
    Application.ProcessMessages;
    sleep(100);
  end;
  Close;
end;

{----------------------------------------------------------------------------}
// システムメニューのイベント
procedure TBardPlayDelphi.WMCommand(var Msg: TWMSysCommand);
var
  BardPlayVersionInfo:   TBardPlayVersionInfo;
begin
    case Msg.CmdType of
        MENUCMD_VERSIONINFO:
        begin
          // バージョン情報画面を動的に作る
          Application.CreateForm(TBardPlayVersionInfo, BardPlayVersionInfo);
          try
            // バージョン情報の表示
            BardPlayVersionInfo.ShowModal;
          finally
            // 動的にフォームを作ったので、最後は閉じておく
            BardPlayVersionInfo.Free;
          end;
        end;
    end;
    inherited;
end;

{----------------------------------------------------------------------------}
// MIDIイベントスレッドが止まった時の処理
procedure TBardPlayDelphi.MIDIEventThreadTerminate(Sender: TObject);
begin
  TMIDIEventThread(Sender)  := nil; // なぜかこっちは効かない
  MIDIEventThread           := nil; // しっくりこないけど、こちらで対応
end;

{----------------------------------------------------------------------------}
// MIDIデバイス情報の取得
procedure TBardPlayDelphi.getMIDIDeviceList();
var
  n             : Integer;
  iDeviceCount  : integer;
  strDeviceName : String;
  iRetLength    : Integer;

begin
  // MIDIのデバイス数を数える
  iDeviceCount := procMIDIIn_GetDeviceNum();

  // MIDIデバイスが見つかったら、コンボボックスに登録する
  if iDeviceCount >0 then
  begin
    // まずはコンボボックスの中身をクリア
    cbDeviceList.Items.Clear;
    // デバイスの数だけ登録
    for n := 1 to iDeviceCount do
    begin
      // デバイス番号ごとに名前を取得
      iRetLength := procMIDIIn_GetDeviceName(n-1,strDeviceName,32);
      if iRetLength <> 0 then
      begin
        // コンボボックスに登録
        cbDeviceList.Items.Add(strDeviceName);
      end;
    end;
  end;

  if cbDeviceList.Items.Count > 0 then
  begin
    // もしコンボボックスに何か登録されていたら、アプリとして有効にする
    cbDeviceList.style    := csDropDownList;  // ドロップダウンリストにする
    cbDeviceList.Enabled  := True;            // コントロールを有効にする
    cbDeviceList.ItemIndex:= 0;
    btnStart.Enabled      := True;            // 実行ボタンも有効にする
  end
  else
  begin
    // MIDIデバイスが見つからなかったときは、アプリとして無効にする
    cbDeviceList.Style    := csSimple;
//    cbDeviceList.Enabled  := False;
    cbDeviceList.Text     := NSG_DEVUCE_NOTFOUND;
//    btnStart.Enabled      := False;

  end;

end;

end.
