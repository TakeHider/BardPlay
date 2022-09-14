unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons,threadMIDIEvent,unitMIDIIO,FormVersion,System.IniFiles,
  Vcl.ExtCtrls;

type
  TBardPlayDelphi = class(TForm)
    cbDeviceList: TComboBox;
    btnStart: TBitBtn;
    btnExit: TBitBtn;
    btnRefresh: TBitBtn;
    ImageList: TImageList;
    speTop: TShape;
    Label1: TLabel;
    speBottom: TShape;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure speBottomContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Private 宣言 }
    MIDIEventThread : TMIDIEventThread;
    FDefaultDeviceName : String;
    procedure getMIDIDeviceList();
    procedure WMCommand(var Msg: TWMSysCommand);message WM_SYSCOMMAND;
    procedure MIDIEventThreadTerminate(Sender: TObject);
    function ReadDefaultDeviceName() : String;
    procedure WriteDefaultDeviceName(strDeviceName : String);


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
MNU_VERSIONINFO = 'Version Info.';


{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}

{----------------------------------------------------------------------------}
// フォーム作成時
procedure TBardPlayDelphi.FormCreate(Sender: TObject);
var
  hSysMenu: Integer;

begin

  MIDIEventThread := nil;
  // iniの読み込み
  FDefaultDeviceName := ReadDefaultDeviceName();

  // システムメニューの追加
  hSysMenu := GetSystemMenu(Handle,False);

  // セパレータの下にバージョン情報を追加
  AppendMenu(hSysMenu, MF_SEPARATOR,0,nil);
  AppendMenu(hSysMenu, MF_STRING, MENUCMD_VERSIONINFO, PWChar(MNU_VERSIONINFO));

  // MIDIデバイス情報の更新
  getMIDIDeviceList();


end;

procedure TBardPlayDelphi.FormShow(Sender: TObject);
begin
{$IFDEF DEBUG}
AllocConsole;
{$ENDIF}
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
    FDefaultDeviceName  := cbDeviceList.Items[cbDeviceList.ItemIndex];  // デバイス名を保持
    btnStart.ImageIndex := 1;           // ボタンのアイコンをStopにする
    btnStart.Caption    := 'Stop';      // ボタンのキャプションを変更
    cbDeviceList.Enabled:= False;       // コンボボックスの選択を抑止
    // プロセスの実行
    if not Assigned(MIDIEventThread) then
    begin
      MIDIEventThread := TMIDIEventThread.Create(TRUE);         // 一時停止状態でスレッドを作成
//    MIDIEventThread.FreeOnTerminate := True;                  // スレッドが終わったらメモリを開放する
      MIDIEventThread.OnTerminate := MIDIEventThreadTerminate;  // スレッド終了時のイベントを紐づける
      MIDIEventThread.FDeviceNumber := cbDeviceList.ItemIndex;  // デバイス番号を渡す
      MIDIEventThread.FDeviceName   := FDefaultDeviceName;      // デバイス番号
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
    if Assigned(MIDIEventThread) then
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
 if Assigned(MIDIEventThread) then
  begin
    MIDIEventThread.Terminate;
    Application.ProcessMessages;
  end;
  if FDefaultDeviceName<>'' then
    WriteDefaultDeviceName(FDefaultDeviceName);

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
  MIDIEventThread := nil;
end;

{----------------------------------------------------------------------------}
// MIDIデバイス情報の取得
procedure TBardPlayDelphi.getMIDIDeviceList();
var
  n               : Integer;
  iDeviceCount    : integer;
  strDeviceName   : String;
  iRetLength      : Integer;
  strPreDeviceName: String;

begin
  // 現在のリストの状態を保持
  strPreDeviceName := '';
  if cbDeviceList.style = csDropDownList then
  begin
    if (cbDeviceList.Items.Count > 0) and (cbDeviceList.ItemIndex >=0) then
    begin
      strPreDeviceName := cbDeviceList.Items[cbDeviceList.ItemIndex ];
    end;
  end;

  // MIDIのデバイス数を数える
  iDeviceCount := procMIDIIn_GetDeviceNum();

  // MIDIデバイスが見つかったら、コンボボックスに登録する
  if iDeviceCount >0 then
  begin
    // まずはコンボボックスの中身をクリア
    cbDeviceList.Items.Clear;
    cbDeviceList.ItemIndex := -1;
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
    btnStart.Enabled      := True;            // 実行ボタンも有効にする

    // 直近で選択されたものがあれば、デフォルトにする
    if (strPreDeviceName<>'') and (cbDeviceList.ItemIndex =-1)  then
    begin
      if cbDeviceList.Items.IndexOf(strPreDeviceName)>=0 then
      begin
        cbDeviceList.ItemIndex := cbDeviceList.Items.IndexOf(strPreDeviceName)
      end;
    end;
    if (FDefaultDeviceName <> '') and (cbDeviceList.ItemIndex =-1) then
    begin
      begin
        cbDeviceList.ItemIndex := cbDeviceList.Items.IndexOf(FDefaultDeviceName)
      end;
    end;

  end
  else
  begin
    // MIDIデバイスが見つからなかったときは、アプリとして無効にする
    cbDeviceList.Style    := csSimple;
    cbDeviceList.Text     := NSG_DEVUCE_NOTFOUND;
    btnStart.Enabled      := False;

  end;

end;

{----------------------------------------------------------------------------}
// INIファイルからデフォルトのデバイス名を読み込む
function TBardPlayDelphi.ReadDefaultDeviceName() : String;
var
  iniFile : TiniFile;
begin
  iniFile := TiniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    result := iniFile.ReadString('CONFIG','device_name','');
  finally
    iniFile.Free;
  end;
end;

procedure TBardPlayDelphi.speBottomContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

{----------------------------------------------------------------------------}
// INIファイルにデフォルトのデバイス名を書き込む
procedure TBardPlayDelphi.WriteDefaultDeviceName(strDeviceName : String);
var
  iniFile : TiniFile;
begin
  iniFile := TiniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    iniFile.WriteString('CONFIG','device_name',strDeviceName);
  finally
    iniFile.Free;
  end;
end;

end.
