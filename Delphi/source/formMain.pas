{ Bard Play ( BardPlay Delphi ) }
{          Main Form            }
{                               }
{ (C) 2022 TakeHide Soft.       }
{         TakeHider@outlook.com }

unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons,threadMIDIEvent,unitMIDIIO,FormVersion,System.IniFiles,
  Vcl.ExtCtrls,System.StrUtils, Vcl.ComCtrls;

type
  TBardPlayDelphi = class(TForm)
    // ウインドウコントロールとイベント
    cbDeviceList: TComboBox;
    btnStart: TBitBtn;
    btnExit: TBitBtn;
    btnRefresh: TBitBtn;
    ilImageList: TImageList;
    lblMIDIDevice: TLabel;
    chkStartOnRun: TCheckBox;
    lblTransepose: TLabel;
    cbTransepose: TComboBox;
    chkVirtualChords: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private 宣言 }
    MIDIEventThread     : TMIDIEventThread;   // MIDIイベント用(スレッド処理)
    FDefaultDeviceName  : String;             // デフォルトのMIDIデバイス名
    procedure getMIDIDeviceList();            // MIDIデバイスの検索
    procedure WMCommand(var Msg: TWMSysCommand);message WM_SYSCOMMAND;
    procedure ReadIniSetting();
    procedure WriteIniSetting();
    procedure ThreadTerminate(Sender: TObject);
  public
    { Public 宣言 }
    FProcRunning : Boolean;   // スレッドは処理中かな
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
  // スレッドが動いているかのフラグ
  FProcRunning := False;
  // iniの読み込み
  ReadIniSetting();

  // システムメニューの追加
  hSysMenu := GetSystemMenu(Handle,False);

  // セパレータの下にバージョン情報を追加
  AppendMenu(hSysMenu, MF_SEPARATOR,0,nil);
  AppendMenu(hSysMenu, MF_STRING, MENUCMD_VERSIONINFO, PWChar(MNU_VERSIONINFO));

  // MIDIデバイス情報の更新
  getMIDIDeviceList();

  // 自動実行
  if chkStartOnRun.Checked and (btnStart.ImageIndex = 0) and (cbDeviceList.ItemIndex <>-1) then
    btnStartClick(self);

end;

{----------------------------------------------------------------------------}
// フォームクローズ時
procedure TBardPlayDelphi.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // もしスレッドが実行してたら、中止命令を出す
 if FProcRunning then
  begin
    // マルチスレッドの停止命令を出す
    MIDIEventThread.Terminate;
    // 処理を渡して一呼吸置く
    Application.ProcessMessages;
  end;
  // レジストリに書き込む
  if FDefaultDeviceName<>'' then
    WriteIniSetting();

{$IFDEF DEBUG}
writeln(FProcRunning);
{$ENDIF}

  Action := caFree;

end;


{----------------------------------------------------------------------------}
// フォームを開いた時
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
    btnRefresh.Enabled  := False;       // 再検索のボタンを抑止
    cbTransepose.Enabled:= False;       // トランスポーズを抑止
    // プロセスの実行
    if not FProcRunning then
    begin
      FProcRunning    := True;
      MIDIEventThread := TMIDIEventThread.Create(TRUE);           // 一時停止状態でスレッドを作成るる
      MIDIEventThread.FDeviceNumber := cbDeviceList.ItemIndex;    // デバイス番号を渡す
      MIDIEventThread.FDeviceName   := FDefaultDeviceName;        // デバイス番号
      MIDIEventThread.FTransepose   := cbTransepose.ItemIndex -3; // トランスポーズ
      MIDIEventThread.FVirtualChords:= chkVirtualChords.checked;  // 疑似コード
      MIDIEventThread.OnTerminate   := ThreadTerminate;           // スレッドが終了した時の処理
      MIDIEventThread.Start;                                      // スレッドの実行
   end;
  end
  else
  begin
    // Stopボタンが押されたときの動作
    btnStart.ImageIndex := 0;           // ボタンのアイコンをStartにする
    btnStart.Caption    := 'Start';     // ボタンのキャプションを変更
    cbDeviceList.Enabled:= True;        // コンボボックスの選択を有効にする
    btnRefresh.Enabled  := True;       // 再検索のボタンを有効にする
    cbTransepose.Enabled:= True;       // トランスポーズを有効

    // プロセスの停止
    if FProcRunning  then
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
 if FProcRunning then
  begin
    // マルチスレッドの停止命令を出す
    MIDIEventThread.Terminate;
    // 処理を渡して一呼吸置く
    Application.ProcessMessages;
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
            // 背景色♪
            BardPlayVersionInfo.Color := BardPlayDelphi.Color;
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
// スレッドのTerminateイベント
procedure TBardPlayDelphi.ThreadTerminate(Sender: TObject);
begin
  FProcRunning        := False;       // プロセスは止まりましたー
  // Thread側で停止した時は、ボタンをSTOPにしておく
  if btnStart.ImageIndex <>0 then
  begin
    // Stopボタンが押されたときの動作
   btnStart.ImageIndex   := 0;           // ボタンのアイコンをStartにする
    btnStart.Caption      := 'Start';     // ボタンのキャプションを変更
    cbDeviceList.Enabled  := True;        // コンボボックスの選択を有効にする
    btnRefresh.Enabled    := True;        // 再検索のボタンを有効にする
    cbTransepose.Enabled  := True;        // トランスポーズを有効
  End;
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
    // 直近で指定されたものあなければ、INIから取得
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
// INIファイルから読み込む
procedure TBardPlayDelphi.ReadIniSetting();
var
  bStatOnRun    : boolean;
  bVirtualChords: boolean;
  iniFile       : TiniFile;
  strBGColor    : String;
begin
  // INIファイルから情報を読み込む
  iniFile := TiniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    FDefaultDeviceName    := iniFile.ReadString('CONFIG','device_name','');         // デバイス名
    bStatOnRun            := iniFile.ReadInteger('CONFIG','start_on_run',0) = 1;    // 起動時に開始
    bVirtualChords        := iniFile.ReadInteger('CONFIG','virtual_chords',1) = 1;  // 疑似和音
    strBGColor            := iniFile.ReadString('CONFIG','color','#F5FFFA');        // 背景色
    cbTransepose.ItemIndex:= iniFile.ReadInteger('CONFIG','transpose',0)+3 ;        // トランスポーズ
  finally
    iniFile.Free;
  end;
  // アプリの背景色をセットする
  if Length(strBGColor)=7 then
  begin
    if LeftStr(strBGColor,1)='#' then
    begin
      BardPlayDelphi.Color := RGB(StrToInt('$' + MidStr(strBGColor,2,2)),
                                  StrToInt('$' + MidStr(strBGColor,4,2)),
                                  StrToInt('$' + MidStr(strBGColor,6,2)));

    end;

  end;

  chkStartOnRun.Checked     := bStatOnRun;      // Start On Run
  chkVirtualChords.checked  := bVirtualChords;  // Virtual Chords
end;


{----------------------------------------------------------------------------}
// INIファイルに書き込む
procedure TBardPlayDelphi.WriteIniSetting();
var
  iStatOnRun        : Integer;
  iVirtualChords    : Integer;
  iniFile           : TiniFile;
begin
  // 画面の状態を取得(Delphiには3項演算子が無いのだ)
  if chkStartOnRun.Checked then       // Start On Run
    iStatOnRun := 1
  else
    iStatOnRun := 0;
  if chkVirtualChords.Checked then    // Virtual Chords
    iVirtualChords := 1
  else
    iVirtualChords := 0;


  // INIファイルに情報を書き込む
  iniFile := TiniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    iniFile.WriteString('CONFIG','device_name',FDefaultDeviceName);
    iniFile.WriteInteger('CONFIG','start_on_run',iStatOnRun);
    iniFIle.WriteInteger('CONFIG','transpose', cbTransepose.ItemIndex -3);
    iniFile.WriteInteger('CONFIG','virtual_chords',iVirtualChords);
  finally
    iniFile.Free;
  end;
end;

end.
