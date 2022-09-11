unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons,threadEventProc,unitMIDIIO,FormVersion;

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
  private
    { Private 宣言 }
    procedure getMIDIDeviceList();
    procedure WMCommand(var Msg: TWMSysCommand);message WM_SYSCOMMAND;
  public
    { Public 宣言 }
    FThreadRunning  : Boolean;


  end;


 const
  MENUCMD_VERSIONINFO  = 10;  // バージョン情報

var
  BardPlayDelphi: TBardPlayDelphi;

implementation




{$R *.dfm}
ResourceString
ERR_NODEVICE = '*** MIDI Devicve Not Found ***';
MNU_VERSIONINFO = 'Version Info';

{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}


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
end;


// 更新ボタンが押されたとき
procedure TBardPlayDelphi.btnRefreshClick(Sender: TObject);
begin
  // MIDIデバイス情報の更新
  getMIDIDeviceList();
end;

// 閉じるボタン
procedure TBardPlayDelphi.btnExitClick(Sender: TObject);
begin
  Close;
end;

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
    cbDeviceList.Enabled  := False;
    cbDeviceList.Text     := ERR_NODEVICE;
    btnStart.Enabled      := False;

  end;

end;

end.
