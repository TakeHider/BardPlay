unit formMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls, Vcl.Buttons,threadEventProc,unitMIDIIO;

type
  TBardPlay = class(TForm)
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
  public
    { Public 宣言 }
    FThreadRunning  : Boolean;


  end;

var
  BardPlay: TBardPlay;

implementation

{$R *.dfm}
ResourceString
ERR_NODEVICE = '*** MIDI Devicve Not Found ***';

{$IFDEF DEBUG}
{$APPTYPE CONSOLE}
{$ENDIF}


// フォーム作成時
procedure TBardPlay.FormCreate(Sender: TObject);
begin

{$IFDEF DEBUG}
WriteLn('デバグ情報');
{$ENDIF}

  // MIDIデバイス情報の更新
  getMIDIDeviceList();
end;


// 更新ボタンが押されたとき
procedure TBardPlay.btnRefreshClick(Sender: TObject);
begin
  // MIDIデバイス情報の更新
  getMIDIDeviceList();
end;

// 閉じるボタン
procedure TBardPlay.btnExitClick(Sender: TObject);
begin
  Close;
end;


// MIDIデバイス情報の取得
procedure TBardPlay.getMIDIDeviceList();
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
    cbDeviceList.style    := csDropDownList;
    cbDeviceList.Style    := csSimple;
    cbDeviceList.Enabled  := False;
    cbDeviceList.Text     := ERR_NODEVICE;
    btnStart.Enabled      := False;

  end;

end;

end.
