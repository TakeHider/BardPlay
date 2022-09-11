unit threadMIDIEvent;

interface

uses
  Windows,Classes,ShellAPI,Vcl.Forms,Vcl.StdCtrls;


type
  TMIDIEventThread = class(TThread)
  private
    procedure Sync_SetDeviceError;
  public
    FDeviceNumber: Integer;
  protected
    procedure Execute; override;
  end;

implementation

uses 
  formMain;

ResourceString
MSG_DEVICE_ERROR = '*** MIDI Devicve Error ***';

{----------------------------------------------------------------------------}
// スレッドの実行
procedure TMIDIEventThread.Execute;

begin
  repeat

    Application.ProcessMessages;
    sleep(1);
  until Terminated;

end;

{----------------------------------------------------------------------------}
// MIDIデバイスがうまく繋げられなかった時の処理
procedure TMIDIEventThread.Sync_SetDeviceError;
begin
  // 親ウインドウのプロシージャを呼び出す
  with BardPlayDelphi do
  begin
    // MIDIデバイスが見つからなかったときは、アプリとして無効にする
    cbDeviceList.Style    := csSimple;
    cbDeviceList.Enabled  := False;
    cbDeviceList.Text     := MSG_DEVICE_ERROR;
    btnStart.Enabled      := False;
  end;

end;

end.
