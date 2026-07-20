{ Bard Play ( BardPlay Delphi ) }
{     MIDIIO.dll Interface      }
{                               }
{ (C) 2022 TakeHide Soft.       }
{         TakeHider@outlook.com }

unit unitMIDIIO;

// MIIDIO.DLLとDelphiの橋渡し関数
// C言語のDLLをそのまま利用するに手は諸々お手続きが必要なので、ここでDelphi用関数に翻訳しておく
interface
uses
  Winapi.Windows, System.SysUtils;

type
  TMIDIHandle = Pointer;

{----------------------------------------------------------------------------}
// Delphi側関数 Delphi側はこの関数で呼び出す
function procMIDIIn_GetDeviceNum(): Integer;
function procMIDIIn_GetDeviceName(iID: Integer; var strDeviceName: String; iLen: Integer): Integer;
function procMIDIIn_Open(const strDeviceName: String): TMIDIHandle;
function procMIDIIn_Close(iMIDIDevice: TMIDIHandle): Integer;
function procMIDIIn_Reset(iMIDIDevice: TMIDIHandle): Integer;
function procMIDIIn_GetMIDIMessage(iMIDIIn: TMIDIHandle; var ucMessage: Array of byte; iLen: Integer): Integer;


implementation
Const MIDIIO_DLL = 'MIDIIO.dll';


{----------------------------------------------------------------------------}
// MIDIIO.DLL 側関数
{----------------------------------------------------------------------------}
// MIDI出力デバイスの数を調べる
function MIDIIn_GetDeviceNum(): DWORD; stdcall; external MIDIIO_DLL;

// MIDI入力デバイスの名前を調べる
function MIDIIn_GetDeviceNameW(lID: DWORD; pszDeviceName:PWideChar; lLen: DWORD): DWORD; stdcall; external MIDIIO_DLL;

// MIDI入力デバイスを開く
function MIDIIn_OpenW(const pszDeviceName: PWideChar): TMIDIHandle; stdcall; external MIDIIO_DLL;

// MIDI入力デバイスを閉じる
function MIDIIn_Close(pMIDIDevice: TMIDIHandle): DWORD;   stdcall; external MIDIIO_DLL;

// MIDI入力デバイスをリセットする
function MIDIIn_Reset(pMIDIDevice: TMIDIHandle): DWORD;  stdcall; external MIDIIO_DLL;

// MIDI入力デバイスから1メッセージ入力する
function MIDIIn_GetMIDIMessage(pMIDIIn: TMIDIHandle; pMessage: PByte; lLen: DWORD): DWORD; stdcall; external MIDIIO_DLL;


{----------------------------------------------------------------------------}
// MIDI出力デバイスの数を調べる
function procMIDIIn_GetDeviceNum(): integer;
begin
  // 関数をそのまま返す
  result := MIDIIn_GetDeviceNum();
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスの名前を調べる
function procMIDIIn_GetDeviceName(iID: Integer; var strDeviceName: String; iLen: Integer): Integer;
var
  pszDeviceName: PWChar;
begin
  // 文字列を格納するための領域を確保
  pszDeviceName := StrAlloc(iLen);
  try
    // DLLの呼び出し
    result := MIDIIn_GetDeviceNameW(iID,pszDeviceName,iLen);
    // 取得した文字列を、受け取った文字変数に格納
    strDeviceName := pszDeviceName;
  finally
    // 最後に領域を解放
    StrDispose(pszDeviceName);
  end;
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスを開く
function procMIDIIn_Open( const strDeviceName: string): TMIDIHandle;
begin
  if strDeviceName = '' then
    Result := nil
  else
    Result := MIDIIn_OpenW(PWideChar(strDeviceName));
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスを閉じる
function procMIDIIn_Close(iMIDIDevice: TMIDIHandle): Integer;
begin
  result := MIDIIn_Close(iMIDIDevice);
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスをリセットする
function procMIDIIn_Reset(iMIDIDevice: TMIDIHandle): Integer;
begin
  result := MIDIIn_Reset(iMIDIDevice);
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスから1メッセージ入力する
function procMIDIIn_GetMIDIMessage(iMIDIIn: TMIDIHandle; var ucMessage: Array of byte; iLen: Integer): Integer;
var
  pMessage: PByte;
  n: Integer;
begin
  // byte型のメモリ領域を確保
  pMessage := AllocMem(iLen * SizeOf(byte));
  try
    // 関数の呼び出し  戻り値に受け取ったメッセージの数が入る
    result := MIDIIn_GetMIDIMessage(iMIDIIn, pMessage,iLen);
    // 引数で受け取ったByte型の配列に格納
    for n:=0  to iLen-1 do
      ucMessage[n] := pMessage[n];
  finally
    FreeMem(pMessage);
  end;




end;

end.
