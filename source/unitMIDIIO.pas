unit unitMIDIIO;

// MIIDIO.DLLとDelphiの橋渡し関数
// C言語のDLLをそのまま利用するに手は諸々お手続きが必要なので、ここでDelphi用関数に翻訳しておく
interface
uses
  Winapi.Windows, System.SysUtils;
  
{----------------------------------------------------------------------------}
// Delphi側関数 Delphi側はこの関数で呼び出す
function procMIDIIn_GetDeviceNum(): Integer;
function procMIDIIn_GetDeviceName(iID: Integer; var strDeviceName: String; iLen: Integer): Integer;
function procMIDIIn_Open(strDeviceName: String): Integer;
function procMIDIIn_Close(iMIDIDevice: Integer): Integer;
function procMIDIIn_Reset(iMIDIDevice: Integer): Integer;
function procMIDIIn_GetMIDIMessage(iMIDIIn: Integer; var ucMessage: Array of byte; iLen: Integer): Integer;


implementation
Const procMIDIIn_DLL = 'MIDIIO.dll';


{----------------------------------------------------------------------------}
// MIDIIO.DLL 側関数
{----------------------------------------------------------------------------}
// MIDI出力デバイスの数を調べる
function MIDIIn_GetDeviceNum(): DWORD; stdcall; external procMIDIIn_DLL;

// MIDI入力デバイスの名前を調べる
function MIDIIn_GetDeviceNameW(lID: DWORD; pszDeviceName:PWChar; lLen: DWORD): DWORD; stdcall; external procMIDIIn_DLL;

// MIDI入力デバイスを開く
function MIDIIn_OpenW(const pszDeviceName: PWChar): DWORD; stdcall; external procMIDIIn_DLL;

// MIDI入力デバイスを閉じる
function MIDIIn_Close(pMIDIDevice: DWORD): DWORD;   stdcall; external procMIDIIn_DLL;

// MIDI入力デバイスをリセットする
function MIDIIn_Reset(pMIDIDevice: DWORD): DWORD;  stdcall; external procMIDIIn_DLL;

// MIDI入力デバイスから1メッセージ入力する
function MIDIIn_GetMIDIMessage(pMIDIIn: DWORD; pMessage: PByte; lLen: DWORD): DWORD; stdcall; external procMIDIIn_DLL;


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
function procMIDIIn_Open(strDeviceName: String):Integer;
var
  pszDeviceName: PWChar;
begin
  if length(strDeviceName)=0 then
  begin
    result := 0;
  end
  else
  begin
    // 文字列を格納するための領域を確保
    pszDeviceName := StrAlloc(length(strDeviceName));
    try
      // DLLの呼び出し
      result := MIDIIn_OpenW(pszDeviceName);
    finally
      // 最後に領域を解放
      StrDispose(pszDeviceName);
    end;
  end;
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスを閉じる
function procMIDIIn_Close(iMIDIDevice: Integer): Integer;
begin
  result := MIDIIn_Close(iMIDIDevice);
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスをリセットする
function procMIDIIn_Reset(iMIDIDevice: Integer): Integer;
begin
  result := MIDIIn_Reset(iMIDIDevice);
end;

{----------------------------------------------------------------------------}
// MIDI入力デバイスから1メッセージ入力する
function procMIDIIn_GetMIDIMessage(iMIDIIn: Integer; var ucMessage: Array of byte; iLen: Integer): Integer;
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
    begin
      ucMessage[n] := pMessage[n];
    end;
  finally
    FreeMem(pMessage);
  end;




end;

end.
