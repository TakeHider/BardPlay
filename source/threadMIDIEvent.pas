unit threadMIDIEvent;

interface

uses
  Windows,Classes,ShellAPI,Vcl.Forms,Vcl.StdCtrls,Generics.Collections,
  System.IniFiles,System.SysUtils,intList;


type
  // INIファイルの中身を格納する構造体
  TIniInfo = record
    iDeviceNumber : Integer;  // デバイス番号
    strDeviceName : String;   // デバイス名
    bPlayOnStart  : Boolean;  // 起動時に処理を開始するか
    iExitOutRange : Integer;  // 範囲から外れたらSTOPするか(および、境界値との距離)
    iMinRange     : Integer;  // 範囲下限
    iMaxRange     : Integer;  // 範囲上限
    astrKeyMap    : array[0..127] of String;  // マッピング情報
  end;


  // マッピングを保持するハッシュ(連想配列)
  // http://bougyuusonnin.seesaa.net/article/148148627.html
  THash<TKey,TValue> = class(TDictionary<TKey,TValue>)
  private
    function  GetItem(const Key: TKey): TValue;
    procedure SetItem(const Key: TKey; const Value: TValue);
  public
    property Items[const Key: TKey]: TValue read GetItem write SetItem; default;
  end;

  // スレッド本体
  TMIDIEventThread = class(TThread)
  private
    FKeyCode      : THash<string, byte>;
    FOption       : TIniInfo;
    FSendNoteList : TIntegerList;
    procedure Sync_SetDeviceError;
    procedure setKeyCode();
    procedure ReadIniFile();
    procedure sendKeyMessage(wmEvent: Integer; ucNote: byte);
  public
    FDeviceNumber: Integer;
    FDeviceName  : String;
    constructor Create(CreateSuspended : Boolean);
  protected
    procedure Execute; override;
  end;



implementation

uses 
  formMain,unitMIDIIO,System.Types,System.StrUtils;

const
  // 共通定数
  NOTE_ON    = $90;                 // ノートOnの信号
  NOTE_OFF   = $80;                 // ノートOffの信号
  WM_KEYUP   = $101;                // メッセージイベント KeyUp
  WM_KEYDOWN = $100;                // メッセージイベント KeyDown

ResourceString
MSG_DEVICE_ERROR = '*** MIDI Devicve Error ***';
INISECTION_CONFIG = 'CONFIG';
INISECTION_MAPPING= 'MAPPING';

{----------------------------------------------------------------------------}
{ THash<TKey, TValue> }
function THash<TKey, TValue>.GetItem(const Key: TKey): TValue;
begin
  TryGetValue(Key, Result);
end;

procedure THash<TKey, TValue>.SetItem(const Key: TKey;
  const Value: TValue);
begin
  AddOrSetValue(Key, Value);
end;


{----------------------------------------------------------------------------}
// スレッドのコンストラクタ
constructor TMIDIEventThread.Create(CreateSuspended : Boolean);
begin
  //Suspend状態で開始する
  inherited Create(CreateSuspended);
  //処理終了時にオブジェクトを自動破棄する
  FreeOnTerminate:=True;
  // キーマッピングオブジェクトの作成
  FKeyCode := THash<string, byte>.Create;
  // キーマッピングのセット (ここに書くとうざいので、別のところでやってもらう)
  setKeyCode();
  // INIの読み込み
  ReadIniFile();
end;
{----------------------------------------------------------------------------}
// スレッドの実行
procedure TMIDIEventThread.Execute;
var
  pMIDIIn     : Integer;
  iRet        : Integer;
  aucMessage  : Array[0..255] of byte;
  ucStatus    : byte;
  ucData1     : byte;
  ucData2     : byte;
  ucPreNote   : byte;
  n           : Integer;

begin


  // MIDIデバイスを開く
  pMIDIIn := procMIDIIn_Open(FDeviceName);
  if pMIDIin = 0 then
  begin
    // MIDIデバイスのオープンに失敗した時
    Synchronize(Sync_SetDeviceError);
  end
  else
  begin
    FSendNoteList := TIntegerList.Create;
    try
      ucPreNote := $00;
      repeat
        // MIDIメッセージの受信
        iRet := procMIDIIn_GetMIDIMessage(pMIDIIn, aucMessage, SizeOf(aucMessage));
        // ノートOnとノートOffは3バイトずつ信号が来る
        if iRet >= 3 then
        begin
          ucStatus  := aucMessage[0]; // 最初のバイトはステータス
          ucData1   := aucMessage[1]; // 次のバイトは音階
          ucData2   := aucMessage[2]; // 最後のバイトは強さ
          // 取り扱うイベントは ノートOnとノートOffのみ
          if (ucStatus = NOTE_ON) or (ucStatus = NOTE_OFF) then
          begin
            // 音階がマッピング範囲内の時
            if (FOption.iMinRange <= ucData1) and (ucData1 <= FOption.iMaxRange) then
            begin
              // ノートONのとき
              if (ucStatus = NOTE_ON) and (ucData2 <> $00) then
              begin;
                // もし他のノートが押されていたら、放しておく
                if (ucPreNote <> ucData1) and ( ucPreNote <> $00) then
                begin
                 sendKeyMessage(WM_KEYUP,ucPreNote);
                end;
                // 今と異なるノートだったら、指定されたノートを押す
                if ucPreNote <> ucData1 then
                begin
                  sendKeyMessage(WM_KEYDOWN, ucData1);
                  ucPreNote := ucData1;
                end;
              end
              //ノートオフの時
              else if (ucStatus = NOTE_OFF) or  ((ucStatus = NOTE_ON) and (ucData2 = $00)) then
              begin

                // 今押されているノートと同じだった時に放す
                if ucPreNote = ucData1 then
                begin
                  // 指定されたノートを離す
                  sendKeyMessage(WM_KEYUP, ucData1);
                  ucPreNote := $00;
                end;

              end;
            end
            // 範囲外の時
            else
            begin
              if FOption.iExitOutRange > 0 then
              begin
                // 「範囲外の音が出たときは止める」指定がされていたら、ループを抜ける
                if ((ucData1-FOption.iExitOutRange) < FOption.iMinRange)  or
                   ((ucData1+FOption.iExitOutRange) > FOPtion.iMaxRange) then
                begin
                  Terminate;
                  Application.ProcessMessages;
                  sleep(1);
                end;
              end;
            end;
          end;
        end
        else
        begin
          Application.ProcessMessages;
          sleep(1);
        end;

      until Terminated;

      // ループを抜けたとき、もし何か押された状態のままだったら、放しておく
      if ucPreNote <> 0 then
      begin
        sendKeyMessage(WM_KEYUP, ucPreNote);
        //ucPreNote := 0;
      end;
      // 押されたキーを片っ端から離す(要らないかな)
      if FSendNoteList.Count > 0 then
        for n := 0 to FSendNoteList.Count -1  do
          sendKeyMessage(WM_KEYUP, FSendNoteList.Items[n]);


    finally
      FSendNoteList.Free;
      procMIDIIn_Close(pMIDIIn);  // MIDIデバイスの解放
    end;

  end;
end;

{----------------------------------------------------------------------------}
// キー情報の送信
procedure TMIDIEventThread.sendKeyMessage(wmEvent: Integer; ucNote: byte);
var
  iIndex  : Integer;
  hwnd    : Integer;
  strKey  : String;
  astrKeys: TStringDynArray;
  n       : Integer;

begin
  hwnd := GetForegroundWindow();

  strKey := FOption.astrKeyMap[ucNote];
  if strKey <> '' then
  begin
    // アサインされたキーを配列に格納
    astrKeys := SplitString(strKey,' ');
    if length(astrKeys)>0 then
    begin
      for n := 0 to length(astrKeys)-1 do
      begin
        if wmEvent = WM_KEYDOWN then      // ノートOnのとき
        begin
          // 指定されたキーの順番で押す
          iIndex := n;
          // 押されたキーを覚えておく
          if FSendNoteList.IndexOf(ucNote) = -1 then FSendNoteList.Add(ucNote);
        end
        else if wmEvent = WM_KEYUP then   // ノートOffのとき
        begin
          // 逆の順番で離す
          iIndex := (length(astrKeys)-n-1);
        end
        else                              // 何だかよくわからないとき
        begin
          iIndex := n;
        end;
{$IFDEF DEBUG}
WriteLn(Format(' winHandle = 0x%x : Note = %d : Key = %s : Event = %d', [hwnd, ucNote, astrKeys[iIndex], wmEvent]));
{$ENDIF}
        // User32.SendMessage をコール
        SendMessage(hwnd, wmEvent, FKeyCode.GetItem(astrKeys[iIndex]), 0);
        // 次のイベントのためにちょっとプロセスを開けておく
        Application.ProcessMessages;

      end;

    end;
  end;
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
//  cbDeviceList.Enabled  := False;
    cbDeviceList.Text     := MSG_DEVICE_ERROR;
    btnStart.Enabled      := False;
  end;

end;

{----------------------------------------------------------------------------}
// 設定情報の読み込み
procedure TMIDIEventThread.ReadIniFile();
var
  iniFile : TIniFile;
  n : Integer;
begin
  iniFile := TiniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    // CONFIGセクションの読み込み
    FOption.strDeviceName := iniFile.ReadString(INISECTION_CONFIG,'device_name','');
    FOption.bPlayOnStart  := iniFile.ReadInteger(INISECTION_CONFIG,'start_on_run',0) = 1;
    FOption.iExitOutRange := iniFile.ReadInteger(INISECTION_CONFIG,'start_on_run',1);

    // MAPPINGセクションの読み込み
    FOption.iMinRange := 0; // 範囲の最小値
    FOption.iMaxRange := 0; // 範囲の最大値
    for n := 0 to 127 do
    begin
      // マッピングにセット(キーが無いときは空欄をセット)
      FOption.astrKeyMap[n] := iniFile.ReadString(INISECTION_MAPPING,IntToStr(n),'');
      // キーが存在するときは、範囲の指定をする
      if FOption.astrKeyMap[n]<>'' then
      begin
        if FOption.iMinRange = 0 then FOption.iMinRange := n;
        FOption.iMinRange := n;
      end;
    end;
  finally
    iniFile.Free;
  end;
end;

{----------------------------------------------------------------------------}
// キーマッピングのセット
// PythonのPyGameに準拠。ただし、全ては大変なので使いそうなものだけ抜粋
// 余裕があったら、すべて登録しておきたい
procedure TMIDIEventThread.setKeyCode();
begin
  // if not Assigned(FKeyMap) Then Exit;
  FKeyCode.Add('backspace'  ,$08);
  FKeyCode.Add('tab'        ,$09);
  FKeyCode.Add('enter'      ,$0D);
  FKeyCode.Add('shift'      ,$10);
  FKeyCode.Add('ctrl'       ,$11);
  FKeyCode.Add('alt'        ,$12);
  FKeyCode.Add('pause'      ,$13);
  FKeyCode.Add('capslock'   ,$14);
  FKeyCode.Add('esc'        ,$1B);
  FKeyCode.Add('space'      ,$20);
  FKeyCode.Add('pageup'     ,$21);
  FKeyCode.Add('pagedown'   ,$22);
  FKeyCode.Add('end'        ,$23);
  FKeyCode.Add('home'       ,$24);
  FKeyCode.Add('left'       ,$25);
  FKeyCode.Add('up'         ,$26);
  FKeyCode.Add('right'      ,$27);
  FKeyCode.Add('down'       ,$28);
  FKeyCode.Add('printscrn'  ,$2C);
  FKeyCode.Add('insert'     ,$2D);
  FKeyCode.Add('delete'     ,$2E);
  FKeyCode.Add('0'          ,$30);
  FKeyCode.Add('1'          ,$31);
  FKeyCode.Add('2'          ,$32);
  FKeyCode.Add('3'          ,$33);
  FKeyCode.Add('4'          ,$34);
  FKeyCode.Add('5'          ,$35);
  FKeyCode.Add('6'          ,$36);
  FKeyCode.Add('7'          ,$37);
  FKeyCode.Add('8'          ,$38);
  FKeyCode.Add('9'          ,$39);
  FKeyCode.Add('a'          ,$41);
  FKeyCode.Add('b'          ,$42);
  FKeyCode.Add('c'          ,$43);
  FKeyCode.Add('d'          ,$44);
  FKeyCode.Add('e'          ,$45);
  FKeyCode.Add('f'          ,$46);
  FKeyCode.Add('g'          ,$47);
  FKeyCode.Add('h'          ,$48);
  FKeyCode.Add('i'          ,$49);
  FKeyCode.Add('j'          ,$4A);
  FKeyCode.Add('k'          ,$4B);
  FKeyCode.Add('l'          ,$4C);
  FKeyCode.Add('m'          ,$4D);
  FKeyCode.Add('n'          ,$4E);
  FKeyCode.Add('o'          ,$4F);
  FKeyCode.Add('p'          ,$50);
  FKeyCode.Add('q'          ,$51);
  FKeyCode.Add('r'          ,$52);
  FKeyCode.Add('s'          ,$53);
  FKeyCode.Add('t'          ,$54);
  FKeyCode.Add('u'          ,$55);
  FKeyCode.Add('v'          ,$56);
  FKeyCode.Add('w'          ,$57);
  FKeyCode.Add('x'          ,$58);
  FKeyCode.Add('y'          ,$59);
  FKeyCode.Add('z'          ,$5A);
  FKeyCode.Add('f1'         ,$70);
  FKeyCode.Add('f2'         ,$71);
  FKeyCode.Add('f3'         ,$72);
  FKeyCode.Add('f4'         ,$73);
  FKeyCode.Add('f5'         ,$74);
  FKeyCode.Add('f6'         ,$75);
  FKeyCode.Add('f7'         ,$76);
  FKeyCode.Add('f8'         ,$77);
  FKeyCode.Add('f9'         ,$78);
  FKeyCode.Add('f10'        ,$79);
  FKeyCode.Add('f11'        ,$7A);
  FKeyCode.Add('f12'        ,$7B);

end;
end.
