(*
  Version resource object
  copyright(c) 1996,1997 tac all rights reserved

  Warning: This unit is only proven with Delphi 2.0J
*)

unit Version;

interface

type
  TVersion = class( TObject )
  private
    FHandle: LongInt;         // Instance Handle = hInstance
    FVersionInfo: Pointer;    // Pointer of VersionInfo
    FSize: Word;              // Size of VersionInfo

    FLanguage: Word;          // Language code
    FCharSet: Word;           // Character set code
    FLangChar: String;     // String formatted Language and Character

    procedure GetLangCharSet;
    function GetStrInfo( subtype: string ): string;

    function GetFileVersion: string;
    function GetProductVersion: string;
    function GetFileFlags: LongInt;
    function GetFileOS: LongInt;
    function GetFileType: LongInt;
    function GetSubType: LongInt;

    function GetFileOSString: string;
    function GetFileFlagString: string;
    function GetFileTypeString: string;
    function GetSubTypeString: string;
    function GetLanguage: string;
    function GetCharSet: string;

    function GetComment: string;
    function GetCompanyName: string;
    function GetFileDescription: string;
    function GetFileVerStr: string;
    function GetInternalName: string;
    function GetLegalCopyright: string;
    function GetLegalTradeMarks: string;
    function GetOriginalFilename: string;
    function GetPrivateBuild: string;
    function GetProductName: string;
    function GetProductVerStr: string;
    function GetSpecialBuild: string;

    function LanguageName(nIndex : ShortInt) : String;
    function CharSetName(nIndex : ShortInt) : String;

  public
    constructor Create( filename: string );
    procedure Free;

    property FileVersion: string read GetFileVersion;
    property ProductVersion: string read GetProductVersion;
    property FileFlags: LongInt read GetFileFlags;
    property FileOS: LongInt read GetFileOS;
    property FileType: LongInt read GetFileType;
    property SubType: LongInt read GetSubType;

    property StrFileOS: string read GetFileOSString;
    property StrFileFlag: string read GetFileFlagString;
    property StrFileType: string read GetFileTypeString;
    property StrSubType: string read GetSubTypeString;
    property Language: string read GetLanguage;
    property CharSet: string read GetCharSet;

    property Comments: string read GetComment;
    property CompanyName: string read GetCompanyName;
    property FileDescription: string read GetFileDescription;
    property FileVerStr: string read GetFileVerStr;
    property InternalName: string read GetInternalName;
    property LegalCopyright: string read GetLegalCopyright;
    property LegalTradeMarks: string read GetLegalTradeMarks;
    property OriginalFilename: string read GetOriginalFilename;
    property PrivateBuild: string read GetPrivateBuild;
    property ProductName: string read GetProductName;
    property ProductVerStr: string read GetProductVerStr;
    property SpecialBuild: string read GetSpecialBuild;
  end;

implementation

uses
  SysUtils, Windows;

const
  LanguageMax = 45;
  CharSetMax = 12;

  LanguageID: array[1..LanguageMax] of Word = (
    $0401, $0402, $0403, $0404, $0405,
    $0406, $0407, $0408, $0409, $040a,
    $040b, $040c, $040d, $040e, $040f,
    $0410, $0411, $0412, $0413, $0414,
    $0415, $0416, $0417, $0418, $0419,
    $041a, $041b, $041c, $041d, $041e,
    $041f, $0420, $0421, $0804, $0807,
    $0809, $080a, $080c, $0810, $0813,
    $0814, $0816, $081a, $0c0c, $100c
  );
(*
  LanguageName: array[1..LanguageMax] of string[33] = (
    'アラビア語',
    'ブルガリア語',
    'カタロニア語',
    '伝統的な中国語',
    'チェコ語',
    'デンマーク語',
    'ドイツ語',
    'ギリシャ語',
    'アメリカ英語',
    '標準スペイン語',
    'フィンランド語',
    'フランス語',
    'ヘブライ語',
    'ハンガリー語',
    'アイスランド語',
    'イタリア語',
    '日本語',
    '韓国語',
    'オランダ語',
    'ノルウェー語－ブークモール',
    'ポーランド語',
    'ブラジル系ポルトガル語',
    'レートロマンス語',
    'ルーマニア語',
    'ロシア語',
    'クロアチア－セルビア語（ラテン）',
    'スロバキア語',
    'アルバニア語',
    'スウェーデン語',
    'タイ語',
    'トルコ語',
    'ウルドゥー語',
    'バハサ語',
    '簡体字中国語',
    'スイス系ドイツ語',
    'イギリス英語',
    'メキシコ系スペイン語',
    'ベルギー系フランス語',
    'スイス系イタリア語',
    'ベルギー系オランダ語',
    'ノルウェー語－ニーノシク',
    'ポルトガル語',
    'セルビア－クロアチア語（キリル）',
    'カナダ系フランス語',
    'スイス系フランス語'
  );
*)

  CharSetID: array[1..CharSetMax] of Word = (
       0,  932,  949,  950, 1200, 1250,
    1251, 1252, 1253, 1254, 1255, 1256
  );

(*
  CharSetName: array[1..CharSetMax] of String[16] = (
    '7bit ASCII',
    '日本',
    '韓国',
    '台湾',
    'Unicode',
    'ラテン2（東欧）',
    'キリル',
    '多言語',
    'ギリシャ語',
    'トルコ語',
    'ヘブライ語',
    'アラビア語'
  );
*)

resourcestring
  cLanguageName00 = '未定義の言語';
  cLanguageName01 =  'アラビア語';
  cLanguageName02 =  'ブルガリア語';
  cLanguageName03 =  'カタロニア語';
  cLanguageName04 =  '伝統的な中国語';
  cLanguageName05 =  'チェコ語';
  cLanguageName06 =  'デンマーク語';
  cLanguageName07 =  'ドイツ語';
  cLanguageName08 =  'ギリシャ語';
  cLanguageName09 =  'アメリカ英語';
  cLanguageName10 =  '標準スペイン語';
  cLanguageName11 =  'フィンランド語';
  cLanguageName12 =  'フランス語';
  cLanguageName13 =  'ヘブライ語';
  cLanguageName14 =  'ハンガリー語';
  cLanguageName15 =  'アイスランド語';
  cLanguageName16 =  'イタリア語';
  cLanguageName17 =  '日本語';
  cLanguageName18 =  '韓国語';
  cLanguageName19 =  'オランダ語';
  cLanguageName20 =  'ノルウェー語－ブークモール';
  cLanguageName21 =  'ポーランド語';
  cLanguageName22 =  'ブラジル系ポルトガル語';
  cLanguageName23 =  'レートロマンス語';
  cLanguageName24 =  'ルーマニア語';
  cLanguageName25 =  'ロシア語';
  cLanguageName26 =  'クロアチア－セルビア語（ラテン）';
  cLanguageName27 =  'スロバキア語';
  cLanguageName28 =  'アルバニア語';
  cLanguageName29 =  'スウェーデン語';
  cLanguageName30 =  'タイ語';
  cLanguageName31 =  'トルコ語';
  cLanguageName32 =  'ウルドゥー語';
  cLanguageName33 =  'バハサ語';
  cLanguageName34 =  '簡体字中国語';
  cLanguageName35 =  'スイス系ドイツ語';
  cLanguageName36 =  'イギリス英語';
  cLanguageName37 =  'メキシコ系スペイン語';
  cLanguageName38 =  'ベルギー系フランス語';
  cLanguageName39 =  'スイス系イタリア語';
  cLanguageName40 =  'ベルギー系オランダ語';
  cLanguageName41 =  'ノルウェー語－ニーノシク';
  cLanguageName42 =  'ポルトガル語';
  cLanguageName43 =  'セルビア－クロアチア語（キリル）';
  cLanguageName44 =  'カナダ系フランス語';
  cLanguageName45 =  'スイス系フランス語';


  cCharSetName01 = '7bit ASCII';
  cCharSetName02 = '日本';
  cCharSetName03 = '韓国';
  cCharSetName04 = '台湾';
  cCharSetName05 = 'Unicode';
  cCharSetName06 = 'ラテン2（東欧）';
  cCharSetName07 = 'キリル';
  cCharSetName08 = '多言語';
  cCharSetName09 = 'ギリシャ語';
  cCharSetName10 = 'トルコ語';
  cCharSetName11 = 'ヘブライ語';
  cCharSetName12 = 'アラビア語';


  cS_FF_Debug        = 'デバッグ情報あり,';
  cS_FF_InfoInferred = 'バージョン情報は動的に生成,';
  cS_FF_Patched      = 'ファイルは修正を加えられている,';
  cS_FF_PreRelease   = '開発途中バージョン,';
  cS_FF_PrivateBuild = '私家版,';
  cS_FF_SpecialBuild = '特別バージョン,';

  cOS_Unknown       = '未定義';
  cOS_Dos           = 'MS-DOS';
  cOS_OS216         = '16bit OS/2';
  cOS_OS232         = '32bit OS/2';
  cOS_NT            = 'Windows NT';
  cOS__Windows16    = '16bit Windows';
  cOS__PM16         = '16bit PM';
  cOS__PM32         = '32bit PM';
  cOS__Windows32    = '32bit Windows';
  cOS_DOS_Windows16 = '16bit Windows on DOS';
  cOS_DOS_Windows32 = '32bit Windows on DOS';
  cOS_OS216_PM16    = '16bit OS/2 PM';
  cOS_OS232_PM32    = '32bit OS/2 PM';
  cOS_NT_Windows32  = '32bit Windows on NT';

  cft_UnKnown    = '未定義';
  cft_APP        = 'アプリケーション';
  cft_DLL        = 'ダイナミックリンクライブラリ';
  cft_Drv        = 'デバイスドライバ';
  cft_Font       = 'フォントファイル';
  cft_Vxd        = '仮想デバイスドライバ';
  cft_Static_Lib = 'スタティックリンクライブラリ';

  cft2_Drv_UnKnown     = '不明なデバイスドライバ';
  cft2_Drv_Comm        = 'COM ポートドライバ';
  cft2_Drv_Printer     = 'プリンタドライバ';
  cft2_Drv_Keyboard    = 'キーボードドライバ';
  cft2_Drv_Language    = '言語ドライバ';
  cft2_Drv_Display     = 'ディスプレイドライバ';
  cft2_Drv_Mouse       = 'マウスドライバ';
  cft2_Drv_Network     = 'ネットワークドライバ';
  cft2_Drv_System      = 'システムドライバ';
  cft2_Drv_Installable = 'インストール可能なデバイスドライバ';
  cft2_Drv_Sound       = 'サウンドドライバ';

  cft2_Font_UnKnown  = '認識不明なフォント';
  cft2_Font_Raster   = 'ラスタフォント';
  cft2_Font_Vector   = 'ベクトルフォント';
  cft2_Font_TrueType = 'TrueType フォント';

constructor TVersion.Create( filename: string );
var
  fname: PChar;
  Handle: DWORD;
  Flag: Boolean;
begin
  inherited Create;
  fname := StrAlloc( 256 );
  StrPCopy( fname, filename );
  Handle := 0;

  FSize := GetFileVersionInfoSize( fname, Handle );
  if FSize = 0 then
    FHandle := 0
  else begin
    FHandle := hInstance;
    FVersionInfo := AllocMem( FSize );
    Flag := GetFileVersionInfo( fname, FHandle, FSize, FVersionInfo );
    if Flag <> True then
      FHandle := 0 { No VersionInfo }
    else
      GetLangCharSet;
  end; { of if }

  StrDispose( fname );
end;

procedure TVersion.Free;
begin
  FreeMem( FVersionInfo, FSize );
  inherited Destroy;
end;

procedure TVersion.GetLangCharSet;
var
  Ptr: Pointer;
  LangChar: LongInt;
  Len: UINT;
begin
  if FHandle <> 0 then begin
    VerQueryValue( FVersionInfo, '\VarFileInfo\Translation', Ptr, Len );
    LangChar := LongInt(Ptr^);
    FLanguage := LoWord( LangChar );
    FCharSet := HiWord( LangChar );

    FLangChar := IntToHex( FLanguage,4 ) + IntToHex( FCharSet,4 );
  end; { of if }
end;

function TVersion.GetStrInfo( subtype:string ):string;
var
  Ptr: Pointer;
  Len: UINT;
  msg: string;
  pmsg: PChar;
  flag: Boolean;
begin
  result := '';
  if FHandle <> 0 then begin
    pmsg := StrAlloc( 256 );
    msg := '\StringFileInfo\'+String(FLangChar)+'\'+subtype;
    StrPCopy( pmsg, msg );

    flag := VerQueryValue( FVersionInfo, pmsg, Ptr, Len );
    if ( flag <> False ) and ( Len <> 0 ) then
      result := StrPas( PChar(Ptr) );
    StrDispose( pmsg );
  end;
end;

function TVersion.GetLanguage:String;
var
  lc: Integer;
begin
  if FHandle <> 0 then begin
    for lc := 1 to LanguageMax do
      if FLanguage = LanguageID[lc] then
        result := LanguageName(lc);
    end
  else
    result := cLanguageName00;//'未定義の言語';
end;

function TVersion.GetCharSet:string;
var
  lc: Integer;
begin
  if FHandle <> 0 then begin
    for lc := 1 to CharSetMax do
      if FCharSet = CharSetID[lc] then
        result := CharSetName(lc);
    end
  else
    result := ''
end;

function TVersion.GetFileVersion:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'FileVersion' );
end;

function TVersion.GetProductVersion:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'ProductVersion' );
end;

function TVersion.GetFileFlags:LongInt;
var
  Ptr: Pointer;
  Len: UINT;
begin
  if FHandle <> 0 then begin
    VerQueryValue( FVersionInfo, '\', Ptr, Len );
    with TvsFixedFileInfo(Ptr^)  do
      result := dwFileFlags and dwFileFlagsMask;
  end else
    result := 0;
end;



function TVersion.GetFileFlagString:string;
var
  BufStr: string;
  lBuf: LongInt;
begin
  result := '';
  BufStr := '';
  lBuf := FileFlags;
  if VS_FF_Debug and lBuf <> 0 then
    BufStr := BufStr + cS_FF_Debug;          //'デバッグ情報あり,';
  if VS_FF_InfoInferred and lBuf <> 0 then
    BufStr := BufStr + cS_FF_InfoInferred;   //'バージョン情報は動的に生成,';
  if VS_FF_Patched and lBuf <> 0 then
    BufStr := BufStr + cS_FF_Patched;        //'ファイルは修正を加えられている,';
  if VS_FF_PreRelease and lBuf <> 0 then
    BufStr := BufStr + cS_FF_PreRelease;     //'開発途中バージョン,';
  if VS_FF_PrivateBuild and lBuf <> 0 then
    BufStr := BufStr + cS_FF_PrivateBuild;   //'私家版,';
  if VS_FF_SpecialBuild and lBuf <> 0 then
    BufStr := BufStr + cS_FF_SpecialBuild;   //'特別バージョン,';

  if BufStr <> '' then
    result := Copy( BufStr, 1, Length( BufStr )-1 );
end;

function TVersion.GetFileOS:LongInt;
var
  Ptr: Pointer;
  Len: UINT;
begin
  if FHandle <> 0 then begin
    VerQueryValue( FVersionInfo, '\', Ptr, Len );
    with TvsFixedFileInfo(Ptr^)  do
      result := dwFileOS;
  end else
    result := 0;
end;


function TVersion.GetFileOSString:string;
var
  BufStr: string;
  lBuf:  LongInt;
begin
  BufStr := '';
  lBuf := FileOS;
  if lBuf = VOS_Unknown then
    BufStr := cOS_Unknown;       //'未定義';
  if lBuf = VOS_Dos then
    BufStr := cOS_Dos;           //'MS-DOS';
  if lBuf = VOS_OS216 then
    BufStr := cOS_OS216;         //'16bit OS/2';
  if lBuf = VOS_OS232 then
    BufStr := cOS_OS232;         //'32bit OS/2';
  if lBuf = VOS_NT then
    BufStr := cOS_NT;            //'Windows NT';
  if lBuf = VOS__Windows16 then
    BufStr := cOS__Windows16;    //'16bit Windows';
  if lBuf = VOS__PM16 then
    BufStr := cOS__PM16;         //'16bit PM';
  if lBuf = VOS__PM32 then
    BufStr := cOS__PM32;         //'32bit PM';
  if lBuf = VOS__Windows32 then
    BufStr := cOS__Windows32;    //'32bit Windows';
  if lBuf = VOS_DOS_Windows16 then
    BufStr := cOS_DOS_Windows16; //'16bit Windows on DOS';
  if lBuf = VOS_DOS_Windows32 then
    BufStr := cOS_DOS_Windows32; //'32bit Windows on DOS';
  if lBuf = VOS_OS216_PM16 then
    BufStr := cOS_OS216_PM16;    //'16bit OS/2 PM';
  if lBuf = VOS_OS232_PM32 then
    BufStr := cOS_OS232_PM32;    //'32bit OS/2 PM';
  if lBuf = VOS_NT_Windows32 then
    BufStr := cOS_NT_Windows32;  //'32bit Windows on NT';

  result := BufStr;
end;

function TVersion.GetFileType:LongInt;
var
  Ptr: Pointer;
  Len: UINT;
begin
  if FHandle <> 0 then begin
    VerQueryValue( FVersionInfo, '\', Ptr, Len );
    with TvsFixedFileInfo(Ptr^)  do
      result := dwFileType;
  end else
    result := 0;
end;




function TVersion.GetFileTypeString:string;
var
  lBuf: LongInt;
begin
  result := '';
  lBuf := FileType;
  if lBuf = vft_UnKnown then
    result := cft_UnKnown;            //'未定義';
  if lBuf = vft_APP then
    result := cft_APP;                //'アプリケーション';
  if lBuf = vft_DLL then
    result := cft_DLL;                //'ダイナミックリンクライブラリ';
  if lBuf = vft_Drv then
    result := cft_Drv;                //'デバイスドライバ';
  if lBuf = vft_Font then
    result := cft_Font;               //'フォントファイル';
  if lBuf = vft_Vxd then
    result := cft_Vxd;                //'仮想デバイスドライバ';
  if lBuf = vft_Static_Lib then
    result := cft_Static_Lib;         //'スタティックリンクライブラリ';
end;

function TVersion.GetSubType:LongInt;
var
  Ptr: Pointer;
  Len: UINT;
begin
  if FHandle <> 0 then begin
    VerQueryValue( FVersionInfo, '\', Ptr, Len );
    with TvsFixedFileInfo(Ptr^)  do
      result := dwFileSubType;
  end else
    result := 0;
end;


function TVersion.GetSubTypeString:string;
var
  tmp: LongInt;
begin
  result := '';
  if FHandle <> 0 then begin
    if FileType = vft_Drv then begin
      Tmp := SubType;
      if Tmp = vft2_UnKnown then
        result := cft2_Drv_UnKnown;            //'不明なデバイスドライバ';
      if Tmp = vft2_Drv_Comm then
        result := cft2_Drv_Comm;               //'COM ポートドライバ';
      if Tmp = vft2_Drv_Printer then
        result := cft2_Drv_Printer;            //'プリンタドライバ';
      if Tmp = vft2_Drv_Keyboard then
        result := cft2_Drv_Keyboard;           //'キーボードドライバ';
      if Tmp = vft2_Drv_Language then
        result := cft2_Drv_Language;           //'言語ドライバ';
      if Tmp = vft2_Drv_Display then
        result := cft2_Drv_Display;            //'ディスプレイドライバ';
      if Tmp = vft2_Drv_Mouse then
        result := cft2_Drv_Mouse;              //'マウスドライバ';
      if Tmp = vft2_Drv_Network then
        result := cft2_Drv_Network;            //'ネットワークドライバ';
      if Tmp = vft2_Drv_System then
        result := cft2_Drv_System;             //'システムドライバ';
      if Tmp = vft2_Drv_Installable then
        result := cft2_Drv_Installable;        //'インストール可能なデバイスドライバ';
      if Tmp = vft2_Drv_Sound then
        result := cft2_Drv_Sound;              //'サウンドドライバ';
    end;
    if FileType = vft_Font then begin
      Tmp := SubType;
      if Tmp = vft2_UnKnown then
        result := cft2_Font_UnKnown;    //'認識不明なフォント';
      if Tmp = vft2_Font_Raster then
        result := cft2_Font_Raster;     //'ラスタフォント';
      if Tmp = vft2_Font_Vector then
        result := cft2_Font_Vector;     //'ベクトルフォント';
      if Tmp = vft2_Font_TrueType then
        result := cft2_Font_TrueType;   //'TrueType フォント';
    end;
  end; { of FHandle if }
end;

function TVersion.GetComment:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'Comments' );
end;

function TVersion.GetCompanyName:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'CompanyName' );
end;

function TVersion.GetFileDescription:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'FileDescription' );
end;

function TVersion.GetFileVerStr:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'FileVersion' );
end;

function TVersion.GetInternalName:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'InternalName' );
end;

function TVersion.GetLegalCopyright:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'LegalCopyright' );
end;

function TVersion.GetLegalTradeMarks:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'LegalTrademarks' );
end;

function TVersion.GetOriginalFilename:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'OriginalFilename' );
end;

function TVersion.GetPrivateBuild:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'PrivateBuild' );
end;

function TVersion.GetProductName:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'ProductName' );
end;

function TVersion.GetProductVerStr:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'ProductVersion' );
end;

function TVersion.GetSpecialBuild:string;
begin
  result := '';
  if FHandle <> 0 then
    result := GetStrInfo( 'SpecialBuild' );
end;


function TVersion.LanguageName(nIndex : ShortInt) : String;
begin
  case nIndex of
    1 : result := cLanguageName01;
    2 : result := cLanguageName02;
    3 : result := cLanguageName03;
    4 : result := cLanguageName04;
    5 : result := cLanguageName05;
    6 : result := cLanguageName06;
    7 : result := cLanguageName07;
    8 : result := cLanguageName08;
    9 : result := cLanguageName09;
   10 : result := cLanguageName10;
   11 : result := cLanguageName11;
   12 : result := cLanguageName12;
   13 : result := cLanguageName13;
   14 : result := cLanguageName14;
   15 : result := cLanguageName15;
   16 : result := cLanguageName16;
   17 : result := cLanguageName17;
   18 : result := cLanguageName18;
   19 : result := cLanguageName19;
   20 : result := cLanguageName20;
   21 : result := cLanguageName21;
   22 : result := cLanguageName22;
   23 : result := cLanguageName23;
   24 : result := cLanguageName24;
   25 : result := cLanguageName25;
   26 : result := cLanguageName26;
   27 : result := cLanguageName27;
   28 : result := cLanguageName28;
   29 : result := cLanguageName29;
   30 : result := cLanguageName30;
   31 : result := cLanguageName31;
   32 : result := cLanguageName32;
   33 : result := cLanguageName33;
   34 : result := cLanguageName34;
   35 : result := cLanguageName35;
   36 : result := cLanguageName36;
   37 : result := cLanguageName37;
   38 : result := cLanguageName38;
   39 : result := cLanguageName39;
   40 : result := cLanguageName40;
   41 : result := cLanguageName41;
   42 : result := cLanguageName42;
   43 : result := cLanguageName43;
   44 : result := cLanguageName44;
   45 : result := cLanguageName45;
  end;
end;

function TVersion.CharSetName(nIndex : ShortInt) : String;
begin
  case nIndex of
    1 : result := cCharSetName01;
    2 : result := cCharSetName02;
    3 : result := cCharSetName03;
    4 : result := cCharSetName04;
    5 : result := cCharSetName05;
    6 : result := cCharSetName06;
    7 : result := cCharSetName07;
    8 : result := cCharSetName08;
    9 : result := cCharSetName09;
   10 : result := cCharSetName10;
   11 : result := cCharSetName11;
   12 : result := cCharSetName12;
  end;
end;

end.
