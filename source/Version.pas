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
    '�A���r�A��',
    '�u���K���A��',
    '�J�^���j�A��',
    '�`���I�Ȓ�����',
    '�`�F�R��',
    '�f���}�[�N��',
    '�h�C�c��',
    '�M���V����',
    '�A�����J�p��',
    '�W���X�y�C����',
    '�t�B�������h��',
    '�t�����X��',
    '�w�u���C��',
    '�n���K���[��',
    '�A�C�X�����h��',
    '�C�^���A��',
    '���{��',
    '�؍���',
    '�I�����_��',
    '�m���E�F�[��|�u�[�N���[��',
    '�|�[�����h��',
    '�u���W���n�|���g�K����',
    '���[�g���}���X��',
    '���[�}�j�A��',
    '���V�A��',
    '�N���A�`�A�|�Z���r�A��i���e���j',
    '�X���o�L�A��',
    '�A���o�j�A��',
    '�X�E�F�[�f����',
    '�^�C��',
    '�g���R��',
    '�E���h�D�[��',
    '�o�n�T��',
    '�ȑ̎�������',
    '�X�C�X�n�h�C�c��',
    '�C�M���X�p��',
    '���L�V�R�n�X�y�C����',
    '�x���M�[�n�t�����X��',
    '�X�C�X�n�C�^���A��',
    '�x���M�[�n�I�����_��',
    '�m���E�F�[��|�j�[�m�V�N',
    '�|���g�K����',
    '�Z���r�A�|�N���A�`�A��i�L�����j',
    '�J�i�_�n�t�����X��',
    '�X�C�X�n�t�����X��'
  );
*)

  CharSetID: array[1..CharSetMax] of Word = (
       0,  932,  949,  950, 1200, 1250,
    1251, 1252, 1253, 1254, 1255, 1256
  );

(*
  CharSetName: array[1..CharSetMax] of String[16] = (
    '7bit ASCII',
    '���{',
    '�؍�',
    '��p',
    'Unicode',
    '���e��2�i�����j',
    '�L����',
    '������',
    '�M���V����',
    '�g���R��',
    '�w�u���C��',
    '�A���r�A��'
  );
*)

resourcestring
  cLanguageName00 = '����`�̌���';
  cLanguageName01 =  '�A���r�A��';
  cLanguageName02 =  '�u���K���A��';
  cLanguageName03 =  '�J�^���j�A��';
  cLanguageName04 =  '�`���I�Ȓ�����';
  cLanguageName05 =  '�`�F�R��';
  cLanguageName06 =  '�f���}�[�N��';
  cLanguageName07 =  '�h�C�c��';
  cLanguageName08 =  '�M���V����';
  cLanguageName09 =  '�A�����J�p��';
  cLanguageName10 =  '�W���X�y�C����';
  cLanguageName11 =  '�t�B�������h��';
  cLanguageName12 =  '�t�����X��';
  cLanguageName13 =  '�w�u���C��';
  cLanguageName14 =  '�n���K���[��';
  cLanguageName15 =  '�A�C�X�����h��';
  cLanguageName16 =  '�C�^���A��';
  cLanguageName17 =  '���{��';
  cLanguageName18 =  '�؍���';
  cLanguageName19 =  '�I�����_��';
  cLanguageName20 =  '�m���E�F�[��|�u�[�N���[��';
  cLanguageName21 =  '�|�[�����h��';
  cLanguageName22 =  '�u���W���n�|���g�K����';
  cLanguageName23 =  '���[�g���}���X��';
  cLanguageName24 =  '���[�}�j�A��';
  cLanguageName25 =  '���V�A��';
  cLanguageName26 =  '�N���A�`�A�|�Z���r�A��i���e���j';
  cLanguageName27 =  '�X���o�L�A��';
  cLanguageName28 =  '�A���o�j�A��';
  cLanguageName29 =  '�X�E�F�[�f����';
  cLanguageName30 =  '�^�C��';
  cLanguageName31 =  '�g���R��';
  cLanguageName32 =  '�E���h�D�[��';
  cLanguageName33 =  '�o�n�T��';
  cLanguageName34 =  '�ȑ̎�������';
  cLanguageName35 =  '�X�C�X�n�h�C�c��';
  cLanguageName36 =  '�C�M���X�p��';
  cLanguageName37 =  '���L�V�R�n�X�y�C����';
  cLanguageName38 =  '�x���M�[�n�t�����X��';
  cLanguageName39 =  '�X�C�X�n�C�^���A��';
  cLanguageName40 =  '�x���M�[�n�I�����_��';
  cLanguageName41 =  '�m���E�F�[��|�j�[�m�V�N';
  cLanguageName42 =  '�|���g�K����';
  cLanguageName43 =  '�Z���r�A�|�N���A�`�A��i�L�����j';
  cLanguageName44 =  '�J�i�_�n�t�����X��';
  cLanguageName45 =  '�X�C�X�n�t�����X��';


  cCharSetName01 = '7bit ASCII';
  cCharSetName02 = '���{';
  cCharSetName03 = '�؍�';
  cCharSetName04 = '��p';
  cCharSetName05 = 'Unicode';
  cCharSetName06 = '���e��2�i�����j';
  cCharSetName07 = '�L����';
  cCharSetName08 = '������';
  cCharSetName09 = '�M���V����';
  cCharSetName10 = '�g���R��';
  cCharSetName11 = '�w�u���C��';
  cCharSetName12 = '�A���r�A��';


  cS_FF_Debug        = '�f�o�b�O��񂠂�,';
  cS_FF_InfoInferred = '�o�[�W�������͓��I�ɐ���,';
  cS_FF_Patched      = '�t�@�C���͏C�����������Ă���,';
  cS_FF_PreRelease   = '�J���r���o�[�W����,';
  cS_FF_PrivateBuild = '���Ɣ�,';
  cS_FF_SpecialBuild = '���ʃo�[�W����,';

  cOS_Unknown       = '����`';
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

  cft_UnKnown    = '����`';
  cft_APP        = '�A�v���P�[�V����';
  cft_DLL        = '�_�C�i�~�b�N�����N���C�u����';
  cft_Drv        = '�f�o�C�X�h���C�o';
  cft_Font       = '�t�H���g�t�@�C��';
  cft_Vxd        = '���z�f�o�C�X�h���C�o';
  cft_Static_Lib = '�X�^�e�B�b�N�����N���C�u����';

  cft2_Drv_UnKnown     = '�s���ȃf�o�C�X�h���C�o';
  cft2_Drv_Comm        = 'COM �|�[�g�h���C�o';
  cft2_Drv_Printer     = '�v�����^�h���C�o';
  cft2_Drv_Keyboard    = '�L�[�{�[�h�h���C�o';
  cft2_Drv_Language    = '����h���C�o';
  cft2_Drv_Display     = '�f�B�X�v���C�h���C�o';
  cft2_Drv_Mouse       = '�}�E�X�h���C�o';
  cft2_Drv_Network     = '�l�b�g���[�N�h���C�o';
  cft2_Drv_System      = '�V�X�e���h���C�o';
  cft2_Drv_Installable = '�C���X�g�[���\�ȃf�o�C�X�h���C�o';
  cft2_Drv_Sound       = '�T�E���h�h���C�o';

  cft2_Font_UnKnown  = '�F���s���ȃt�H���g';
  cft2_Font_Raster   = '���X�^�t�H���g';
  cft2_Font_Vector   = '�x�N�g���t�H���g';
  cft2_Font_TrueType = 'TrueType �t�H���g';

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
    result := cLanguageName00;//'����`�̌���';
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
    BufStr := BufStr + cS_FF_Debug;          //'�f�o�b�O��񂠂�,';
  if VS_FF_InfoInferred and lBuf <> 0 then
    BufStr := BufStr + cS_FF_InfoInferred;   //'�o�[�W�������͓��I�ɐ���,';
  if VS_FF_Patched and lBuf <> 0 then
    BufStr := BufStr + cS_FF_Patched;        //'�t�@�C���͏C�����������Ă���,';
  if VS_FF_PreRelease and lBuf <> 0 then
    BufStr := BufStr + cS_FF_PreRelease;     //'�J���r���o�[�W����,';
  if VS_FF_PrivateBuild and lBuf <> 0 then
    BufStr := BufStr + cS_FF_PrivateBuild;   //'���Ɣ�,';
  if VS_FF_SpecialBuild and lBuf <> 0 then
    BufStr := BufStr + cS_FF_SpecialBuild;   //'���ʃo�[�W����,';

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
    BufStr := cOS_Unknown;       //'����`';
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
    result := cft_UnKnown;            //'����`';
  if lBuf = vft_APP then
    result := cft_APP;                //'�A�v���P�[�V����';
  if lBuf = vft_DLL then
    result := cft_DLL;                //'�_�C�i�~�b�N�����N���C�u����';
  if lBuf = vft_Drv then
    result := cft_Drv;                //'�f�o�C�X�h���C�o';
  if lBuf = vft_Font then
    result := cft_Font;               //'�t�H���g�t�@�C��';
  if lBuf = vft_Vxd then
    result := cft_Vxd;                //'���z�f�o�C�X�h���C�o';
  if lBuf = vft_Static_Lib then
    result := cft_Static_Lib;         //'�X�^�e�B�b�N�����N���C�u����';
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
        result := cft2_Drv_UnKnown;            //'�s���ȃf�o�C�X�h���C�o';
      if Tmp = vft2_Drv_Comm then
        result := cft2_Drv_Comm;               //'COM �|�[�g�h���C�o';
      if Tmp = vft2_Drv_Printer then
        result := cft2_Drv_Printer;            //'�v�����^�h���C�o';
      if Tmp = vft2_Drv_Keyboard then
        result := cft2_Drv_Keyboard;           //'�L�[�{�[�h�h���C�o';
      if Tmp = vft2_Drv_Language then
        result := cft2_Drv_Language;           //'����h���C�o';
      if Tmp = vft2_Drv_Display then
        result := cft2_Drv_Display;            //'�f�B�X�v���C�h���C�o';
      if Tmp = vft2_Drv_Mouse then
        result := cft2_Drv_Mouse;              //'�}�E�X�h���C�o';
      if Tmp = vft2_Drv_Network then
        result := cft2_Drv_Network;            //'�l�b�g���[�N�h���C�o';
      if Tmp = vft2_Drv_System then
        result := cft2_Drv_System;             //'�V�X�e���h���C�o';
      if Tmp = vft2_Drv_Installable then
        result := cft2_Drv_Installable;        //'�C���X�g�[���\�ȃf�o�C�X�h���C�o';
      if Tmp = vft2_Drv_Sound then
        result := cft2_Drv_Sound;              //'�T�E���h�h���C�o';
    end;
    if FileType = vft_Font then begin
      Tmp := SubType;
      if Tmp = vft2_UnKnown then
        result := cft2_Font_UnKnown;    //'�F���s���ȃt�H���g';
      if Tmp = vft2_Font_Raster then
        result := cft2_Font_Raster;     //'���X�^�t�H���g';
      if Tmp = vft2_Font_Vector then
        result := cft2_Font_Vector;     //'�x�N�g���t�H���g';
      if Tmp = vft2_Font_TrueType then
        result := cft2_Font_TrueType;   //'TrueType �t�H���g';
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
