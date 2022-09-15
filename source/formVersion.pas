unit FormVersion;

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls,Dialogs,SysUtils;

type
  TBardPlayVersionInfo = class(TForm)
    pnlAbout:         TPanel;
    OKButton:         TButton;
    labJpProductName: TLabel;
    labVersion:       TLabel;
    labCopyright:     TLabel;
    labUsProductName: TLabel;
    imgIcon:          TImage;
    Memo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MemoChange(Sender: TObject);
  private
    { Private �錾 }
    FCompanyName      : String;
    FFileDescription  : String;
    FFileVersion      : String;
    FInternalName     : String;
    FLegalCopyright   : String;
    FLegalTrademarks  : String;
    FOriginalFilename : String;
    FProductName      : String;
    FProductVersion   : String;
    FComments         : String;
    FSpecialBuild     : String;
    FPrivateBuild     : String;
    FSampleNumber     : String;

    function Today : String;
    function GetEXEDLLVersionInfo(FileName: string): Boolean;
  public
    { Public �錾 }
  end;



implementation

{$R *.DFM}


const
  cMonth : array[1..12] of String = ('Jan','Feb','Mar','Apr','May','Jun',
                                     'Jul','Aug','Sep','Oct','Nov','Dec');
  cNum   : array[1..4] of String = ('st','nd','rd','th');


{----------------------------------------------------------------------------}
{ ��i���쐬���ꂽ���t��Ԃ� }
function TBardPlayVersionInfo.Today : String;
var
  iYY,iMM,iDD : Word;
  sAdd        : String;
  oDate       : TDateTime;
begin
  if FileAge(Application.ExeName,oDate,True) then
  begin
    DecodeDate(oDate,iYY,iMM,iDD);
    if iDD<4 then
      sAdd := cNum[iDD]
    else
      sAdd := cNum[4];

    result := Format('%s. %d%s , %d',[cMonth[iMM],iDD,sAdd,iYY]);
  end
  else
    result := '';
end;

{----------------------------------------------------------------------------}
{�t�H�[�����쐬���ꂽ�Ƃ��̏���}
procedure TBardPlayVersionInfo.FormCreate(Sender: TObject);
var
  nInc     : SmallInt;
begin
  with imgIcon.Picture.Icon do Handle := LoadIcon(hInstance,'MAINICON');

  nInc := pnlAbout.Width + (imgIcon.Width div 2);  //���x���̈ʒu�����߂邽�߂̒萔
  // �o�[�W�������̎擾
  GetEXEDLLVersionInfo(Application.exeName);

  //�t�@�C���̐��� (���{��̃t�@�C����)
  with labJpProductName do
  begin
    Caption := FFileDescription;
    Left    :=(nInc - Width) div 2;
  end;
  //�t�@�C���̃o�[�W����
  with labVersion do
  begin
    Caption := FFileVersion + ' ';
    Left    :=(nInc - Width) div 2  ;
  end;
  //�v���_�N�g��(�p��̃t�@�C���̐���)
  with labUsProductName do
  begin
    Caption := FProductName;
    Left    := (nInc - Width) div 2;
  end;
  //�X�N���[���e���b�v
  with Memo.Lines do
  begin
    Insert(0,'-'+FFileDescription+'-');
    Insert(0,'');
    Insert(0,'Version '+FFileVersion);
    Insert(0,FProductName);
    Insert(Count-2,Today);
  end;



end;

{----------------------------------------------------------------------------}
procedure TBardPlayVersionInfo.MemoChange(Sender: TObject);
begin

end;

{----------------------------------------------------------------------------}
{�t�H�[�������}
procedure TBardPlayVersionInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;



//-----------------------------------------------------------------------------
//  ���ۂɃA�v���P�[�V�����̃o�[�W�������̒l���擾���ĕ\�����郁�\�b�h
//  �����̓t�@�C���̃t���p�X��
//-----------------------------------------------------------------------------
// http://mrxray.on.coocan.jp/Delphi/plSamples/318_AppVersionInfo.htm
function TBardPlayVersionInfo.GetEXEDLLVersionInfo(FileName: string): Boolean;
type
  TLangAndCodePage = record
    wLanguage : WORD;
    wCodePage : WORD;
  end;
  PLangAndCodePage = ^TLangAndCodePage;

var
  dwHandle    : Cardinal;
  pInfo       : Pointer;
  pLangCode   : PLangAndCodePage;
  SubBlock    : String;
  InfoSize    : DWORD;
  pFileInfo   : Pointer;
  strList     : TStringList;
  KeyName     : String;
  StrText     : String;
  n           : Integer;

begin
  Result := False;

  InfoSize := GetFileVersionInfoSize(PChar(FileName), dwHandle);
  if InfoSize = 0 then exit;

  GetMem(pInfo, InfoSize);
  try
    GetFileVersionInfo(PChar(FileName), 0, InfoSize, pInfo);

    //���P�[�����ʎq�ƃR�[�h�y�[�W���擾
    VerQueryValue(pInfo, '\VarFileInfo\Translation', Pointer(pLangCode), InfoSize);

    //��Ŏ擾�����l�����ɁC
    //�e����擾�p�ɁCVerQueryValue�֐��̑�2�����Ŏg�p���镶������쐬
    SubBlock := IntToHex(pLangCode.wLanguage, 4) + IntToHex(pLangCode.wCodePage, 4);
    SubBlock := '\StringFileInfo\' + SubBlock + PathDelim;


    strList := TStringList.Create;
    try
      //�擾���鍀�ڂ̖��O�𕶎���z��Ɋi�[
      strList.Add(SubBlock + 'CompanyName');
      strList.Add(SubBlock + 'FileDescription');
      strList.Add(SubBlock + 'FileVersion');
      strList.Add(SubBlock + 'InternalName');
      strList.Add(SubBlock + 'LegalCopyright');
      strList.Add(SubBlock + 'LegalTrademarks');
      strList.Add(SubBlock + 'OriginalFilename');
      strList.Add(SubBlock + 'ProductName');
      strList.Add(SubBlock + 'ProductVersion');
      strList.Add(SubBlock + 'Comments');
      strList.Add(SubBlock + 'SpecialBuild');
      strList.Add(SubBlock + 'PrivateBuild');
      strList.Add(SubBlock + 'Sample-Number');

      //���ږ��ɑ������郁���o�[�̒l�����ԂɎ擾
      for n := 0 to strList.Count-1 do
      begin
        KeyName := strList.Strings[n];
        if VerQueryValue(pInfo,PChar(KeyName),Pointer(pFileInfo),InfoSize) then 
        begin
          StrText := Format('%-16s', [ExtractFileName(KeyName)]);
          case n of
             0:FCompanyName     := PChar(pFileInfo);
             1:FFileDescription := PChar(pFileInfo);
             2:FFileVersion     := PChar(pFileInfo);
             3:FInternalName    := PChar(pFileInfo);
             4:FLegalCopyright  := PChar(pFileInfo);
             5:FLegalTrademarks := PChar(pFileInfo);
             6:FOriginalFilename:= PChar(pFileInfo);
             7:FProductName     := PChar(pFileInfo);
             8:FProductVersion  := PChar(pFileInfo);
             9:FComments        := PChar(pFileInfo);
            10:FSpecialBuild    := PChar(pFileInfo);
            11:FPrivateBuild    := PChar(pFileInfo);
            12:FSampleNumber    := PChar(pFileInfo);
          end;
        end;
      end;
    finally
      strList.Free;
    end;

    Result := True;
  finally
    FreeMem(pInfo, InfoSize);
  end;
end;



end.

