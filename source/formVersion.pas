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
    { Private 宣言 }
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
    { Public 宣言 }
  end;



implementation

{$R *.DFM}


const
  cMonth : array[1..12] of String = ('Jan','Feb','Mar','Apr','May','Jun',
                                     'Jul','Aug','Sep','Oct','Nov','Dec');
  cNum   : array[1..4] of String = ('st','nd','rd','th');


{----------------------------------------------------------------------------}
{ 作品が作成された日付を返す }
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
{フォームが作成されたときの処理}
procedure TBardPlayVersionInfo.FormCreate(Sender: TObject);
var
  nInc     : SmallInt;
begin
  with imgIcon.Picture.Icon do Handle := LoadIcon(hInstance,'MAINICON');

  nInc := pnlAbout.Width + (imgIcon.Width div 2);  //ラベルの位置を決めるための定数
  // バージョン情報の取得
  GetEXEDLLVersionInfo(Application.exeName);

  //ファイルの説明 (日本語のファイル名)
  with labJpProductName do
  begin
    Caption := FFileDescription;
    Left    :=(nInc - Width) div 2;
  end;
  //ファイルのバージョン
  with labVersion do
  begin
    Caption := FFileVersion + ' ';
    Left    :=(nInc - Width) div 2  ;
  end;
  //プロダクト名(英語のファイルの説明)
  with labUsProductName do
  begin
    Caption := FProductName;
    Left    := (nInc - Width) div 2;
  end;
  //スクロールテロップ
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
{フォームを閉じる}
procedure TBardPlayVersionInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;



//-----------------------------------------------------------------------------
//  実際にアプリケーションのバージョン等の値を取得して表示するメソッド
//  引数はファイルのフルパス名
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

    //ロケール識別子とコードページを取得
    VerQueryValue(pInfo, '\VarFileInfo\Translation', Pointer(pLangCode), InfoSize);

    //上で取得した値を元に，
    //各種情報取得用に，VerQueryValue関数の第2引数で使用する文字列を作成
    SubBlock := IntToHex(pLangCode.wLanguage, 4) + IntToHex(pLangCode.wCodePage, 4);
    SubBlock := '\StringFileInfo\' + SubBlock + PathDelim;


    strList := TStringList.Create;
    try
      //取得する項目の名前を文字列配列に格納
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

      //項目名に相当するメンバーの値を順番に取得
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

