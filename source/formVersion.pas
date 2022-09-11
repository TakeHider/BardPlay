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
    function Today : String;
  public
    { Public �錾 }
  end;



implementation

{$R *.DFM}
Uses
  Version;

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
  oVersion : TVersion;
  nInc     : SmallInt;
begin
  with imgIcon.Picture.Icon do Handle := LoadIcon(hInstance,'MAINICON');

  nInc := pnlAbout.Width + (imgIcon.Width div 2);  //���x���̈ʒu�����߂邽�߂̒萔
  oVersion := TVersion.Create(Application.ExeName);
  try
    with oVersion do
    begin
      //�t�@�C���̐��� (���{��̃t�@�C����)
      with labJpProductName do
      begin
        Caption := FileDescription;
        Left    :=(nInc - Width) div 2;
      end;
      //�t�@�C���̃o�[�W����
      with labVersion do
      begin
        Caption := FileVersion + ' ';
        Left    :=(nInc - Width) div 2  ;
      end;
      //�v���_�N�g��(�p��̃t�@�C���̐���)
      with labUsProductName do
      begin
        Caption := ProductName;
        Left    := (nInc - Width) div 2;
      end;
      //�X�N���[���e���b�v
      with Memo.Lines do
      begin
        Insert(0,'- '+FileDescription+' -');
        Insert(0,'');
        Insert(0,'Version '+FileVersion);
        Insert(0,ProductName);
        Insert(Count-2,Today);
      end;
    end;
  finally
    oVersion.Free;
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

end.

