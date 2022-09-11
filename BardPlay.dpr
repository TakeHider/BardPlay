program BardPlay;

uses
  Windows,
  Vcl.Forms,
  formMain in 'source\formMain.pas' {BardPlayDelphi},
  threadEventProc in 'source\threadEventProc.pas',
  unitMIDIIO in 'source\unitMIDIIO.pas',
  formVersion in 'source\formVersion.pas' {BardPlayVersionInfo},
  Version in 'source\Version.pas';

{$R *.res}
resourcestring
  //アプリケーションのクラス名(ミューテックスに利用)
  //アイコンフォームの名前に先頭にTを付けたもの
  cAppClass = 'TBardPlay';

var
  hMutex : THandle;   //アトム識別子(二重起動の防止用)


begin
  // ミューテックスをオープンしてみる(二重起動の防止)
  hMutex := OpenMutex(MUTEX_ALL_ACCESS, False, PChar(cAppClass));
  //もしミューテックスが既に作成されているときは？？
  if hMutex <> 0 then
  begin
    //ミューテックスを閉じる
    CloseHandle(hMutex);
  end
  else
  begin
    //ミューテックスを作成
    hMutex := CreateMutex(nil, False, PChar(cAppClass));
    //後は通常に起動
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'Bard Play';
    Application.CreateForm(TBardPlayDelphi, BardPlayDelphi);
  Application.Run;
    // ミューテックスを開放
    ReleaseMutex(hMutex);
  end;

end.
