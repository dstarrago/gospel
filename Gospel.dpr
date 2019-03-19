program Gospel;

uses
  Forms,
  REAbout in 'REABOUT.PAS' {AboutBox},
  REMain in 'REMain.pas' {MainForm},
  ReInit in 'Reinit.pas',
  Fonts in 'Fonts.pas' {FontsDlg},
  FindText in 'FindText.pas' {Find},
  PreferencesDlg in 'PreferencesDlg.pas' {Preferences},
  Parser in 'Parser.pas',
  Kernel in 'Kernel.pas',
  Compiler in 'Compiler.pas',
  Presentation in 'Presentation.pas' {Present},
  GospelMath in 'GospelMath.pas',
  Constants in 'Constants.pas',
  GospelMovement in 'GospelMovement.pas',
  Directories in 'Directories.pas' {DirectoriesForm},
  SourceManage in 'SourceManage.pas',
  Paths in 'Paths.pas' {PathForm},
  PrjManager in 'PrjManager.pas' {ProjectManager},
  AutoCAD in 'AutoCAD.pas',
  GospelAcad in 'GospelAcad.pas',
  DrawForm in 'DrawForm.pas' {DrawingForm},
  GraphSys in 'GraphSys.pas',
  GospelGraphics in 'GospelGraphics.pas',
  GospelBasics in 'GospelBasics.pas',
  Threads in 'Threads.pas',
  GospelEditor in 'GospelEditor.pas';

{$R *.RES}

var
  Present: TPresent;

begin
  Application.Title := 'Gospel Code Editor';
  Present := TPresent.Create(nil);
  Application.CreateForm(TCompilerIDE, CompilerIDE);
  Application.CreateForm(TDirectoriesForm, DirectoriesForm);
  Application.CreateForm(TPreferences, Preferences);
  Application.CreateForm(TFontsDlg, FontsDlg);
  Present.Close;
  Application.Run;
end.
