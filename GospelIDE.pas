unit GospelIDE;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
	Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ClipBrd,
	ToolWin, ActnList, ImgList, CodeEdit, Parser, GSystem, Compiler;

type
		TCompilerIDE = class(TForm)
      MainMenu: TMainMenu;
      FileNewItem: TMenuItem;
      FileOpenItem: TMenuItem;
      FileSaveItem: TMenuItem;
      FileSaveAsItem: TMenuItem;
      FilePrintItem: TMenuItem;
      FileExitItem: TMenuItem;
      EditUndoItem: TMenuItem;
      EditCutItem: TMenuItem;
      EditCopyItem: TMenuItem;
      EditPasteItem: TMenuItem;
      HelpAboutItem: TMenuItem;
      OpenDialog: TOpenDialog;
      SaveDialog: TSaveDialog;
      PrintDialog: TPrintDialog;
      StatusBar: TStatusBar;
      StandardToolBar: TToolBar;
      OpenButton: TToolButton;
      SaveButton: TToolButton;
      PrintButton: TToolButton;
      ToolButton5: TToolButton;
      UndoButton: TToolButton;
      CutButton: TToolButton;
			CopyButton: TToolButton;
      PasteButton: TToolButton;
      ToolbarImages: TImageList;
      ToolButton1: TToolButton;
      EditCutCmd: TAction;
      EditCopyCmd: TAction;
      EditPasteCmd: TAction;
      EditUndoCmd: TAction;
      EditFontCmd: TAction;
      Search1: TMenuItem;
      Find1: TMenuItem;
      Replace1: TMenuItem;
      SearchAgain1: TMenuItem;
      GotolineNumber1: TMenuItem;
      Options1: TMenuItem;
      Fonts1: TMenuItem;
      SelectAll1: TMenuItem;
      Redo1: TMenuItem;
      Reopen1: TMenuItem;
      Preferences1: TMenuItem;
      Content1: TMenuItem;
      Tools1: TMenuItem;
      ConfigureTools1: TMenuItem;
      ActionList1: TActionList;
			Run1: TMenuItem;
      Syntaxcheck1: TMenuItem;
      Compile1: TMenuItem;
      Run2: TMenuItem;
      StatusBar1: TStatusBar;
      Panel6: TPanel;
      Panel1: TPanel;
      Panel3: TPanel;
      GroupBox1: TGroupBox;
      Types: TListBox;
      GroupBox2: TGroupBox;
      Variables: TListBox;
      GroupBox3: TGroupBox;
      Constants: TListBox;
      GroupBox5: TGroupBox;
      Procedures: TListBox;
      GroupBox6: TGroupBox;
      Functions: TListBox;
      Panel4: TPanel;
      GroupBox4: TGroupBox;
      Tokens: TListBox;
      GroupBox7: TGroupBox;
      Blocks: TListBox;
      GroupBox8: TGroupBox;
			Console: TListBox;
      CodeContainer: TGroupBox;
      View1: TMenuItem;
      Symbols1: TMenuItem;

      procedure SelectionChange(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure ShowHint(Sender: TObject);
      procedure FileNew(Sender: TObject);
      procedure FileOpen(Sender: TObject);
      procedure FileSave(Sender: TObject);
      procedure FileSaveAs(Sender: TObject);
      procedure FilePrint(Sender: TObject);
      procedure FileExit(Sender: TObject);
      procedure EditUndo(Sender: TObject);
      procedure EditCut(Sender: TObject);
      procedure EditCopy(Sender: TObject);
      procedure EditPaste(Sender: TObject);
      procedure HelpAbout(Sender: TObject);
      procedure SelectFont(Sender: TObject);
      procedure FormResize(Sender: TObject);
      procedure FormPaint(Sender: TObject);
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure FormShow(Sender: TObject);
			procedure RichEditChange(Sender: TObject);
      procedure ActionList1Update(Action: TBasicAction;
        var Handled: Boolean);
      procedure Find1Click(Sender: TObject);
      procedure Preferences1Click(Sender: TObject);
      procedure BlocksClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure Syntaxcheck1Click(Sender: TObject);
      procedure Compile1Click(Sender: TObject);
      procedure Run2Click(Sender: TObject);
      procedure Symbols1Click(Sender: TObject);
    private
      CurrentBlock: TBlock;
      ReadyToRun: boolean;
    private
      FFileName: string;
      FUpdating: Boolean;
      function CurrText: TTextAttributes;
      procedure SetFileName(const FileName: String);
      procedure CheckFileSave;
      procedure SetEditRect;
      procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
      procedure PerformFileOpen(const AFileName: string);
      procedure SetModified(Value: Boolean);
			procedure EditorMoveCursor(Sender: TObject; const Col, Row: longint);
		public
			Editor: TCodeEdit;
			GospelCompiler: TGospelCompiler;
			function Compile: boolean;
			procedure ShowSymbols;
			procedure ShowBlocks;
			procedure Run;
	end;

var
	CompilerIDE: TCompilerIDE;

implementation

uses REAbout, RichEdit, ShellAPI, ReInit, Fonts, FindText,
		 PreferencesDlg;

const
	GutterWid = 6;

{$R *.DFM}

procedure TCompilerIDE.SelectionChange(Sender: TObject);
begin
  with Editor.Paragraph do
  try
    FUpdating := True;
  finally
    FUpdating := False;
    ShowCaret(Editor.Handle);
  end;
end;

procedure TCompilerIDE.SetFileName(const FileName: String);
begin
  FFileName := FileName;
  Caption := Format('%s - %s', [ExtractFileName(FileName), Application.Title]);
end;

procedure TCompilerIDE.CheckFileSave;
var
  SaveResp: Integer;
begin
  if not Editor.Modified then Exit;
  SaveResp := MessageDlg(Format('Save changes to %s?', [FFileName]),
    mtConfirmation, mbYesNoCancel, 0);
	case SaveResp of
    idYes: FileSave(Self);
    idNo: {Nothing};
    idCancel: Abort;
  end;
end;

procedure TCompilerIDE.SetEditRect;
var
  R: TRect;
begin
  with Editor do
  begin
    R := Rect(GutterWid, 0, ClientWidth-GutterWid, ClientHeight);
    SendMessage(Handle, EM_SETRECT, 0, Longint(@R));
  end;
end;

  function TCompilerIDE.CurrText: TTextAttributes;
    begin
      if Editor.SelLength > 0 then Result := Editor.SelAttributes
      else Result := Editor.DefAttributes;
    end;

{ Event Handlers }

procedure TCompilerIDE.FormCreate(Sender: TObject);
var
  ScreenLogPixels: Integer;
  DC: HDC;
begin
  Editor := TCodeEdit.Create(Self);
  Editor.Parent := CodeContainer;
  Editor.Align := alClient;
  Editor.OnMoveCaret := EditorMoveCursor;
  Editor.OnChange := RichEditChange;
  GospelCompiler := TGospelCompiler.Create;
  GospelCompiler.Console := Console;
  Application.OnHint := ShowHint;
  OpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
  SaveDialog.InitialDir := OpenDialog.InitialDir;
  SetFileName('Untitled');
  SelectionChange(Self);
  try
    CurrText.Name := 'Courier New';
    CurrText.Size := 10;
  except
    CurrText.Name := FontsDlg.FontName.Items[0];
		DC := GetDC(0);
    ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
    CurrText.Size := -MulDiv(DefFontData.Height, 72, ScreenLogPixels);
  end;
end;

procedure TCompilerIDE.ShowHint(Sender: TObject);
begin
  if Length(Application.Hint) > 0 then
  begin
    StatusBar.SimplePanel := True;
    StatusBar.SimpleText := Application.Hint;
  end
  else StatusBar.SimplePanel := False;
end;

procedure TCompilerIDE.FileNew(Sender: TObject);
begin
  SetFileName('Untitled');
  Editor.Lines.Clear;
  Editor.Modified := False;
  SetModified(False);
end;

procedure TCompilerIDE.PerformFileOpen(const AFileName: string);
begin
  Editor.Lines.LoadFromFile(AFileName);
  SetFileName(AFileName);
  Editor.SetFocus;
  Editor.Modified := False;
  SetModified(False);
end;

procedure TCompilerIDE.FileOpen(Sender: TObject);
begin
  CheckFileSave;
  if OpenDialog.Execute then
  begin
    PerformFileOpen(OpenDialog.FileName);
    Editor.ReadOnly := ofReadOnly in OpenDialog.Options;
  end;
end;

procedure TCompilerIDE.FileSave(Sender: TObject);
begin
  if FFileName = 'Untitled' then
    FileSaveAs(Sender)
  else
	begin
    Editor.Lines.SaveToFile(FFileName);
    Editor.Modified := False;
    SetModified(False);
  end;
end;

procedure TCompilerIDE.FileSaveAs(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
		if FileExists(SaveDialog.FileName) then
			if MessageDlg(Format('OK to overwrite %s', [SaveDialog.FileName]),
				mtConfirmation, mbYesNoCancel, 0) <> idYes then Exit;
		Editor.Lines.SaveToFile(SaveDialog.FileName);
		SetFileName(SaveDialog.FileName);
		Editor.Modified := False;
		SetModified(False);
	end;
end;

procedure TCompilerIDE.FilePrint(Sender: TObject);
begin
	if PrintDialog.Execute then
		Editor.Print(FFileName);
end;

procedure TCompilerIDE.FileExit(Sender: TObject);
begin
  Close;
end;

procedure TCompilerIDE.EditUndo(Sender: TObject);
begin
  with Editor do
    if HandleAllocated then SendMessage(Handle, EM_UNDO, 0, 0);
end;

procedure TCompilerIDE.EditCut(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TCompilerIDE.EditCopy(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TCompilerIDE.EditPaste(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TCompilerIDE.HelpAbout(Sender: TObject);
begin
  with TAboutBox.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TCompilerIDE.SelectFont(Sender: TObject);
begin
  with FontsDlg do
    begin
      FontSize.Text := IntToStr(Editor.SelAttributes.Size);
      FontName.Text := Editor.SelAttributes.Name;
      if ShowModal = mrOk
        then
          begin
            CurrText.Size := StrToInt(FontSize.Text);
            CurrText.Name := FontName.Items[FontName.ItemIndex];
          end;
    end;
  Editor.SetFocus;
end;

procedure TCompilerIDE.FormResize(Sender: TObject);
begin
  SetEditRect;
  SelectionChange(Sender);
end;

procedure TCompilerIDE.FormPaint(Sender: TObject);
begin
  SetEditRect;
end;

procedure TCompilerIDE.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  try
    CheckFileSave;
  except
    CanClose := False;
  end;
end;

procedure TCompilerIDE.FormShow(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  RichEditChange(nil);
  Editor.SetFocus;
  { Check if we should load a file from the command line }
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    PerformFileOpen(ParamStr(1));
end;

procedure TCompilerIDE.WMDropFiles(var Msg: TWMDropFiles);
var
  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then
    begin
      CheckFileSave;
      PerformFileOpen(CFileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TCompilerIDE.RichEditChange(Sender: TObject);
begin
  SetModified(Editor.Modified);
end;

procedure TCompilerIDE.SetModified(Value: Boolean);
begin
  if Value
    then
      begin
        StatusBar.Panels[1].Text := 'Modified';
        ReadyToRun := false;
      end
    else StatusBar.Panels[1].Text := '';
end;

procedure TCompilerIDE.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
 { Update the status of the edit commands }
  EditCutCmd.Enabled := Editor.SelLength > 0;
  EditCopyCmd.Enabled := EditCutCmd.Enabled;
  if Editor.HandleAllocated then
  begin
    EditUndoCmd.Enabled := Editor.Perform(EM_CANUNDO, 0, 0) <> 0;
    EditPasteCmd.Enabled := Editor.Perform(EM_CANPASTE, 0, 0) <> 0;
  end;
end;

  procedure TCompilerIDE.EditorMoveCursor(Sender: TObject; const Col, Row: Integer);
    begin
      StatusBar.Panels[0].Text := Format('Line: %3d   Col: %3d', [Row, Col]);
    end;

  procedure TCompilerIDE.Find1Click(Sender: TObject);
    var
      FoundAt: LongInt;                     //Ver la UNit Commdlg.pas en Rtl\Win y Dialogos
      StartPos, ToEnd: integer;
      ST: TSearchTypes;
    begin
      if Find.ShowModal = mrOk
        then
          with Editor do
            begin
              if Find.Scope.ItemIndex = scSelected_Text
                then
                  begin
                    Startpos := SelStart;
                    ToEnd    := SelStart + SelLength;
                  end
                else
                  begin
                    if Find.Origin.ItemIndex = orFrom_Cursor
                      then StartPos := SelStart
                      else StartPos := 0;
                    ToEnd := Length(Text);
                  end;
              ST := [];
              if Find.MatchCase.Checked
                then ST := ST + [stMatchCase];
              if Find.WholeWords.Checked
                then ST := ST + [stWholeWord];
              FoundAt := FindText(Find.Text.Text, StartPos, ToEnd, ST);
              if FoundAt <> -1
                then
                  begin
                    SetFocus;
                    SelStart := FoundAt;
                    SelLength := Length(Find.Text.Text);
                  end
                else
                  begin
                    MessageDlg(Format('Search string ''%s'' not found',
                     [Find.Text.Text]), mtInformation, [mbOk], 0);
                    if SelLength > 0
                      then SelLength := 0;
                  end;
            end;
    end;

  procedure TCompilerIDE.Preferences1Click(Sender: TObject);
    begin
      if Preferences.ShowModal = mrOk
        then
          with Editor do
            begin
              if Preferences.AutoIndentMode.Checked
                then Options := Options + [eoAutoIdentMode]
                else Options := Options - [eoAutoIdentMode];
              if Preferences.BackspaceUnindent.Checked
                then Options := Options + [eoBackspaceUnindents]
                else Options := Options - [eoBackspaceUnindents];
              if Preferences.SmartTab.Checked
                then Options := Options + [eoSmartTab]
                else Options := Options - [eoSmartTab];
              if Preferences.InsertMode.Checked
                then Options := Options + [eoInsertMode]
                else Options := Options - [eoInsertMode];
            end;
    end;

function TCompilerIDE.Compile: boolean;
  begin
    GospelCompiler.Source := Editor.Text;
    Result := GospelCompiler.Compile;
    Statusbar1.SimpleText := GospelCompiler.CompilerMessage;
    CurrentBlock := GospelCompiler.GlobalBlock;
    ShowBlocks;
    ShowSymbols;
  end;

  procedure TCompilerIDE.ShowSymbols;
    var
      i: integer;
    begin
      Types.Clear;
      Variables.Clear;
      Constants.Clear;
      Procedures.Clear;
      Functions.Clear;
      for i := 0 to pred(CurrentBlock.Types.Count) do
        Types.Items.Add(CurrentBlock.Types[i].Name);
      for i := 0 to pred(CurrentBlock.Variables.Count) do
        Variables.Items.Add(Format('%s : %s',
          [CurrentBlock.Variables[i].Name,
          TData(CurrentBlock.Variables[i]).DataType.Name]));
      for i := 0 to pred(CurrentBlock.Constants.Count) do
        Constants.Items.Add(Format('%s : %s',
          [CurrentBlock.Constants[i].Name,
          TData(CurrentBlock.Constants[i]).DataType.Name]));
      for i := 0 to pred(CurrentBlock.Procedures.Count) do
        Procedures.Items.Add(CurrentBlock.Procedures[i].Name);
      for i := 0 to pred(CurrentBlock.Functions.Count) do
        Functions.Items.Add(CurrentBlock.Functions[i].Name);
    end;

  procedure TCompilerIDE.ShowBlocks;
    var
      i: integer;
    begin
      Blocks.Clear;
      for i := 0 to pred(GospelCompiler.BlockList.Count) do
        Blocks.Items.Add(TBlock(GospelCompiler.BlockList.Items[i]).Name);
    end;

  procedure TCompilerIDE.BlocksClick(Sender: TObject);
    var
      Block: TBlock;
    begin
      if Blocks.ItemIndex >= 0
        then
          begin
            Block := TBlock(GospelCompiler.BlockList.Items[Blocks.ItemIndex]);
            if Block <> CurrentBlock
              then
                begin
                  CurrentBlock := Block;
                  ShowSymbols;
                end;
          end;
    end;

  procedure TCompilerIDE.FormDestroy(Sender: TObject);
    begin
      GospelCompiler.free;
    end;

  procedure TCompilerIDE.Syntaxcheck1Click(Sender: TObject);
    begin
      Compile;
    end;

  procedure TCompilerIDE.Compile1Click(Sender: TObject);
    begin
      Compile;
    end;

  procedure TCompilerIDE.Run2Click(Sender: TObject);
    begin
      if ReadyToRun
        then Run
        else
          if Compile
            then Run;
    end;

  procedure TCompilerIDE.Run;
    begin
      GospelCompiler.Interpret;
    end;

  procedure TCompilerIDE.Symbols1Click(Sender: TObject);
    begin
      Symbols1.Checked := not Symbols1.Checked;
      Panel1.Visible := Symbols1.Checked;
    end;

end.




