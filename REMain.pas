unit REMain;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Menus, ComCtrls, ClipBrd,
  ToolWin, ActnList, ImgList, Parser, Kernel, Compiler,
  Constants, SourceManage, GospelEditor, mwCustomEdit, mwHighlighter,
  mwGeneralSyn;

const
  FilterAllExtentions  = 'Gospel Application File (*.GAF)|*.gaf|Gospel file (*.GOS)|*.gos|Text Files (*.TXT)|*.txt';
  FilterApplication    = 'Gospel Application File (*.GAF)|*.gaf';
  FilterModule         = 'Gospel file (*.GOS)|*.gos';

type
  TCompilerIDE = class(TForm)
      MainMenu: TMainMenu;
      NewModuleItem: TMenuItem;
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
      Application1: TMenuItem;
      Syntaxcheck1: TMenuItem;
      Compile1: TMenuItem;
      CompileStatus: TPanel;
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
      ConsoleContainer: TGroupBox;
      Console: TListBox;
      View1: TMenuItem;
      Symbols1: TMenuItem;
      Run1: TMenuItem;
      Run3: TMenuItem;
      N3: TMenuItem;
      Options2: TMenuItem;
      Stepover1: TMenuItem;
      TraceInto1: TMenuItem;
      RuntoCursor1: TMenuItem;
      Pause1: TMenuItem;
      Reset1: TMenuItem;
      NewApplication1: TMenuItem;
      Restore: TAction;
      Next: TAction;
      Previous: TAction;
      N5: TMenuItem;
      OpenProject1: TMenuItem;
      N6: TMenuItem;
      SaveAll1: TMenuItem;
      Close1: TMenuItem;
      CloseAll1: TMenuItem;
      N7: TMenuItem;
      CodeContainer: TPageControl;
      ProjectManager1: TMenuItem;
      AddtoProject1: TMenuItem;
      RemovefromProject1: TMenuItem;
      N8: TMenuItem;
      ToolButton2: TToolButton;
      ToolButton3: TToolButton;
      ToolButton4: TToolButton;
      ImageList1: TImageList;
      BreakPoint: TAction;
      procedure FormCreate(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
      procedure ShowHint(Sender: TObject);
      procedure NewModule1Click(Sender: TObject);
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
      procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
      procedure FormShow(Sender: TObject);
      procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
      procedure Find1Click(Sender: TObject);
      procedure Preferences1Click(Sender: TObject);
      procedure BlocksClick(Sender: TObject);
      procedure Syntaxcheck1Click(Sender: TObject);
      procedure Compile1Click(Sender: TObject);
      procedure Run2Click(Sender: TObject);
      procedure Symbols1Click(Sender: TObject);
      procedure Pause1Click(Sender: TObject);
      procedure Reset1Click(Sender: TObject);
      procedure EndOfRunnig(Sender: TObject);
      procedure Options2Click(Sender: TObject);
      procedure TraceInto1Click(Sender: TObject);
      procedure NewApplication1Click(Sender: TObject);
      procedure RestoreExecute(Sender: TObject);
      procedure NextExecute(Sender: TObject);
      procedure PreviousExecute(Sender: TObject);
      procedure SaveAll1Click(Sender: TObject);
      procedure Close1Click(Sender: TObject);
      procedure CloseAll1Click(Sender: TObject);
      procedure OpenProject1Click(Sender: TObject);
      procedure ProjectManager1Click(Sender: TObject);
      procedure Stepover1Click(Sender: TObject);
      procedure FormActivate(Sender: TObject);
      procedure BreakPointExecute(Sender: TObject);
      procedure RuntoCursor1Click(Sender: TObject);
    private
      CurrentBlock: TBlock;
      SourceManager: TSourcemanager;
      FFileName: string;
      ReadyToRun: boolean;
      function GetEditor: TCodeEdit;
      procedure CheckFileSave;
      procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
      procedure SetModified(Value: Boolean);
      procedure EditorMoveCursor(Sender: TObject; const X, Y: integer);
      procedure ShowExecutionBreak(const theLine: integer);
      procedure HideExecutionBreak(const theLine: integer);
      procedure SaveAs(anEditor: TCodeEdit);
      procedure Save(anEditor: TCodeEdit);
      procedure ListView1DblClick(Sender: TObject);
      procedure DoRun;
    public
      GospelCompiler: TGospelCompiler;
      function Compile: boolean;
      procedure ShowSymbols;
      procedure ShowBlocks;
      procedure Run;
      property Editor: TCodeEdit read GetEditor;
    end;

var
  CompilerIDE: TCompilerIDE;

implementation

uses REAbout, RichEdit, ShellAPI, ReInit, Fonts, FindText, PreferencesDlg,
     Directories, PrjManager;

{$R *.DFM}

  procedure TCompilerIDE.CheckFileSave;
    var
      SaveResp: Integer;
    begin
      if Editor.Modified
        then
          begin
            SaveResp := MessageDlg(Format('Save changes to %s?', [SourceManager.ActiveModule]),
              mtConfirmation, mbYesNoCancel, 0);
            case SaveResp of
              idYes: FileSave(Self);
              idNo: {Nothing};
              idCancel: Abort;
            end;
          end;
    end;

{ Event Handlers }

procedure TCompilerIDE.FormCreate(Sender: TObject);
var
  ScreenLogPixels: Integer;
  DC: HDC;
begin
  Application.ProcessMessages;
  Sourcemanager := TSourceManager.Create(CodeContainer, EditorMoveCursor);
  GospelCompiler := TGospelCompiler.Create(ShowExecutionBreak, HideExecutionBreak, SourceManager);
  GospelCompiler.Console := Console;
  GospelCompiler.OnFinishRuning := EndOfRunnig;
  Application.OnHint := ShowHint;
  try
    Sourcemanager.FontName := 'Courier New';
    Sourcemanager.FontSize := 10;
  except
    Sourcemanager.FontName := FontsDlg.FontName.Items[0];
    DC := GetDC(0);
    ScreenLogPixels := GetDeviceCaps(DC, LOGPIXELSY);
    Sourcemanager.FontSize := -MulDiv(DefFontData.Height, 72, ScreenLogPixels);
  end;
  ProjectManager := TProjectManager.Create(Application);
  ProjectManager.ListView1.OnDblClick := ListView1DblClick;
  SourceManager.NewApplication;
  Panel1.Visible := Symbols1.Checked; 
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

  procedure TCompilerIDE.NewModule1Click(Sender: TObject);
    begin
      SourceManager.NewModule;
    end;

  procedure TCompilerIDE.FileOpen(Sender: TObject);
    begin
      OpenDialog.Filter := FilterAllExtentions;
      OpenDialog.DefaultExt := IncludeExt;
      OpenDialog.FilterIndex := 2;
      if OpenDialog.Execute
        then
          begin
            if ExtractFileExt(OpenDialog.FileName) = ApplicationExt
              then
                begin
                  CloseAll1Click(Self);
                  SourceManager.OpenApplication(OpenDialog.FileName);
                  ReadyToRun := false;
                end
              else SourceManager.OpenFile(OpenDialog.FileName);
            OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
            SaveDialog.InitialDir := OpenDialog.InitialDir;
            Editor.ReadOnly := ofReadOnly in OpenDialog.Options;
          end;
    end;

  procedure TCompilerIDE.FileSave(Sender: TObject);
    begin
      Save(Editor);
    end;

  procedure TCompilerIDE.FileSaveAs(Sender: TObject);
    begin
      SaveAs(Editor);
    end;

  procedure TCompilerIDE.FilePrint(Sender: TObject);
    begin
      {
      if PrintDialog.Execute
        then Editor.Print(FFileName);
      }
    end;

  procedure TCompilerIDE.FileExit(Sender: TObject);
    begin
      Close;
    end;

  procedure TCompilerIDE.EditUndo(Sender: TObject);
    begin
      with Editor do
        if HandleAllocated
          then SendMessage(Handle, EM_UNDO, 0, 0);
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
      FontSize.Text := IntToStr(Editor.Font.Size);
      FontName.Text := Editor.Font.Name;
      if ShowModal = mrOk
        then
          begin
            SourceManager.FontName := FontName.Items[FontName.ItemIndex];
            SourceManager.FontSize := StrToInt(FontSize.Text);
          end;
    end;
  Editor.SetFocus;
end;

  procedure TCompilerIDE.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    begin
      try
        //CloseAll1Click(Self);
        SaveAll1Click(Self);
      except
        CanClose := False;
      end;
    end;

procedure TCompilerIDE.FormShow(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  Editor.SetFocus;
  { Check if we should load a file from the command line }
  if (ParamCount > 0) and FileExists(ParamStr(1)) then
    SourceManager.OpenFile(ParamStr(1));
end;

  procedure TCompilerIDE.WMDropFiles(var Msg: TWMDropFiles);
    var
      CFileName: array[0..MAX_PATH] of Char;
    begin
      try
        if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0
          then
            begin
              SourceManager.OpenFile(CFileName);
              Msg.Result := 0;
            end;
      finally
        DragFinish(Msg.Drop);
      end;
    end;

  procedure TCompilerIDE.SetModified(Value: Boolean);
    begin
      if Value
        then
          begin
            StatusBar.Panels[1].Text := 'Modified';
            //CompileStatus.Visible := false;
            ReadyToRun := false;
          end
        else StatusBar.Panels[1].Text := '';
    end;

  procedure TCompilerIDE.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    begin
     { Update the status of the edit commands }
      if Editor <> nil
        then
          begin
            EditCutCmd.Enabled := Editor.SelAvail;
            EditCopyCmd.Enabled := EditCutCmd.Enabled;
            SaveButton.Enabled := Editor.Modified;
            SetModified(Editor.Modified);
            if Editor.HandleAllocated
              then
                begin
                  EditUndoCmd.Enabled := Editor.CanUndo;
                  EditPasteCmd.Enabled := Editor.CanRedo;
                end;
          end;
  end;

  procedure TCompilerIDE.EditorMoveCursor(Sender: TObject; const X, Y: integer);
    begin
      if Sender is TCodeEdit
        then
          with Sender as TCodeEdit do
            StatusBar.Panels[0].Text := Format('Line: %3d   Col: %3d', [Y, X]);
    end;

  procedure TCompilerIDE.Find1Click(Sender: TObject);
    var
      FoundAt: LongInt;                     //Ver la UNit Commdlg.pas en Rtl\Win y Dialogos
      StartPos, ToEnd: integer;
      ST: TSearchTypes;
    begin
      {
      if Find.ShowModal = mrOk
        then
          with Editor do
            begin
              if Find.Scope.ItemIndex = scSelected_Text
                then
                  begin
                    Startpos := GetSelStart;
                    ToEnd    := GetSelEnd;
                  end
                else
                  begin
                    if Find.Origin.ItemIndex = orFrom_Cursor
                      then StartPos := GetSelStart
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
            end;}
    end;

  procedure TCompilerIDE.Preferences1Click(Sender: TObject);
    begin
      if Preferences.ShowModal = mrOk
        then
          with Editor do
            begin
              {
              if Preferences.AutoIndentMode.Checked
                then Options := Options + [eoAutoIdentMode]
                else Options := Options - [eoAutoIdentMode];
              if Preferences.BackspaceUnindent.Checked
                then Options := Options + [eoBackspaceUnindents]
                else Options := Options - [eoBackspaceUnindents];
              }
              if Preferences.SmartTab.Checked
                then SmartTab := true
                else SmartTab := false;
              if Preferences.InsertMode.Checked
                then InsertMode := true
                else InsertMode := false;
            end;
    end;

  function TCompilerIDE.Compile: boolean;
    begin
      Result := GospelCompiler.Compile;
      if Result
        then
          begin
            CompileStatus.Font.Color := clBlack;
            CompileStatus.Caption := GospelCompiler.CompilerMessage;
            ReadyToRun := true;
          end
        else
          begin
            CompileStatus.Font.Color := clMaroon;
            CompileStatus.Caption := 'Error: ' + GospelCompiler.CompilerMessage;
            CompileStatus.Visible := true;
          end;
      CurrentBlock := SystemBlock;
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
      Tokens.Clear;
      try
        for i := 0 to pred(CurrentBlock.Types.Count) do
          Types.Items.Add(CurrentBlock.Types[i].Name);
      except
        Types.Items.Add('Can''t show all items');
      end;
      try
        for i := 0 to pred(CurrentBlock.Variables.Count) do
          Variables.Items.Add(Format('%s : %s',
            [CurrentBlock.Variables[i].Name,
            TData(CurrentBlock.Variables[i]).DataType.Name]));
      except
        Variables.Items.Add('Can''t show all items');
      end;
      try
        for i := 0 to pred(CurrentBlock.Constants.Count) do
          Constants.Items.Add(Format('%s : %s',
            [CurrentBlock.Constants[i].Name,
            TData(CurrentBlock.Constants[i]).DataType.Name]));
      except
        Constants.Items.Add('Can''t show all items');
      end;
      try
        for i := 0 to pred(CurrentBlock.Procedures.Count) do
          Procedures.Items.Add(CurrentBlock.Procedures[i].Name);
      except
        Procedures.Items.Add('Can''t show all items');
      end;
      try
        for i := 0 to pred(CurrentBlock.Functions.Count) do
          Functions.Items.Add(CurrentBlock.Functions[i].Name);
      except
        Functions.Items.Add('Can''t show all items');
      end;
      try
        for i := 0 to pred(GospelCompiler.Parser.Scanner.TokenList.Count) do
          Tokens.Items.Add(GospelCompiler.Parser.Scanner.TokenList[i]);
      except
        Tokens.Items.Add('Can''t show all items');
      end;
    end;

  procedure TCompilerIDE.ShowBlocks;
    var
      i: integer;
    begin
      try
        Blocks.Clear;
        for i := 0 to pred(GospelCompiler.BlockList.Count) do
          Blocks.Items.Add(TBlock(GospelCompiler.BlockList.Items[i]).Name);
      except
        Blocks.Items.Add('Can''t show all items');
      end;
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
      Sourcemanager.free;
    end;

  procedure TCompilerIDE.Syntaxcheck1Click(Sender: TObject);
    begin
      Compile;
    end;

  procedure TCompilerIDE.Compile1Click(Sender: TObject);
    begin
      Compile;
      CompileStatus.Visible := true;
    end;

  procedure TCompilerIDE.Run2Click(Sender: TObject);
    begin
      GospelCompiler.Runner.Breaking := false;
      GospelCompiler.Runner.FStepOver := false;
      DoRun;
    end;

  procedure TCompilerIDE.Run;
    begin
      CompileStatus.Visible := false;
      Pause1.Enabled := true;
      Reset1.Enabled := true;
      Run3.Enabled := false;
      ToolButton3.Enabled := false;
      ToolButton4.Enabled := true;
      if not GospelCompiler.Runner.Runing
        then
          begin
            Console.Items.Clear;
            SourceManager.CheckBreakPoint;
          end;
      GospelCompiler.Run;
    end;

  procedure TCompilerIDE.Symbols1Click(Sender: TObject);
    begin
      Symbols1.Checked := not Symbols1.Checked;
      Panel1.Visible := Symbols1.Checked;
    end;

  procedure TCompilerIDE.Pause1Click(Sender: TObject);
    begin
      Pause1.Enabled := false;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      GospelCompiler.Pause;
    end;

  procedure TCompilerIDE.Reset1Click(Sender: TObject);
    begin
      Pause1.Enabled := false;
      Reset1.Enabled := false;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      GospelCompiler.Reset;
    end;

  procedure TCompilerIDE.EndOfRunnig(Sender: TObject);
    begin
      Pause1.Enabled := false;
      Reset1.Enabled := false;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      SourceManager.RemoveCodeMarks;
    end;

  procedure TCompilerIDE.Options2Click(Sender: TObject);
    begin
      DirectoriesForm.SearchPath.Text := SourceManager.SearchPath;
      DirectoriesForm.OutputDirectory.Text := SourceManager.OutputDirectory;
      if DirectoriesForm.ShowModal = mrOk
        then
          begin
            SourceManager.SearchPath := DirectoriesForm.SearchPath.Text;
            SourceManager.OutputDirectory := DirectoriesForm.OutputDirectory.Text;
          end;
    end;

  procedure TCompilerIDE.TraceInto1Click(Sender: TObject);
    begin
      DoRun;
      Pause1.Enabled := false;
      Reset1.Enabled := true;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      GospelCompiler.Runner.TraceInto;
    end;

  procedure TCompilerIDE.ShowExecutionBreak(const theLine: integer);
    var
      mark: TMark;
    begin
      with Editor do
        begin
          mark := TMark.Create(Editor);
          with mark do
            begin
              Line := theLine;
              Column := CaretX;
              ImageIndex := imExecution;
              IsBookmark := false;
              Visible := true;
              ForeGround := clWhite;
              BackGround := clNavy;
              PaintLine  := true;
              InternalImage := false;
            end;
          Marks.Place(mark);
        end;
      Pause1.Enabled := false;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
    end;

  procedure TCompilerIDE.HideExecutionBreak(const theLine: integer);
    var
      Marks: TMarks;
      i: integer;
    begin
      Editor.Marks.GetMarksForLine(theLine, Marks);
      i := 1;
      while Marks[i] <> nil do inc(i);
      if Marks[pred(i)] <> nil
        then Editor.Marks.Remove(Marks[pred(i)]);
    end;

  function TCompilerIDE.GetEditor: TCodeEdit;
    begin
      Result := SourceManager.ActiveEditor;
    end;

  procedure TCompilerIDE.NewApplication1Click(Sender: TObject);
    begin
      CloseAll1Click(Self);
      SourceManager.NewApplication;
    end;

  procedure TCompilerIDE.RestoreExecute(Sender: TObject);
    begin
      if WindowState = wsMaximized
        then WindowState := wsNormal
        else WindowState := wsMaximized;
    end;

  procedure TCompilerIDE.NextExecute(Sender: TObject);
    begin
      CodeContainer.SelectNextPage(true);
    end;

  procedure TCompilerIDE.PreviousExecute(Sender: TObject);
    begin
      CodeContainer.SelectNextPage(false);
    end;

  procedure TCompilerIDE.SaveAll1Click(Sender: TObject);
    var
      i: integer;
    begin
      for i := 0 to pred(SourceManager.EditCount) do
        if SourceManager.Editor[i].Modified
          then Save(SourceManager.Editor[i]);
    end;

  procedure TCompilerIDE.Close1Click(Sender: TObject);
    begin
      if SourceManager.EditCount > 0
        then
          begin
            CheckFileSave;
            SourceManager.CloseActiveModule;
            if SourceManager.EditCount = 0
              then ProjectManager.Clear;
          end;
    end;

  procedure TCompilerIDE.CloseAll1Click(Sender: TObject);
    var
      SaveResp: Integer;
      i: integer;
    begin
      if SourceManager.HasApplication
        then
          begin
            if SourceManager.ApplicationModified
              then
                begin
                  SaveResp := MessageDlg(Format('Save changes to project %s?', [SourceManager.AppName]),
                    mtConfirmation, mbYesNoCancel, 0);
                  case SaveResp of
                    idYes: SaveAll1Click(Self);
                    idNo: {Nothing};
                    idCancel: Abort;
                  end;
                end;
            SourceManager.CloseAll;
            ProjectManager.Clear;
          end
        else
          for i := 1 to SourceManager.EditCount do
            Close1Click(Self);
    end;

  procedure TCompilerIDE.SaveAs(anEditor: TCodeEdit);
    begin
      if SourceManager.ActiveModule = SourceManager.AppName
        then
          begin
            SaveDialog.Filter := FilterApplication;
            SaveDialog.DefaultExt := ApplicationExt;
          end
        else
          begin
            SaveDialog.Filter := FilterAllExtentions;
            SaveDialog.DefaultExt := IncludeExt;
            SaveDialog.FilterIndex := 2;
          end;
      SaveDialog.FileName := SourceManager.ActiveModule;
      if SaveDialog.Execute
        then
          begin
            if FileExists(SaveDialog.FileName)
              then
                if MessageDlg(Format('OK to overwrite %s', [SaveDialog.FileName]),
                  mtConfirmation, mbYesNoCancel, 0) <> idYes
                  then Exit;
            SourceManager.Save(anEditor, SaveDialog.FileName);
            SaveDialog.InitialDir := ExtractFileDir(SaveDialog.FileName);
          end;
    end;

  procedure TCompilerIDE.Save(anEditor: TCodeEdit);
    begin
      if not Editor.Filed
        then SaveAs(anEditor)
        else SourceManager.Save(anEditor, anEditor.FileName);
    end;

  procedure TCompilerIDE.OpenProject1Click(Sender: TObject);
    begin
      OpenDialog.Filter := FilterApplication;
      OpenDialog.DefaultExt := ApplicationExt;
      OpenDialog.FilterIndex := 1;
      if OpenDialog.Execute
        then
          begin
            CloseAll1Click(Self);
            SourceManager.OpenApplication(OpenDialog.FileName);
            OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
            SaveDialog.InitialDir := OpenDialog.InitialDir;
            Editor.ReadOnly := ofReadOnly in OpenDialog.Options;
            ReadyToRun := false;
          end;
    end;

  procedure TCompilerIDE.ProjectManager1Click(Sender: TObject);
    begin
      ProjectManager.Show;
    end;

  procedure TCompilerIDE.ListView1DblClick(Sender: TObject);
    begin
      SourceManager.OpenFile(ProjectManager.FileNameSelected);
    end;

  procedure TCompilerIDE.Stepover1Click(Sender: TObject);
    begin
      DoRun;
      Pause1.Enabled := false;
      Reset1.Enabled := true;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      GospelCompiler.Runner.StepOver;
    end;

  procedure TCompilerIDE.DoRun;
    begin
      if ReadyToRun
        then Run
        else
          if Compile
            then Run;
    end;

  procedure TCompilerIDE.FormActivate(Sender: TObject);
    begin
      EditorMoveCursor(Editor, Editor.CaretX, Editor.CaretY);
    end;

  procedure TCompilerIDE.BreakPointExecute(Sender: TObject);
    begin
      SourceManager.PutBreakPoint;
    end;

  procedure TCompilerIDE.RuntoCursor1Click(Sender: TObject);
    begin
      DoRun;
      Pause1.Enabled := false;
      Reset1.Enabled := true;
      Run3.Enabled := true;
      ToolButton3.Enabled := true;
      ToolButton4.Enabled := false;
      GospelCompiler.Runner.RunToLine(Editor.CaretY);
    end;

end.




