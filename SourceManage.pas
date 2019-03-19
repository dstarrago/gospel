unit SourceManage;

interface

  uses Classes, SysUtils, Graphics, Controls, Comctrls, GospelEditor,
       mwGeneralSyn, mwCustomEdit;

  const
    IncludeExt          = '.gos';
    ApplicationExt      = '.gaf';
    ProjectExt          = '.gpr';
    DefaultApp          = 'Application1';
    DefaultModule       = 'Module';
    Untitled            = 'Untitled';

    imCodeLine          = 15;
    imBreakpointEnable  = 16;
    imBreakPointInvalid = 17;
    imBreakpointValid   = 18;
    imExecution         = 21;

  type
    ESourceManage = class(Exception);
    TSourceManager =
      class
        private
          FEditionList: TStringList;
          FContainer: TPageControl;
          FOnEditorMoveCaret: TCaretMoveEvent;
          FActiveModule: string;
          FSearchPath: string;
          FOutputDirectory: string;
          FAppName: string;
          FAppDirectory: string;
          FActiveEditor: TCodeEdit;
          FFontSize: integer;
          FFontName: TFontName;
          mwGeneralSyn: TmwGeneralSyn;
          function GetEditor(const i: integer): TCodeEdit;
          procedure SetActiveModule(const Value: string);
          procedure SetAppDirectory(const Value: string);
          procedure SetAppName(const Value: string);
          function NewEditor(const Name: string): TCodeEdit;
          function GetDefaultName: string;
          procedure SetFontName(const Value: TFontName);
          procedure SetFontSize(const Value: integer);
          procedure UpDateEditFonts;
          procedure OnPageChange(Sender: TObject);
          function ExtractFileNameWithoutExt(const FileName: TFileName): string;
          function GetEditCount: integer;
          function GetModule(const i: integer): string;
          procedure SetModule(const i: integer; const Value: string);
          function GetApplicationModified: boolean;
          function BuildFileName(const Dir, Name, Ext: string): string;
          function GetHasApplication: boolean;
          function GetSource(const ModuleName: string): string;
          function FindInclude(const ModuleName: string; var Path: string): boolean;
          procedure TabSheetEnter(Sender: TObject);
          procedure InitilizeSyntaxHighLighter;
          procedure EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
          procedure PutImage(Edit: TCodeEdit; const aLine, Image: integer);
        public
          constructor Create(aContainer: TPageControl; anOnEditorMoveCaret: TCaretMoveEvent);
          destructor Destroy;  override;
          procedure NewModule;
          procedure NewApplication;
          procedure OpenModule(const Name: string);
          procedure OpenApplication(const FileName: TFileName);
          procedure OpenFile(const FileName: TFileName);
          procedure SaveProjectAs(const FileName: TFileName);
          procedure SaveApplication(Editor: TCodeEdit; const FileName: TFileName);
          procedure SaveModule(Editor: TCodeEdit; const FileName: TFileName);
          procedure Save(Editor: TCodeEdit; const FileName: TFileName);
          procedure CloseActiveModule;
          procedure CloseAll;
          function IsOpen(const Name: string): boolean;
          function GetFileOpen(const FileName: TFileName): integer;
          function EditorNumber(const Name: string): integer;
          procedure SaveProjectFile;
          procedure LoadProjectFile;
          procedure MarkCodeLines(Edit: TCodeEdit);
          procedure RemoveCodeMarks;
          procedure CheckBreakPoint;
          procedure PutBreakPoint;
          property Editor[const i: integer]: TCodeEdit read GetEditor;
          property EditingModule[const i: integer]: string read GetModule write SetModule;
          property ActiveModule: string read FActiveModule write SetActiveModule;
          property ActiveEditor: TCodeEdit read FActiveEditor;
          property OnEditorMoveCaret: TCaretMoveEvent read FOnEditorMoveCaret write FOnEditorMoveCaret;
          property SearchPath: string read FSearchPath write FSearchPath;
          property OutputDirectory: string read FOutputDirectory write FOutputDirectory;
          property AppDirectory: string read FAppDirectory write SetAppDirectory;
          property AppName: string read FAppName write SetAppName;
          property FontName: TFontName read FFontName write SetFontName;
          property FontSize: integer read FFontSize write SetFontSize;
          property EditCount: integer read GetEditCount;
          property ApplicationModified: boolean read GetApplicationModified;
          property HasApplication: boolean read GetHasApplication;
          property Source[const ModuleName: string]: string read GetSource;
      end;

implementation

  uses Constants, PrjManager, Parser, ReMain, Kernel;

{ TSourceManager }

  procedure TSourceManager.CloseActiveModule;
    var
      i: integer;
    begin
      i := FEditionList.IndexOf(FContainer.ActivePage.Caption);
      if FContainer.PageCount = 1
        then FContainer.ActivePage := nil;
      TTabSheet(Editor[i].Parent).free;
      FEditionList.Delete(i);
      if FEditionList.Count > 0
        then ActiveModule := FEditionList[FContainer.ActivePage.PageIndex];
      if FContainer.PageCount = 0
        then FActiveEditor := nil;
    end;

  procedure TSourceManager.CloseAll;
    var
      i: integer;
    begin
      for i := 1 to EditCount do
        CloseActiveModule;
      FActiveModule := '';
      FAppName := '';
      FAppDirectory := '';
    end;

  constructor TSourceManager.Create(aContainer: TPageControl; anOnEditorMoveCaret: TCaretMoveEvent);
    begin
      inherited Create;
      FContainer := aContainer;
      FContainer.OnChange := OnPageChange;
      FEditionList := TStringList.Create;
      OnEditorMoveCaret := anOnEditorMoveCaret;
      InitilizeSyntaxHighLighter;
    end;

  destructor TSourceManager.Destroy;
    begin
      FEditionList.free;
      inherited;
    end;

  function TSourceManager.ExtractFileNameWithoutExt(const FileName: TFileName): string;
    begin
      Result := ExtractFileName(FileName);
      Result := Copy(Result, 1, pred(Pos('.', Result)));
    end;

  function TSourceManager.GetApplicationModified: boolean;
    var
      i: integer;
    begin
      i := 0;
      Result := false;
      while (i < EditCount) and not Result do
        begin
          Result := Editor[i].Modified;
          inc(i);
        end;
    end;

  function TSourceManager.GetDefaultName: string;
    var
      i: integer;
    begin
      i := 1;
      while FEditionList.IndexOf(DefaultModule + IntToStr(i)) <> -1
        do inc(i);
      Result := DefaultModule + IntToStr(i);
    end;

  function TSourceManager.GetEditCount: integer;
    begin
      Result := FEditionList.Count;
    end;

  function TSourceManager.GetEditor(const i: integer): TCodeEdit;
    begin
      if FEditionList.Count = 0
        then Result := nil
        else Result := TCodeEdit(FEditionList.Objects[i]);
    end;

  function TSourceManager.GetModule(const i: integer): string;
    begin
      Result := FEditionList[i];
    end;

  function TSourceManager.GetFileOpen(const FileName: TFileName): integer;
    var
      Found: boolean;
    begin
      Found := false;
      Result := 0;
      while (Result < EditCount) and not Found do
        begin
          Found := Editor[Result].FileName = FileName;
          inc(Result);
        end;
      dec(Result);
      if not Found
        then Result := -1;
    end;

  function TSourceManager.IsOpen(const Name: string): boolean;
    begin
      Result := FEditionList.IndexOf(Name) <> -1;
    end;

  procedure TSourceManager.NewApplication;
    var
      Editor: TCodeEdit;
    begin
      FAppName := DefaultApp;
      FAppDirectory := GetCurrentDir;
      Editor := NewEditor(FAppName);
      Editor.FileName := BuildFileName(FAppDirectory, FAppName, ApplicationExt);
      Editor.Lines.Add('program ' + FAppName + ';');
      Editor.Lines.Add('');
      Editor.Lines.Add('begin');
      Editor.Lines.Add('end.');
      Editor.Lines.Add('');
      ProjectManager.Add(FAppDirectory, FAppName, ApplicationExt);
    end;

  function TSourceManager.NewEditor(const Name: string): TCodeEdit;
    var
      TabSheet: TTabSheet;
    begin
      TabSheet := TTabSheet.Create(FContainer);
      TabSheet.Visible := false;
      TabSheet.Caption := Name;
      TabSheet.PageControl := FContainer;
      TabSheet.OnEnter := TabSheetEnter;
      Result := TCodeEdit.Create(TabSheet);
      Result.Parent := TabSheet;
      Result.Align := alClient;
      Result.OnCaretMove := OnEditorMoveCaret;
      Result.Lines.Clear;
      Result.Font.Name := FontName;
      Result.Font.Size := FontSize;
      Result.HighLighter := mwGeneralSyn;
      Result.BookMarkImages := ReMain.CompilerIDE.ImageList1;
      Result.OnMouseUp := EditMouseUp;
      FEditionList.AddObject(Name, Result);
      ActiveModule := Name;
    end;

  procedure TSourceManager.NewModule;
    var
      Editor: TCodeEdit;
      Name: string;
    begin
      Name := GetDefaultName;
      Editor := NewEditor(Name);
      Editor.FileName := BuildFileName(FAppDirectory, Name, IncludeExt);
      Editor.Lines.Add('module ' + Name + ';');
      Editor.Lines.Add('');
      Editor.Lines.Add('begin');
      Editor.Lines.Add('end.');
      Editor.Lines.Add('');
      if HasApplication
        then ProjectManager.Add(FAppDirectory, Name, IncludeExt);
    end;

  procedure TSourceManager.OnPageChange(Sender: TObject);
    begin
      ActiveModule := FEditionList[FContainer.ActivePage.Pageindex];
    end;

  procedure TSourceManager.OpenFile(const FileName: TFileName);
    var
      Editor: TCodeEdit;
      FileIndex: integer;
      Name: string;
    begin
      FileIndex := GetFileOpen(FileName);
      if FileIndex >= 0
        then ActiveModule := EditingModule[FileIndex]
        else
          begin
            Name := ExtractFileNameWithoutExt(FileName);
            Editor := NewEditor(Name);
            try
              Editor.Lines.LoadFromFile(FileName);
              Editor.Modified := False;
              Editor.FileName := FileName;
              Editor.Filed := true;
            except
              Editor.free;
              raise;
            end;
          end;
    end;

  procedure TSourceManager.OpenModule(const Name: string);
    var
      Path: string;
    begin
      if FindInclude(Name, Path)
        then OpenFile(Path)
        else raise ESourceManage.Create(Format(emFileNotFound, [Name]));
    end;

  procedure TSourceManager.Save(Editor: TCodeEdit; const FileName: TFileName);
    begin
      if TTabSheet(Editor.Parent).Caption = FAppName
        then SaveApplication(Editor, FileName)
        else SaveModule(Editor, FileName);
    end;

  procedure TSourceManager.SaveApplication(Editor: TCodeEdit; const FileName: TFileName);
    begin
      FAppName := ExtractFileNameWithoutExt(FileName);
      FAppDirectory := ExtractFileDir(FileName);
      SetCurrentDir(FAppDirectory);
      SaveModule(Editor, FileName);
      SaveProjectFile;
    end;

  procedure TSourceManager.SaveModule(Editor: TCodeEdit; const FileName: TFileName);
    var
      i: integer;
      Name: string;
    begin
      i := FEditionList.IndexOfObject(Editor);
      Name := ExtractFileNameWithoutExt(FileName);
      if Editor.FileName <> FileName
        then ProjectManager.Replace(EditingModule[i] +
             ExtractFileExt(Editor.FileName), ExtractFileDir(FileName),
             Name + ExtractFileExt(FileName));
      Editor.Lines.SaveToFile(FileName);
      EditingModule[i] := Name;
      Editor.FileName := FileName;
      Editor.Modified := false;
      Editor.Filed := true;
    end;

  procedure TSourceManager.SetActiveModule(const Value: string);
    var
      i: integer;
    begin
      i := FEditionList.IndexOf(Value);
      if i >= 0
        then
          begin
            FActiveEditor := Editor[i];
            FContainer.ActivePage := TTabSheet(Editor[i].Parent);
            FActiveModule := Value;
          end;
    end;

  procedure TSourceManager.SetAppDirectory(const Value: string);
    begin
      FAppDirectory := Value;
    end;

  procedure TSourceManager.SetAppName(const Value: string);
    begin
      FAppName := Value;
    end;

  procedure TSourceManager.SetFontName(const Value: TFontName);
    begin
      if Value <> FFontName
        then
          begin
            FFontName := Value;
            UpDateEditFonts;
          end;
    end;

  procedure TSourceManager.SetFontSize(const Value: integer);
    begin
      if Value <> FFontSize
        then
          begin
            FFontSize := Value;
            UpDateEditFonts;
          end;
    end;

  procedure TSourceManager.SetModule(const i: integer; const Value: string);
    begin
      if FEditionList[i] <> Value
        then
          begin
            FEditionList[i] := Value;
            TTabSheet(Editor[i].Parent).Caption := Value;
          end;
    end;

  procedure TSourceManager.UpDateEditFonts;
    var
      i: integer;
    begin
      for i := 0 to pred(FEditionList.Count) do
        begin
          Editor[i].Font.Name := FontName;
          Editor[i].Font.Size := FontSize;
        end;
    end;

  procedure TSourceManager.OpenApplication(const FileName: TFileName);
    begin
      OpenFile(FileName);
      FAppName := ExtractFileNameWithoutExt(FileName);
      FAppDirectory := ExtractFileDir(FileName);
      SetCurrentDir(FAppDirectory);
      LoadProjectFile;
    end;

  function TSourceManager.BuildFileName(const Dir, Name, Ext: string): string;
    begin
      Result := Dir + '\' + Name + Ext;
    end;

  procedure TSourceManager.SaveProjectFile;
    var
      i: integer;
      Data: TStringList;
    begin
      if (ProjectManager.FileCount > 0) and HasApplication
        then
          begin
            Data := TStringList.Create;
            for i := 0 to pred(ProjectManager.FileCount) do
              Data.Add(ProjectManager.FileName[i]);
            Data.SaveToFile(BuildFileName(FAppDirectory, FAppName, ProjectExt));
            Data.free;
          end;
    end;

  procedure TSourceManager.LoadProjectFile;
    var
      i: integer;
      Data: TStringList;
    begin
      Data := TStringList.Create;
      try
        Data.LoadFromFile(BuildFileName(FAppDirectory, FAppName, ProjectExt));
        if Data.Count > 0
          then
            for i := 0 to pred(Data.Count) do
              ProjectManager.Add(ExtractFileDir(Data[i]), ExtractFileNameWithoutExt(Data[i]), ExtractFileExt(Data[i]));
      except
        ProjectManager.Add(FAppDirectory, FAppName, ApplicationExt);
      end;
      Data.free;
    end;

  procedure TSourceManager.SaveProjectAs(const FileName: TFileName);
    var
      Index: integer;
      Data: TStringList;
    begin
      Index := FEditionList.IndexOf(AppName);
      if Index <> -1
        then SaveApplication(Editor[Index], FileName)
        else
          begin
            Data := TStringList.Create;
            Data.LoadFromFile(BuildFileName(AppDirectory, AppName, ApplicationExt));
            if BuildFileName(AppDirectory, AppName, ApplicationExt) <> FileName
              then ProjectManager.Replace(BuildFileName('', AppName, ApplicationExt),
                   ExtractFileDir(FileName), ExtractFileName(FileName));
            FAppName := ExtractFileNameWithoutExt(FileName);
            FAppDirectory := ExtractFileDir(FileName);
            SetCurrentDir(FAppDirectory);
            Data.SaveToFile(FileName);
            SaveProjectFile;
            Data.free;
          end;
    end;

  function TSourceManager.GetHasApplication: boolean;
    begin
      Result := AppName <> '';
    end;

  function TSourceManager.GetSource(const ModuleName: string): string;
    var
      Index: integer;
      S: TStringList;
      Path: string;
    begin
      Index := FEditionList.IndexOf(ModuleName);
      if Index >= 0
        then Result := Editor[Index].Lines.Text
        else
          begin
            S := TStringList.Create;
            try
              Index := ProjectManager.GetModuleIndex(ModuleName);
              if Index >= 0
                then S.LoadFromFile(ProjectManager.FileName[Index])
                else
                  if FindInclude(ModuleName, Path)
                    then S.LoadFromFile(Path)
                    else raise ESourceManage.Create(Format(emFileNotFound, [ModuleName]));
              Result := S.Text;
            finally
              S.free;
            end;
          end;
    end;

  function TSourceManager.EditorNumber(const Name: string): integer;
    begin
      Result := FEditionList.IndexOf(Name);
    end;

  function TSourceManager.FindInclude(const ModuleName: string; var Path: string): boolean;
    var
      Dir: string;
    begin
      Path := '';
      if FileExists(BuildFileName(FAppDirectory, ModuleName, IncludeExt))
        then Path := BuildFileName(FAppDirectory, ModuleName, IncludeExt)
        else
          begin
            Dir := FileSearch(BuildFileName('', ModuleName, IncludeExt), FSearchPath);
            if Dir <> ''
              then Path := BuildFileName(Dir, ModuleName, IncludeExt);
          end;
      Result := Path <> '';
    end;

  procedure TSourceManager.TabSheetEnter(Sender: TObject);
    var
      Ed: TCodeEdit;
    begin
      Ed := TCodeEdit(TTabSheet(Sender).Controls[0]);
      OnEditorMoveCaret(Ed, Ed.CaretX, Ed.CaretY);
      TCodeEdit(TTabSheet(Sender).Controls[0]).SetFocus;
    end;

  procedure TSourceManager.InitilizeSyntaxHighLighter;
    var
      i: integer;
    begin
      mwGeneralSyn := TmwGeneralSyn.Create(FContainer);
      with mwGeneralSyn do
        begin
          for i := 0 to pred(ReservedWords.Count) do
            KeyWords.Add(ReservedWords.Strings[i]);
          NumberAttri.ForeGround := clBlue;
          CommentAttri.ForeGround := clNavy;
          StringAttri.ForeGround := clRed;
          Comments := [csPasStyle, csAnsiStyle];
        end;
    end;

  procedure TSourceManager.PutBreakPoint;
    var
      BreakPoint: TBreakPoint;
      Statement: TStatement;
      Marks: TMarks;
    begin
      BreakPoint := ActiveEditor.BreakPoints.GetBreakPointFromLine(ActiveEditor.CaretY);
      if CompilerIDE.GospelCompiler.Runner.Runing
        then Statement := TStatement(ActiveEditor.Lines.Objects[ActiveEditor.CaretY])
        else Statement := nil;
      if BreakPoint = nil
        then
          begin
            BreakPoint := TBreakPoint.Create(ActiveEditor);
            BreakPoint.Line := ActiveEditor.CaretY;
            ActiveEditor.Marks.GetMarksForLine(BreakPoint.Line, Marks);
            if (Marks[1] <> nil) and (Marks[1].ImageIndex = imCodeLine)
              then Marks[1].Visible := false;
            ActiveEditor.BreakPoints.Add(BreakPoint);
            if CompilerIDE.GospelCompiler.Runner.Runing
              then
                if Statement <> nil
                  then
                    begin
                      Statement.HasBreakPoint := true;
                      BreakPoint.ImageIndex := imBreakpointValid;
                    end
                  else
                    begin
                      BreakPoint.ImageIndex := imBreakpointInvalid;
                      BreakPoint.ForeGround := clLime;
                      BreakPoint.BackGround := clOlive;
                    end
              else BreakPoint.ImageIndex := imBreakpointEnable;
          end
        else              // Remove the BreakPoint
          begin
            ActiveEditor.BreakPoints.Remove(BreakPoint);
            ActiveEditor.Marks.GetMarksForLine(BreakPoint.Line, Marks);
            if (Marks[1] <> nil) and (Marks[1].ImageIndex = imCodeLine)
              then Marks[1].Visible := true;
            BreakPoint.free;
            if CompilerIDE.GospelCompiler.Runner.Runing and (Statement <> nil)
              then Statement.HasBreakPoint := false;
          end;
    end;

  procedure TSourceManager.EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    begin
      if X < ActiveEditor.GutterWidth
        then PutBreakPoint;
    end;

  procedure TSourceManager.MarkCodeLines(Edit: TCodeEdit);
    var
      i: integer;
    begin
      for i := 0 to pred(Edit.Lines.Count) do
        if Edit.Lines.Objects[i] <> nil
          then PutImage(Edit, i, imCodeLine);
    end;

  procedure TSourceManager.PutImage(Edit: TCodeEdit; const aLine, Image: integer);
    var
      mark: TMark;
    begin
      with Edit do
        begin
          mark := TMark.Create(Edit);
          with mark do
            begin
              Line := aLine;
              Column := 0;
              ImageIndex := Image;
              IsBookmark := false;
              if Edit.BreakPoints.GetBreakPointFromLine(aLine) = nil
                then Visible := true
                else Visible := false;
              PaintLine  := false;
              InternalImage := false;
            end;
          Marks.Insert(0, mark);           //Marks.Place(mark);
        end;
    end;

  procedure TSourceManager.RemoveCodeMarks;
    var
      i, j: integer;
      Marks: TMarks;
    begin
      for i := 0 to pred(EditCount) do
        for j := 0 to pred(Editor[i].Lines.Count) do
          if Editor[i].Lines.Objects[j] <> nil
            then
              begin
                Editor[i].Marks.GetMarksForLine(j, Marks);
                if (Marks[1] <> nil) and (Marks[1].ImageIndex = imCodeLine)
                  then Editor[i].Marks.Remove(Marks[1]);
              end;
    end;

  procedure TSourceManager.CheckBreakPoint;
    var
      i, j: integer;
      Statement: TStatement;
    begin
      for i := 0 to pred(EditCount) do
        for j := 0 to pred(Editor[i].BreakPoints.Count) do
          if Editor[i].Lines.Objects[Editor[i].BreakPoints[j].Line] <> nil
            then
              begin
                Editor[i].BreakPoints[j].ImageIndex := imBreakPointValid;
                Statement := TStatement(Editor[i].Lines.Objects[Editor[i].BreakPoints[j].Line]);
                Statement.HasBreakPoint := true;
              end
            else
              begin
                Editor[i].BreakPoints[j].ImageIndex := imBreakPointInvalid;
                Editor[i].BreakPoints[j].ForeGround := clLime;
                Editor[i].BreakPoints[j].BackGround := clOlive;
              end;
    end;

end.
