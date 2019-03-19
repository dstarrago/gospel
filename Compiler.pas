unit Compiler;

interface

  uses Classes, SysUtils, Kernel, Parser, StdCtrls, SourceManage;

  type
    TGospelCompiler =
      class
        private
          FMainModule: TModule;
          FCompilerMessage: string;
          FSourceManager: TSourceManager;
          function GetConsole: TListBox;
          procedure SetConsole(const Value: TListBox);
          function GetBlockList: TList;
          procedure SetBlockList(const Value: TList);
          procedure SetOnFinishRuning(const Value: TNotifyEvent);
        public
          Parser: TParser;
          Runner: TRunner;
          constructor Create(AnOnBreakingExecution, AnOnResumeExecution: TBreakingExecution; aSourceManager: TSourceManager);
          destructor  Destroy; override;
          function Compile: boolean;
          procedure Run;
          procedure Reset;
          procedure Pause;
          property MainModule: TModule read FMainModule write FMainModule;
          property CompilerMessage: string read FCompilerMessage write FCompilerMessage;
          property Console: TListBox read GetConsole write SetConsole;
          property BlockList: TList read GetBlockList write SetBlockList;
          property OnFinishRuning: TNotifyEvent write SetOnFinishRuning;
      end;

implementation

{ TGospelCompiler }

  function TGospelCompiler.Compile: boolean;
    begin
      Result := false;
      Parser.ModuleName := FSourceManager.AppName;
      try
        MainModule := Parser.CompileProgram;
        Runner.StartPoint := MainModule.Body;
        CompilerMessage := 'Compilation successful';
        Result := true;
      except
        on E: Exception do
          begin
            CompilerMessage := E.Message;
          end;
      end;
    end;

  constructor TGospelCompiler.Create(AnOnBreakingExecution, AnOnResumeExecution: TBreakingExecution; aSourceManager: TSourceManager);
    begin
      inherited Create;
      FSourceManager := aSourceManager;
      Runner := TRunner.Create(AnOnBreakingExecution, AnOnResumeExecution);
      Parser := TParser.Create(Runner, aSourceManager);
    end;

  destructor TGospelCompiler.Destroy;
    begin
      Parser.free;
      Runner.free;
      inherited;
    end;

  function TGospelCompiler.GetBlockList: TList;
    begin
      Result := Parser.BlockList;
    end;

  function TGospelCompiler.GetConsole: TListBox;
    begin
      Result := Parser.Console;
    end;

  procedure TGospelCompiler.Run;
    begin
      Runner.RunApplication;
    end;

  procedure TGospelCompiler.Pause;
    begin
      Runner.Pause;
    end;

  procedure TGospelCompiler.Reset;
    begin
      Runner.Reset;
    end;

  procedure TGospelCompiler.SetBlockList(const Value: TList);
    begin
      Parser.BlockList := Value;
   end;

  procedure TGospelCompiler.SetConsole(const Value: TListBox);
    begin
      Parser.Console := Value;
    end;

  procedure TGospelCompiler.SetOnFinishRuning(const Value: TNotifyEvent);
    begin
      Runner.OnTerminateEvent := Value;
    end;

end.
