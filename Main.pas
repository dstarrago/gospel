unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, TeeProcs, TeEngine, Chart, ComCtrls, Parser, GSystem;


type
  TCompiler = class(TForm)
    Panel1: TPanel;
    Panel3: TPanel;
    GroupBox1: TGroupBox;
    Types: TListBox;
    GroupBox2: TGroupBox;
    Variables: TListBox;
    GroupBox3: TGroupBox;
    Constants: TListBox;
    StatusBar1: TStatusBar;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Procedures: TListBox;
    Functions: TListBox;
    Panel4: TPanel;
    GroupBox4: TGroupBox;
    Tokens: TListBox;
    GroupBox7: TGroupBox;
    Blocks: TListBox;
    Panel2: TPanel;
    Panel5: TPanel;
    Run: TPanel;
    Panel6: TPanel;
    Memo2: TMemo;
    Memo1: TMemo;
    GroupBox8: TGroupBox;
    Console: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BlocksClick(Sender: TObject);
    procedure RunClick(Sender: TObject);
  private
    Parser1: TParser;
    CurrentBlock: TBlock;
    MainModule: TModule;
  public
    procedure Compile;
    procedure Scan;
    procedure ShowSymbols;
    procedure ShowBlocks;
  end;

var
  Compiler: TCompiler;

implementation

{$R *.DFM}

procedure TCompiler.FormCreate(Sender: TObject);
  begin
    Parser1 := TParser.Create(Memo2.Text);
    Parser1.Console := Console;
    Scan;
    Compile;
  end;

procedure TCompiler.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
    TPanel(Sender).BevelInner := bvLowered;
  end;

procedure TCompiler.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
    TPanel(Sender).BevelInner := bvRaised;
  end;

procedure TCompiler.Panel2Click(Sender: TObject);
  begin
    Scan;
    Compile;
  end;

procedure TCompiler.Compile;
  begin
    try
      Parser1.Source := Memo2.Text;
      MainModule := Parser1.Compile;
      StatusBar1.SimpleText := 'Compilation succesfull';
      CurrentBlock := Parser1.GlobalBlock;
    except
      on E: Exception do
        StatusBar1.SimpleText := E.Message;
    end;
    ShowBlocks;
    ShowSymbols;
  end;

procedure TCompiler.Scan;
  var
    Scanner1: TScanner;
  begin
    Scanner1 := TScanner.Create(Memo2.Text);
    Tokens.Clear;
    while Scanner1.NextToken
      do Tokens.Items.Add(Scanner1.Token);
    Scanner1.free;
  end;

  procedure TCompiler.ShowSymbols;
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

  procedure TCompiler.FormDestroy(Sender: TObject);
    begin
      Parser1.free;
    end;

  procedure TCompiler.ShowBlocks;
    var
      i: integer;
    begin
      Blocks.Clear;
      for i := 0 to pred(Parser1.BlockList.Count) do
        Blocks.Items.Add(TBlock(Parser1.BlockList.Items[i]).Name);
    end;

  procedure TCompiler.BlocksClick(Sender: TObject);
    var
      Block: TBlock;
    begin
      if Blocks.ItemIndex >= 0
        then
          begin
            Block := TBlock(Parser1.BlockList.Items[Blocks.ItemIndex]);
            if Block <> CurrentBlock
              then
                begin
                  CurrentBlock := Block;
                  ShowSymbols;
                end;
          end;
    end;

  procedure TCompiler.RunClick(Sender: TObject);
    begin
      if MainModule <> nil
        then MainModule.Body.Execute;
    end;

end.
