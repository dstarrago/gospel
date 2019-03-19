unit Paths;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  TPathForm = class(TForm)
    PathList: TListBox;
    Replace: TButton;
    Add: TButton;
    Delete: TButton;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Edit1: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure Edit1Change(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure ReplaceClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure PathListClick(Sender: TObject);
  private
    procedure SetPaths(const Value: string);
    function GetPaths: string;
    procedure UpDateButtons;
  public
    property Paths: string read GetPaths write SetPaths;
  end;

var
  PathForm: TPathForm;

implementation

{$R *.DFM}

  procedure TPathForm.Edit1Change(Sender: TObject);
    begin
      UpDateButtons;
    end;

  procedure TPathForm.AddClick(Sender: TObject);
    begin
      if Edit1.Text <> ''
        then PathList.Items.Add(Edit1.Text);
    end;

  procedure TPathForm.ReplaceClick(Sender: TObject);
    begin
      if (Edit1.Text <> '') and (PathList.ItemIndex <> -1)
        then PathList.Items[PathList.ItemIndex] := Edit1.Text;
    end;

  procedure TPathForm.DeleteClick(Sender: TObject);
    begin
      if PathList.ItemIndex <> -1
        then PathList.Items.Delete(PathList.ItemIndex);
    end;

  procedure TPathForm.SpeedButton1Click(Sender: TObject);
    begin
      if PathList.ItemIndex > 0
        then PathList.Items.Exchange(PathList.ItemIndex, pred(PathList.ItemIndex));
    end;

  procedure TPathForm.SpeedButton2Click(Sender: TObject);
    begin
      if PathList.ItemIndex < pred(PathList.Items.Count)
        then PathList.Items.Exchange(PathList.ItemIndex, succ(PathList.ItemIndex));
    end;

  procedure TPathForm.SetPaths(const Value: string);
    var
      i: integer;
      S: string;
    begin
      S := Value;
      i := Pos(';', S);
      while i <> 0 do
        begin
          PathList.Items.Add(Copy(S, 1, pred(i)));
          System.Delete(S, 1, i);
          i := Pos(';', S);
        end;
      if length(S) > 0
        then PathList.Items.Add(S);
      UpDateButtons;
    end;

  function TPathForm.GetPaths: string;
    var
      i: integer;
    begin
      Result := '';
      for i := 0 to PathList.Items.Count - 2 do
        Result := Result + PathList.Items[i] + ';';
      Result := Result + PathList.Items[pred(PathList.Items.Count)];
    end;

  procedure TPathForm.PathListClick(Sender: TObject);
    begin
      Edit1.Text := PathList.Items[PathList.ItemIndex];
    end;

  procedure TPathForm.UpDateButtons;
    begin
      Add.Enabled := Edit1.Text <> '';
      Replace.Enabled := (Edit1.Text <> '') and (PathList.Items.Count > 0);
      Delete.Enabled := PathList.Items.Count > 0;
    end;

end.
