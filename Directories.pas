unit Directories;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TDirectoriesForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    OutputDirectory: TComboBox;
    SearchPath: TComboBox;
    SpeedButton1: TSpeedButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    procedure SpeedButton1Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RecordHistory(ComboBox: TComboBox);
  end;

var
  DirectoriesForm: TDirectoriesForm;

implementation

{$R *.DFM}

  uses Paths;

  procedure TDirectoriesForm.SpeedButton1Click(Sender: TObject);
    var
      PathForm: TPathForm;
    begin
      PathForm := TPathForm.Create(Self);
      try
        PathForm.Paths := SearchPath.Text;
        if PathForm.ShowModal = mrOk
          then SearchPath.Text := PathForm.Paths;
      finally
        PathForm.free;
      end;
    end;

  procedure TDirectoriesForm.BitBtn3Click(Sender: TObject);
    begin
      RecordHistory(SearchPath);
      RecordHistory(OutputDirectory);
    end;

  procedure TDirectoriesForm.RecordHistory(ComboBox: TComboBox);
    var
      Index: integer;
    begin
      Index := ComboBox.Items.IndexOf(ComboBox.Text);
      if Index = -1
        then ComboBox.Items.Insert(0, ComboBox.Text)
        else ComboBox.Items.Move(Index, 0);
    end;

end.
