unit FindText;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

const
  diForward        = 0;
  diBackward       = 1;
  scGlobal         = 0;
  scSelected_Text  = 1;
  orFrom_Cursor    = 0;
  orEntire_Scope   = 1;

type
  TFind = class(TForm)
    Label1: TLabel;
    Text: TComboBox;
    Options: TGroupBox;
    Direction: TRadioGroup;
    Scope: TRadioGroup;
    Origin: TRadioGroup;
    MatchCase: TCheckBox;
    WholeWords: TCheckBox;
    Ok: TButton;
    Cancel: TButton;
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Find: TFind;

implementation

{$R *.DFM}

  procedure TFind.OkClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TFind.CancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TFind.FormActivate(Sender: TObject);
    begin
      OK.SetFocus;
    end;

end.
