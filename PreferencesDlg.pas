unit PreferencesDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TPreferences = class(TForm)
    Ok: TButton;
    Cancel: TButton;
    EditorOptions: TGroupBox;
    InsertMode: TCheckBox;
    AutoIndentMode: TCheckBox;
    SmartTab: TCheckBox;
    BackspaceUnindent: TCheckBox;
    BRIEFCursor: TCheckBox;
    PersistentsBlocks: TCheckBox;
    FindTextAtCursor: TCheckBox;
    UseSyntaxHighLight: TCheckBox;
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Preferences: TPreferences;

implementation

{$R *.DFM}

  procedure TPreferences.OkClick(Sender: TObject);
    begin
      ModalResult := mrOk;
    end;

  procedure TPreferences.CancelClick(Sender: TObject);
    begin
      ModalResult := mrCancel;
    end;

  procedure TPreferences.FormActivate(Sender: TObject);
    begin
      Ok.SetFocus;
    end;

end.
