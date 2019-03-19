unit Fonts;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, ComCtrls;

type
  TFontsDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    FontName: TComboBox;
    UpDown1: TUpDown;
    FontSize: TEdit;
    Sample: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FontNameChange(Sender: TObject);
    procedure FontSizeChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    procedure GetFontNames;
  public
    { Public declarations }
  end;

var
  FontsDlg: TFontsDlg;

implementation

{$R *.DFM}

  function EnumFontsProc(var LogFont: TLogFont; var TextMetric: TTextMetric;
    FontType: Integer; Data: Pointer): Integer; stdcall;
    begin
      if odd(LogFont.lfPitchAndFamily)
        then TStrings(Data).Add(LogFont.lfFaceName);
      Result := 1;
    end;

  procedure TFontsDlg.GetFontNames;
    var
      DC: HDC;
    begin
      DC := GetDC(0);
      EnumFonts(DC, nil, @EnumFontsProc, Pointer(FontName.Items));
      ReleaseDC(0, DC);
      FontName.Sorted := True;
    end;

  procedure TFontsDlg.FormCreate(Sender: TObject);
    begin
      GetFontNames;
    end;

  procedure TFontsDlg.FormShow(Sender: TObject);
    begin
      Sample.Font.Name := FontName.Items[FontName.ItemIndex];
      Sample.Font.Size := StrToInt(FontSize.Text);
    end;

  procedure TFontsDlg.FontNameChange(Sender: TObject);
    begin
      Sample.Font.Name := FontName.Items[FontName.ItemIndex];
    end;

  procedure TFontsDlg.FontSizeChange(Sender: TObject);
    begin
      Sample.Font.Size := StrToInt(FontSize.Text);
    end;

  procedure TFontsDlg.FormActivate(Sender: TObject);
    begin
      OkBtn.SetFocus;
    end;

end.
