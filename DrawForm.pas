unit DrawForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, AutoCAD, GraphSys;

type
  TDrawingForm = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    FDXFFile: TDXF;
    FBitmap: TBmp;
    FPoints: TPoints;
    procedure SeTDXF(const Value: TDXF);
    procedure SetBitmap(const Value: TBmp);
    procedure SetPoints(const Value: TPoints);
  public
    property DXFFile: TDXF read FDXFFile write SeTDXF;
    property Bitmap: TBmp read FBitmap write SetBitmap;
    property Points: TPoints read FPoints write SetPoints;
  end;

implementation

{$R *.DFM}

{ TForm1 }

  procedure TDrawingForm.SeTDXF(const Value: TDXF);
    begin
      if Value <> FDXFFile
        then
          begin
            FDXFFile := Value;
            Refresh;
          end;
    end;

  procedure TDrawingForm.FormPaint(Sender: TObject);
    begin
      if Bitmap <> nil
        then Bitmap.Draw(Canvas);
      if DXFFile <> nil
        then DXFFile.Draw(Canvas);
      if Points <> nil
        then Points.Draw(Canvas);
    end;

  procedure TDrawingForm.SetBitmap(const Value: TBmp);
    begin
      if Value <> FBitmap
        then
          begin
            FBitmap := Value;
            Refresh;
          end;
    end;

  procedure TDrawingForm.SetPoints(const Value: TPoints);
    begin
      if FPoints <> Value
        then
          begin
            FPoints := Value;
            Refresh;
          end;
    end;

  end.
