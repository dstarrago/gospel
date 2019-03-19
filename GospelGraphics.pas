
{*******************************************************}
{                                                       }
{       Gospel Built-in Routine Library                 }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GospelGraphics;

interface

  uses Kernel;

{
  function LoadFromFile(const FileName: TFileName): boolean;
  procedure SaveToFile(const FileName: TFileName);
  procedure Draw(Canvas: TCanvas);
  function WriteToPoints(const Color: TColor): TPoints;
  procedure GrayLevel;
  procedure Umbralize(const Umbral: Integer);
}

  type
    TLoadBitmap =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TSaveBitmap =
      class(TProcedure)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TDrawBitmap =
      class(TProcedure)
        private
          procedure DrawBitmap(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TBitmapToPoints =
      class(TFunction)
        private
          procedure BitmapToPoints(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TPointsToBitmap =
      class(TFunction)
        private
          procedure PointsToBitmap(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TBitmapGrayLevel =
      class(TProcedure)
        private
          procedure BitmapGrayLevel(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TBitmapUmbralize =
      class(TProcedure)
        private
          procedure BitmapUmbralize(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TDrawPoints =
      class(TProcedure)
        private
          procedure DrawPoints(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

implementation

  uses GraphSys, DrawForm, Threads;

{ TLoadBitmap }

  constructor TLoadBitmap.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'LoadBmp';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  StringType.Instantiate;
      Data.Name := 'FileName';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := BooleanType.Instantiate;
    end;

  procedure TLoadBitmap.Execute;
    begin
      with Parameters do
        TBoolean(Result).Value := TBmp(Items[0]).LoadFromFile(TString(Items[1]).Value);
    end;

{ TSaveBitmap }

  constructor TSaveBitmap.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'SaveBmp';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  StringType.Instantiate;
      Data.Name := 'FileName';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TSaveBitmap.Execute;
    begin
      with Parameters do
        TBmp(Items[0]).SaveToFile(TString(Items[1]).Value);
    end;

{ TDrawBitmap }

  constructor TDrawBitmap.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'DrawBmp';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TDrawBitmap.DrawBitmap(const params: array of const);
    var
      Form: TDrawingForm;
    begin
      Form := TDrawingForm.Create(nil);
      Form.Bitmap := TBmp(Parameters.Items[0]);
      Form.ShowModal;
    end;

  procedure TDrawBitmap.Execute;
    begin
      Join(DrawBitmap, []);
    end;

{ TBitmapToPoints }

  procedure TBitmapToPoints.BitmapToPoints(const params: array of const);
    begin
      with Parameters do
        TPoints(Result).Assign(TBmp(Items[0]).WriteToPoints(TInteger(Items[1]).Value));
    end;

  constructor TBitmapToPoints.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'BmpToPoints';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Color';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := PointsType.Instantiate;
    end;

  procedure TBitmapToPoints.Execute;
    begin
      Join(BitmapToPoints, []);
    end;

{ TBitmapGrayLevel }

  procedure TBitmapGrayLevel.BitmapGrayLevel(const params : array of const);
    begin
      with Parameters do
        TBmp(Items[0]).GrayLevel;
    end;

  constructor TBitmapGrayLevel.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'GrayLevel';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TBitmapGrayLevel.Execute;
    begin
      Join(BitmapGrayLevel, []);
    end;

{ TBitmapUmbralize }

  procedure TBitmapUmbralize.BitmapUmbralize(const params : array of const);
    begin
      with Parameters do
        TBmp(Items[0]).Umbralize(TInteger(Items[1]).Value);
    end;

  constructor TBitmapUmbralize.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Umbralize';
      Data :=  BitmapType.Instantiate;
      Data.Name := 'Bmp';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Umbral';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TBitmapUmbralize.Execute;
    begin
      Join(BitmapUmbralize, []);
    end;

{ TDrawPoints }

  constructor TDrawPoints.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'DrawPoints';
      Data :=  PointsType.Instantiate;
      Data.Name := 'Points';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TDrawPoints.DrawPoints(const params: array of const);
    var
      Form: TDrawingForm;
    begin
      Form := TDrawingForm.Create(nil);
      Form.Points := TPoints(Parameters.Items[0]);
      Form.ShowModal;
    end;

  procedure TDrawPoints.Execute;
    begin
      Join(DrawPoints, []);
    end;

{ TPointsToBitmap }

  constructor TPointsToBitmap.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'PointsToBmp';
      Data :=  PointsType.Instantiate;
      Data.Name := 'Pts';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := BitmapType.Instantiate;
    end;

  procedure TPointsToBitmap.Execute;
    begin
      Join(PointsToBitmap, []);
    end;

  procedure TPointsToBitmap.PointsToBitmap(const params: array of const);
    begin
      with Parameters do
        TBmp(Result).Bitmap.Assign(TPoints(Items[0]).WriteToBitmap);
    end;

initialization

    with SystemBlock do
      begin
        Procedures.AddEntry(TSaveBitmap.Create);
        Procedures.AddEntry(TDrawBitmap.Create);
        Functions.AddEntry(TLoadBitmap.Create);
        Functions.AddEntry(TBitmapToPoints.Create);
        Functions.AddEntry(TPointsToBitmap.Create);
        Procedures.AddEntry(TBitmapGrayLevel.Create);
        Procedures.AddEntry(TBitmapUmbralize.Create);
        Procedures.AddEntry(TDrawPoints.Create);
      end;

end.
