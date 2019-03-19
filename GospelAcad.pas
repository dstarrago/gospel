
{*******************************************************}
{                                                       }
{       Gospel Built-in Routine Library                 }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GospelAcad;

{
    procedure LoadFromFile(const FileName: TFileName);
    procedure SaveToFile(const FileName: TFileName);
    procedure Draw(Canvas: TCanvas);
    function WriteToPoints: TPoints;
    procedure ReadFromPoints(Points: TPoints);
    procedure Scale(const Factor: real);
    procedure Translate(const Point: TPoint);
    procedure Rotate(const Angle: real);
}

interface

  uses Kernel;

  type
    TLoadDXF =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TSaveDXF =
      class(TProcedure)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TDrawDXF =
      class(TProcedure)
        private
          procedure DrawDXF(const params : array of const);
        public
          constructor Create;
          procedure Execute; override;
      end;

    TDXFToPoints =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TPointsToDXF =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TScaleDXF =
      class(TProcedure)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TTranslateDXF =
      class(TProcedure)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TRotateDXF =
      class(TProcedure)
        public
          constructor Create;
          procedure Execute; override;
      end;

implementation

  uses AutoCAD, DrawForm, GraphSys, Threads;

{ TLoadDXF }

  constructor TLoadDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'LoadDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  StringType.Instantiate;
      Data.Name := 'FileName';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := BooleanType.Instantiate;
    end;

  procedure TLoadDXF.Execute;
    begin
      with Parameters do
        TBoolean(Result).Value := TDXF(Items[0]).LoadFromFile(TString(Items[1]).Value);
    end;

{ TSaveDXF }

  constructor TSaveDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'SaveDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  StringType.Instantiate;
      Data.Name := 'FileName';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TSaveDXF.Execute;
    begin
      with Parameters do
        TDXF(Items[0]).SaveToFile(TString(Items[1]).Value);
    end;

{ TDrawDXF }

  constructor TDrawDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'DrawDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TDrawDXF.DrawDXF(const params: array of const);
    var
      Form: TDrawingForm;
    begin
      Form := TDrawingForm.Create(nil);
      Form.DXFFile := TDXF(Parameters.Items[0]);
      Form.ShowModal;
    end;

  procedure TDrawDXF.Execute;
    begin
      Join(DrawDXF, []);
    end;

{ TDXFToPoints }

  constructor TDXFToPoints.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'DXFToPoints';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := PointsType.Instantiate;
    end;

  procedure TDXFToPoints.Execute;
    begin
      with Parameters do
        TPoints(Result).Assign(TDXF(Items[0]).WriteToPoints);
    end;

{ TScaleDXF }

  constructor TScaleDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'ScaleDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Factor';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TScaleDXF.Execute;
    begin
      with Parameters do
        TDXF(Items[0]).Scale(TReal(Items[1]).Value);
    end;

{ TTranslateDXF }

  constructor TTranslateDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'TranslateDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'X';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Y';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Z';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TTranslateDXF.Execute;
    begin
      with Parameters do
        TDXF(Items[0]).Translate(TPoint.Create(TReal(Items[1]).Value,
        TReal(Items[2]).Value, TReal(Items[3]).Value));
    end;

{ TRotateDXF }

  constructor TRotateDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'RotateDXF';
      Data :=  DXFType.Instantiate;
      Data.Name := 'Dxf';
      Variables.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Angle';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TRotateDXF.Execute;
    begin
      with Parameters do
        TDXF(Items[0]).Rotate(TReal(Items[1]).Value);
    end;

{ TPointsToDXF }

  constructor TPointsToDXF.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'PointsToDXF';
      Data :=  PointsType.Instantiate;
      Data.Name := 'Pts';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := DXFType.Instantiate;
    end;

  procedure TPointsToDXF.Execute;
    begin
      with Parameters do
        TDXF(Result).ReadFromPoints(TPoints(Items[0]));
    end;

initialization

    with SystemBlock do
      begin
        Procedures.AddEntry(TSaveDXF.Create);
        Procedures.AddEntry(TDrawDXF.Create);
        Procedures.AddEntry(TScaleDXF.Create);
        Procedures.AddEntry(TTranslateDXF.Create);
        Procedures.AddEntry(TRotateDXF.Create);
        Functions.AddEntry(TLoadDXF.Create);
        Functions.AddEntry(TDXFToPoints.Create);
        Functions.AddEntry(TPointsToDXF.Create);
      end;

end.
