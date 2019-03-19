
{*******************************************************}
{                                                       }
{       Gospel Compiler System                          }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit AutoCAD;

interface

  uses Classes, SysUtils, Graphics, Kernel, GraphSys;

  type
    TDXFEntity =
      class
        protected
          function Clone: TDXFEntity;  virtual; abstract;
          procedure Draw(Canvas: TCanvas);  virtual; abstract;
          function WriteToPoints: TPoints;  virtual; abstract;
          procedure Scale(const Factor: real);  virtual; abstract;
          procedure Translate(const Point: TPoint);  virtual; abstract;
          procedure Rotate(const Angle: real);  virtual; abstract;
      end;

    TDXFPoint =
      class(TDXFEntity)
        private
          FZ: real;
          FX: real;
          FY: real;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aX, aY, aZ: real);
          function Clone: TDXFEntity;  override;
          property X: real read FX;
          property y: real read FY;
          property Z: real read FZ;
      end;

    TDXFLine =
      class(TDXFEntity)
        private
          FZ1: real;
          FX1: real;
          FY1: real;
          FZ2: real;
          FX2: real;
          FY2: real;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aX1, aY1, aZ1, aX2, aY2, aZ2: real);
          function Clone: TDXFEntity; override;
          property X1: real read FX1;
          property y1: real read FY1;
          property Z1: real read FZ1;
          property X2: real read FX2;
          property Y2: real read FY2;
          property Z2: real read FZ2;
      end;

    TRealArray = array of real;
    TDXFPolyLine =
      class(TDXFEntity)
        private
          FX: TRealArray;
          FY: TRealArray;
          FLineCount: integer;
          FClosed: integer;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aLineCount: integer; const aX, aY: TRealArray; const aClosed: integer);
          function Clone: TDXFEntity;  override;
          destructor Destroy;  override;
          property LineCount: integer read FLineCount;
          property Closed: integer read FClosed;
          property X: TRealArray read FX;
          property Y: TRealArray read FY;
      end;

    TDXFArc =
      class(TDXFEntity)
        private
          FX: real;
          FY: real;
          FZ: real;
          FRad: real;
          FStartAng: real;
          FEndAng: real;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aX, aY, aZ, aRad, aStartAng, aEndAng: real);
          function Clone: TDXFEntity;  override;
          property X: real read FX;
          property Y: real read FY;
          property Z: real read FZ;
          property Rad: real read FRad;
          property StartAng: real read FStartAng;
          property EndAng: real read FEndAng;
      end;

    TDXFCircle =
      class(TDXFEntity)
        private
          FX: real;
          FY: real;
          FZ: real;
          FRad: real;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aX, aY, aZ, aRad: real);
          function Clone: TDXFEntity;  override;
          property X: real read FX;
          property Y: real read FY;
          property Z: real read FZ;
          property Rad: real read FRad;
      end;

    TDXFEllipse =
      class(TDXFEntity)
        private
          FX1: real;
          FY1: real;
          FZ1: real;
          FZ2: real;
          FX2: real;
          FY2: real;
          FRad: real;
          FStartAng: real;
          FEndAng: real;
        protected
          procedure Draw(Canvas: TCanvas);  override;
          function WriteToPoints: TPoints;  override;
          procedure Scale(const Factor: real);  override;
          procedure Translate(const Point: TPoint);  override;
          procedure Rotate(const Angle: real);  override;
        public
          constructor Create(const aX1, aY1, aZ1, aX2, aY2, aZ2, aRad, aStartAng, aEndAng: real);
          function Clone: TDXFEntity;  override;
          property X1: real read FX1;
          property Y1: real read FY1;
          property Z1: real read FZ1;
          property X2: real read FX2;
          property Y2: real read FY2;
          property Z2: real read FZ2;
          property Rad: real read FRad;
          property StartAng: real read FStartAng;
          property EndAng: real read FEndAng;
      end;

    TDXF = class;
    TDXFiler =
      class
        private
          FOwner: TDXF;
          Source: TStringList;
        public
          constructor Create(DXFObject: TDXF; S: TStringList);
          property Owner: TDXF read FOwner write FOwner;
      end;

    TDXFReader =
      class(TDXFiler)
        private
          X, Y, Z: real;
          V, A: real;
          P, F: integer;
          S, T: string;
          CG: integer;
          FCodeGroup: integer;
          FSourcePos: integer;
          procedure ReadPair;
          procedure ReachEntities;
          procedure ScanEntities;
          procedure FindClause(const Name: string);
          procedure InpuTDXFPoint;
          procedure ReadPoint;
          procedure ReadLine;
          procedure ReadPolyLine;
          procedure ReadArc;
          procedure ReadCircle;
          procedure ReadEllipse;
          function ReadFloat: real;
          function ReadInteger: integer;
          function ReadString: string;
          procedure NextCodeGroup;
          property CodeGroup: integer read FCodeGroup write FCodeGroup;
        public
          procedure Read;
      end;

    TDXFWriter =
      class(TDXFiler)
        private
          FIndex: integer;
          procedure WritePoint;
          procedure WriteLine;
          procedure WritePolyLine;
          procedure WriteArc;
          procedure WriteCircle;
          procedure WriteEllipse;
          property Index: integer read FIndex write FIndex;
        public
          procedure Write;
      end;

    TDXFType =
      class(TDataType)
        public
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TDXF =
      class(TData)
        private
          FEntities: TList;
          function GeTDXFEntity(const i: integer): TDXFEntity;
          function GetEntityCount: integer;
        public
          procedure AddEntity(Entity: TDXFEntity);
          procedure Delete(const i: integer);
          procedure Clear;
          property Entity[const i: integer]: TDXFEntity read GeTDXFEntity;
          property EntityCount: integer read GetEntityCount;
        public
          constructor CreateFrom(aDataType: TDataType); override;
          destructor Destroy;  override;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function LoadFromFile(const FileName: TFileName): boolean;
          procedure SaveToFile(const FileName: TFileName);
        public
          procedure Draw(Canvas: TCanvas);
          function WriteToPoints: TPoints;
          procedure ReadFromPoints(Points: TPoints);
          procedure Scale(const Factor: real);
          procedure Translate(const Point: TPoint);
          procedure Rotate(const Angle: real);
      end;

var
  DXFType: TDXFType;

implementation

  uses Constants, Dialogs;

{ TDXF }

  procedure TDXF.AddEntity(Entity: TDXFEntity);
    begin
      FEntities.Add(Entity);
    end;

  procedure TDXF.Assign(Data: TData);
    var
      i: integer;
    begin
      if Data is TDXF
        then
          begin
            Clear;
            for i := 0 to pred(TDXF(Data).EntityCount) do
              AddEntity(TDXF(Data).Entity[i].Clone);
          end
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  procedure TDXF.Clear;
    var
      i: integer;
    begin
      for i := 0 to pred(EntityCount) do
        Entity[i].free;
      FEntities.Clear;
    end;

  constructor TDXF.CreateFrom(aDataType: TDataType);
    begin
      inherited;
      FEntities := TList.Create;
    end;

  procedure TDXF.Delete(const i: integer);
    begin
      FEntities.Delete(i);
    end;

  destructor TDXF.Destroy;
    begin
      FEntities.free;
      inherited;
    end;

  procedure TDXF.Draw(Canvas: TCanvas);
    var
      i: integer;
    begin
      for i := 0 to pred(FEntities.Count) do
        Entity[i].Draw(Canvas);
    end;

  function TDXF.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      Result.Assign(Self);
    end;

  function TDXF.GeTDXFEntity(const i: integer): TDXFEntity;
    begin
      Result := FEntities.Items[i];
    end;

  function TDXF.GetEntityCount: integer;
    begin
      Result := FEntities.Count; 
    end;

  function TDXF.LoadFromFile(const FileName: TFileName): boolean;
    var
      S: TStringList;
      Reader: TDXFReader;
    begin
      S := TStringList.Create;
      Result := false;
      try
        S.LoadFromFile(FileName);
        Reader := TDXFReader.Create(Self, S);
        try
          Reader.Read;
        finally
          Reader.free;
        end;
        Result := true;
      except
        on EFOpenError do
          MessageDlg(Format(emFileNotFound, [FileName]), mtError, [mbOK], 0);
      end;
      S.free;
    end;

  procedure TDXF.ReadFromPoints(Points: TPoints);
    var
      i: integer;
    begin
      Clear;
      for i := 0 to pred(Points.PointCount) do
        with Points.Point[i] do
          AddEntity(TDXFPoint.Create(X, Y, Z));
    end;

  procedure TDXF.Rotate(const Angle: real);
    var
      i: integer;
    begin
      for i := 0 to pred(FEntities.Count) do
        Entity[i].Rotate(Angle);
    end;

  procedure TDXF.SaveToFile(const FileName: TFileName);
    var
      S: TStringList;
      Writer: TDXFWriter;
    begin
      S := TStringList.Create;
      try
        Writer := TDXFWriter.Create(Self, S);
        try
          Writer.Write;
        finally
          Writer.free;
        end;
        S.SaveToFile(FileName);
      finally
        S.free;
      end;
    end;

  procedure TDXF.Scale(const Factor: real);
    var
      i: integer;
    begin
      for i := 0 to pred(FEntities.Count) do
        Entity[i].Scale(Factor);
    end;

  procedure TDXF.Translate(const Point: TPoint);
    var
      i: integer;
    begin
      for i := 0 to pred(FEntities.Count) do
        Entity[i].Translate(Point);
    end;

  function TDXF.WriteToPoints: TPoints;
    var
      i: integer;
      Points: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
      for i := 0 to pred(FEntities.Count) do
        begin
          Points := Entity[i].WriteToPoints;
          Result.Concat(Points);
          Points.free;
        end;
    end;

{ TDXFiler }

  constructor TDXFiler.Create(DXFObject: TDXF; S: TStringList);
    begin
      inherited Create;
      Owner := DXFObject;
      Source := S;
    end;

{ TDXFReader }

  procedure TDXFReader.FindClause(const Name: string);
    begin
      repeat
        ReadPair;
      until (CodeGroup = 0) and (S = Name);
    end;

  procedure TDXFReader.InpuTDXFPoint;
    begin
      X := ReadFloat;
      CG := ReadInteger;
      if CG <> CodeGroup + 10
        then raise Exception.Create(Format('Invalid Y coord code', [CG]));
      Y := ReadFloat;
      CG := ReadInteger;
      if CG <> CodeGroup + 20
        then CG := -CG
        else Z := ReadFloat;
      if CG = 0
        then dec(FSourcePos);
    end;

  procedure TDXFReader.NextCodeGroup;
    begin
      if CG < 0
        then
          begin
            FCodeGroup := -CG;
            CG := 0;
          end
        else FCodeGroup := ReadInteger;
    end;

  procedure TDXFReader.ReachEntities;
    begin
      FSourcePos := 0;
      repeat
        FindClause('SECTION');
        ReadPair;
      until S = 'ENTITIES';
    end;

  procedure TDXFReader.Read;
    begin
      ReachEntities;
      ScanEntities;
    end;

  procedure TDXFReader.ReadArc;
    var
      X1, Y1, Z1: real;
      Rad: real;
      StartAng, EndAng: real;
    begin
      X1 := 0; Y1 := 0; Z1 := 0;
      Rad := 0; StartAng := 0; EndAng := 0;
      repeat
        ReadPair;
        if CodeGroup = 10
          then
            begin
              X1 := X;
              Y1 := Y;
              Z1 := Z;
            end;
        if CodeGroup = 40
          then Rad := V;
        if CodeGroup = 50
          then StartAng := A;
        if CodeGroup = 51
          then EndAng := A;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFArc.Create(X1, Y1, Z1, Rad, StartAng, EndAng));
    end;

  procedure TDXFReader.ReadCircle;
    var
      X1, Y1, Z1: real;
      Rad: real;
    begin
      X1 := 0; Y1 := 0; Z1 := 0;
      Rad := 0;
      repeat
        ReadPair;
        if CodeGroup = 10
          then
            begin
              X1 := X;
              Y1 := Y;
              Z1 := Z;
            end;
        if CodeGroup = 40
          then Rad := V;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFCircle.Create(X1, Y1, Z1, Rad));
    end;

  procedure TDXFReader.ReadEllipse;
    var
      X1, Y1, Z1: real;
      X2, Y2, Z2: real;
      Rad: real;
      StartAng, EndAng: real;
    begin
      X1 := 0; Y1 := 0; Z1 := 0;
      X2 := 0; Y2 := 0; Z2 := 0;
      Rad := 0; StartAng := 0; EndAng := 0;
      repeat
        ReadPair;
        if CodeGroup = 10
          then
            begin
              X1 := X;
              Y1 := Y;
              Z1 := Z;
            end;
        if CodeGroup = 11
          then
            begin
              X2 := X;
              Y2 := Y;
              Z2 := Z;
            end;
        if CodeGroup = 40
          then Rad := V;
        if CodeGroup = 41
          then StartAng := V;
        if CodeGroup = 42
          then EndAng := V;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFEllipse.Create(X1, Y1, Z1, X2, Y2, Z2, Rad, StartAng, EndAng));
    end;

  function TDXFReader.ReadFloat: real;
    begin
      try
        Result := StrToFloat(Source[FSourcePos]);
        inc(FSourcePos);
      except
        raise Exception.Create(Format('Error reading group code %d in DXF', [CodeGroup]));
      end;
    end;

  function TDXFReader.ReadInteger: integer;
    begin
      try
        Result := StrToInt(Source[FSourcePos]);
        inc(FSourcePos);
      except
        raise Exception.Create(Format('Error reading group code %d in DXF', [CodeGroup]));
      end;
    end;

  procedure TDXFReader.ReadLine;
    var
      X1, Y1, Z1: real;
      X2, Y2, Z2: real;
    begin
      X1 := 0; Y1 := 0; Z1 := 0;
      X2 := 0; Y2 := 0; Z2 := 0;
      repeat
        ReadPair;
        if CodeGroup = 10
          then
            begin
              X1 := X;
              Y1 := Y;
              Z1 := Z;
            end;
        if CodeGroup = 11
          then
            begin
              X2 := X;
              Y2 := Y;
              Z2 := Z;
            end;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFLine.Create(X1, Y1, Z1, X2, Y2, Z2));
    end;

  procedure TDXFReader.ReadPair;
    begin
      NextCodeGroup;
      case CodeGroup of
        0..9, 999:  S := ReadString;
        38..49:     V := ReadFloat;
        50..59:     A := ReadFloat;
        60..69:     P := ReadInteger;
        70..79:     F := ReadInteger;
        90..99:     ReadInteger;
        210..219:   InpuTDXFPoint;
        280..289:   ReadInteger;
        100:        ReadString;
        105:        ReadString;
        140..148:   V := ReadFloat;
        170..178:   F := ReadInteger;
        270..278:   F := ReadInteger;
        else
          begin
            if CodeGroup >= 1000
              then T := ReadString
              else
                if CodeGroup >= 20
                  then ReadString
                  else InpuTDXFPoint;
          end;
      end;
    end;

  procedure TDXFReader.ReadPoint;
    var
      X1, Y1, Z1: real;
    begin
      X1 := 0; Y1 := 0; Z1 := 0;
      repeat
        ReadPair;
        if CodeGroup = 10
          then
            begin
              X1 := X;
              Y1 := Y;
              Z1 := Z;
            end;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFPoint.Create(X1, Y1, Z1));
    end;

  procedure TDXFReader.ReadPolyLine;
    var
      Xp: TRealArray;
      Yp: TRealArray;
      Closed: integer;
      i: integer;
    begin
      i := 0;
      Closed := 0;
      repeat
        ReadPair;
        if CodeGroup = 90
          then
            begin
              SetLength(Xp, P);
              SetLength(Yp, P);
            end;
        if CodeGroup = 10
          then
            begin
              Xp[i] := X;
              Yp[i] := Y;
              inc(i);
            end;
        if CodeGroup = 70
          then Closed := F;
      until CodeGroup = 0;
      Owner.AddEntity(TDXFPolyLine.Create(Length(Xp), Xp, Yp, Closed));
    end;

  function TDXFReader.ReadString: string;
    begin
      Result := Source[FSourcePos];
      inc(FSourcePos);
    end;

  procedure TDXFReader.ScanEntities;
    begin
      ReadPair;
      while not((CodeGroup = 0) and (S = 'ENDSEC')) do
        begin
          if CodeGroup = 0
            then
              begin
                if S = 'POINT' then ReadPoint else
                if S = 'LINE' then ReadLine else
                if S = 'LWPOLYLINE' then ReadPolyLine else
                if S = 'ARC' then ReadArc else
                if S = 'CIRCLE' then ReadCircle else
                if S = 'ELLIPSE' then ReadEllipse else
                ReadPair;
              end
            else ReadPair;
        end;
    end;

{ TDXFWriter }

  procedure TDXFWriter.Write;
    begin
      Source.Add('0');
      Source.Add('SECTION');
      Source.Add('2');
      Source.Add('ENTITIES');
      Index := 0;
      with Owner do
        while Index < EntityCount do
          begin
            if Entity[Index] is TDXFPoint
              then WritePoint else
            if Entity[Index] is TDXFLine
              then WriteLine else
            if Entity[Index] is TDXFPolyLine
              then WritePolyLine else
            if Entity[Index] is TDXFArc
              then WriteArc else
            if Entity[Index] is TDXFCircle
              then WriteCircle else
            if Entity[Index] is TDXFEllipse
              then WriteEllipse;
            inc(FIndex);
          end;
      Source.Add('0');
      Source.Add('ENDSEC');
      Source.Add('0');
      Source.Add('EOF');
    end;

  procedure TDXFWriter.WriteArc;
    begin
      with TDXFArc(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('ARC');
          Source.Add('8');
          Source.Add('0');
          Source.Add('10');
          Source.Add(FloatToStr(X));
          Source.Add('20');
          Source.Add(FloatToStr(Y));
          Source.Add('30');
          Source.Add(FloatToStr(Z));
          Source.Add('40');
          Source.Add(FloatToStr(Rad));
          Source.Add('50');
          Source.Add(FloatToStr(StartAng));
          Source.Add('51');
          Source.Add(FloatToStr(EndAng));
        end;
    end;

  procedure TDXFWriter.WriteCircle;
    begin
      with TDXFCircle(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('CIRCLE');
          Source.Add('8');
          Source.Add('0');
          Source.Add('10');
          Source.Add(FloatToStr(X));
          Source.Add('20');
          Source.Add(FloatToStr(Y));
          Source.Add('30');
          Source.Add(FloatToStr(Z));
          Source.Add('40');
          Source.Add(FloatToStr(Rad));
        end;
    end;

  procedure TDXFWriter.WriteEllipse;
    begin
      with TDXFEllipse(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('ELLIPSE');
          Source.Add('8');
          Source.Add('0');
          Source.Add('10');
          Source.Add(FloatToStr(X1));
          Source.Add('20');
          Source.Add(FloatToStr(Y1));
          Source.Add('30');
          Source.Add(FloatToStr(Z1));
          Source.Add('11');
          Source.Add(FloatToStr(X2));
          Source.Add('21');
          Source.Add(FloatToStr(Y2));
          Source.Add('31');
          Source.Add(FloatToStr(Z2));
          Source.Add('40');
          Source.Add(FloatToStr(Rad));
          Source.Add('50');
          Source.Add(FloatToStr(StartAng));
          Source.Add('51');
          Source.Add(FloatToStr(EndAng));
        end;
    end;

  procedure TDXFWriter.WriteLine;
    begin
      with TDXFLine(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('LINE');
          Source.Add('8');
          Source.Add('0');
          Source.Add('10');
          Source.Add(FloatToStr(X1));
          Source.Add('20');
          Source.Add(FloatToStr(Y1));
          Source.Add('30');
          Source.Add(FloatToStr(Z1));
          Source.Add('11');
          Source.Add(FloatToStr(X2));
          Source.Add('21');
          Source.Add(FloatToStr(Y2));
          Source.Add('31');
          Source.Add(FloatToStr(Z2));
        end;
    end;

  procedure TDXFWriter.WritePoint;
    begin
      with TDXFPoint(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('POINT');
          Source.Add('8');
          Source.Add('0');
          Source.Add('10');
          Source.Add(FloatToStr(X));
          Source.Add('20');
          Source.Add(FloatToStr(Y));
          Source.Add('30');
          Source.Add(FloatToStr(Z));
        end;
    end;

  procedure TDXFWriter.WritePolyLine;
    var
      i: integer;
    begin
      with TDXFPolyLine(Owner.Entity[Index]) do
        begin
          Source.Add('0');
          Source.Add('LWPOLYLINE');
          Source.Add('8');
          Source.Add('0');
          Source.Add('90');
          Source.Add(IntToStr(LineCount));
          Source.Add('70');
          Source.Add(IntToStr(Closed));
          for i := 0 to pred(LineCount) do
            begin
              Source.Add('10');
              Source.Add(FloatToStr(X[i]));
              Source.Add('20');
              Source.Add(FloatToStr(Y[i]));
            end;
        end;
    end;

{ TDXFLine }

  function TDXFLine.Clone: TDXFEntity;
    begin
      Result := TDXFLine.Create(X1, Y1, Z1, X2, Y2, Z2);
    end;

  constructor TDXFLine.Create(const aX1, aY1, aZ1, aX2, aY2, aZ2: real);
    begin
      inherited Create;
      FX1 := aX1;
      FY1 := aY1;
      FZ1 := aZ1;
      FX2 := aX2;
      FY2 := aY2;
      FZ2 := aZ2;
    end;

  procedure TDXFLine.Draw(Canvas: TCanvas);
    begin
      with Canvas do
        begin
          MoveTo(trunc(X1), trunc(Y1));
          LineTo(trunc(X2), trunc(Y2));
        end;
    end;

  procedure TDXFLine.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFLine.Scale(const Factor: real);
    begin
      FX1 := FX1 * Factor;
      FY1 := FY1 * Factor;
      FZ1 := FZ1 * Factor;
      FX2 := FX2 * Factor;
      FY2 := FY2 * Factor;
      FZ2 := FZ2 * Factor;
    end;

  procedure TDXFLine.Translate(const Point: TPoint);
    begin
      FX1 := FX1 + Point.X;
      FY1 := FY1 + Point.Y;
      FZ1 := FZ1 + Point.Z;
      FX2 := FX2 + Point.X;
      FY2 := FY2 + Point.Y;
      FZ2 := FZ2 + Point.Z;
    end;

  function TDXFLine.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
    end;

{ TDXFArc }

  function TDXFArc.Clone: TDXFEntity;
    begin
      Result := TDXFArc.Create(X, Y, Z, Rad, StartAng, EndAng);
    end;

  constructor TDXFArc.Create(const aX, aY, aZ, aRad, aStartAng, aEndAng: real);
    begin
      inherited Create;
      FX := aX;
      FY := aY;
      FZ := aZ;
      FRad := aRad;
      FStartAng := aStartAng;
      FEndAng := aEndAng;
    end;

  procedure TDXFArc.Draw(Canvas: TCanvas);
    var
      X1, X2, X3, X4: real;
      Y1, Y2, Y3, Y4: real;
    begin
      X1 := X - Rad;
      Y1 := Y + Rad;
      X2 := X + Rad;
      Y2 := Y - Rad;
      X3 := X + Rad * cos(Pi * EndAng/180);
      Y3 := Y + Rad * sin(Pi * EndAng/180);
      X4 := X + Rad * cos(Pi * StartAng/180);
      Y4 := Y + Rad * sin(Pi * StartAng/180);
      Canvas.Arc(trunc(X1), trunc(Y1), trunc(X2), trunc(Y2), trunc(X3), trunc(Y3), trunc(X4), trunc(Y4));
    end;

  procedure TDXFArc.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFArc.Scale(const Factor: real);
    begin
      FX := FX * Factor;
      FY := FY * Factor;
      FZ := FZ * Factor;
      FRad := FRad * Factor;
    end;

  procedure TDXFArc.Translate(const Point: TPoint);
    begin
      FX := FX + Point.X;
      FY := FY + Point.Y;
      FZ := FZ + Point.Z;
    end;

  function TDXFArc.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
    end;

{ TDXFPoint }

  function TDXFPoint.Clone: TDXFEntity;
    begin
      Result := TDXFPoint.Create(X, Y, Z);
    end;

  constructor TDXFPoint.Create(const aX, aY, aZ: real);
    begin
      inherited Create;
      FX := aX;
      FY := aY;
      FZ := aZ;
    end;

  procedure TDXFPoint.Draw(Canvas: TCanvas);
    begin
      Canvas.Pixels[trunc(X), trunc(Y)] := clBlack;
    end;

  procedure TDXFPoint.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFPoint.Scale(const Factor: real);
    begin
      FX := FX * Factor;
      FY := FY * Factor;
      FZ := FZ * Factor;
    end;

  procedure TDXFPoint.Translate(const Point: TPoint);
    begin
      FX := FX + Point.X;
      FY := FY + Point.Y;
      FZ := FZ + Point.Z;
    end;

  function TDXFPoint.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
      Result.AddPoint(TPoint.Create(X, Y, Z));
    end;

{ TDXFCircle }

  function TDXFCircle.Clone: TDXFEntity;
    begin
      Result := TDXFCircle.Create(X, Y, Z, Rad); 
    end;

  constructor TDXFCircle.Create(const aX, aY, aZ, aRad: real);
    begin
      inherited Create;
      FX := aX;
      FY := aY;
      FZ := aZ;
      FRad := aRad;
    end;

  procedure TDXFCircle.Draw(Canvas: TCanvas);
    var
      X1, X2: real;
      Y1, Y2: real;
    begin
      X1 := X - Rad;
      Y1 := Y + Rad;
      X2 := X + Rad;
      Y2 := Y - Rad;
      Canvas.Brush.Style := bsClear;
      Canvas.Ellipse(trunc(X1), trunc(Y1), trunc(X2), trunc(Y2));
    end;

  procedure TDXFCircle.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFCircle.Scale(const Factor: real);
    begin
      FX := FX * Factor;
      FY := FY * Factor;
      FZ := FZ * Factor;
      FRad := FRad * Factor;
    end;

  procedure TDXFCircle.Translate(const Point: TPoint);
    begin
      FX := FX + Point.X;
      FY := FY + Point.Y;
      FZ := FZ + Point.Z;
    end;

  function TDXFCircle.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
    end;

{ TDXFEllipse }

  function TDXFEllipse.Clone: TDXFEntity;
    begin
      Result := TDXFEllipse.Create(X1, Y1, Z1, X2, Y2, Z2, Rad, StartAng, EndAng);
    end;

  constructor TDXFEllipse.Create(const aX1, aY1, aZ1, aX2, aY2, aZ2, aRad, aStartAng, aEndAng: real);
    begin
      inherited Create;
      FX1 := aX1;
      FY1 := aY1;
      FZ1 := aZ1;
      FX2 := aX2;
      FY2 := aY2;
      FZ2 := aZ2;
      FRad := aRad;
      FStartAng := aStartAng;
      FEndAng := aEndAng;
    end;

  procedure TDXFEllipse.Draw(Canvas: TCanvas);
    var
      Angle: real;
      AngStep: real;
      a, b: real;
      Xp, Yp: integer;
    begin
      AngStep := 0.1;
      Angle := StartAng;
      a := - abs(X1 - X2);
      b := - abs(Y1 - Y2);
      Xp := trunc(X2 + a * cos(Angle));
      Yp := trunc(Y2 + b * sin(Angle));
      Canvas.MoveTo(Xp, Yp);
      Angle := Angle + AngStep;
      while Angle < EndAng do
        begin
          Xp := trunc(X2 + a * cos(Angle));
          Yp := trunc(Y2 + b * sin(Angle));
          Canvas.LineTo(Xp, Yp);
          Angle := Angle + AngStep;
        end;
    end;

  procedure TDXFEllipse.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFEllipse.Scale(const Factor: real);
    begin
      FX1 := FX1 * Factor;
      FY1 := FY1 * Factor;
      FZ1 := FZ1 * Factor;
      FX2 := FX2 * Factor;
      FY2 := FY2 * Factor;
      FZ2 := FZ2 * Factor;
      FRad := FRad * Factor;
    end;

  procedure TDXFEllipse.Translate(const Point: TPoint);
    begin
      FX1 := FX1 + Point.X;
      FY1 := FY1 + Point.Y;
      FZ1 := FZ1 + Point.Z;
      FX2 := FX2 + Point.X;
      FY2 := FY2 + Point.Y;
      FZ2 := FZ2 + Point.Z;
    end;

  function TDXFEllipse.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
    end;

{ TDXFPolyLine }

  function TDXFPolyLine.Clone: TDXFEntity;
    begin
      Result := TDXFPolyLine.Create(LineCount, X, Y, Closed);
    end;

  constructor TDXFPolyLine.Create(const aLineCount: integer; const aX, aY: TRealArray; const aClosed: integer);
    var
      i: integer;
    begin
      inherited Create;
      FLineCount := aLineCount;
      FClosed := aClosed;
      SetLength(FX, LineCount);
      SetLength(FY, LineCount);
      for i := 0 to pred(LineCount) do
        begin
          FX[i] := aX[i];
          FY[i] := aY[i];
        end;
    end;

  destructor TDXFPolyLine.Destroy;
    begin
      Finalize(FX);
      Finalize(FY);
      inherited;
    end;

  procedure TDXFPolyLine.Draw(Canvas: TCanvas);
    var
      i: integer;
    begin
      Canvas.MoveTo(trunc(X[0]), trunc(Y[0]));
      for i := 1 to pred(LineCount) do
        Canvas.LineTo(trunc(X[i]), trunc(Y[i]));
      if Closed = 1
        then Canvas.LineTo(trunc(X[0]), trunc(Y[0]));
    end;

  procedure TDXFPolyLine.Rotate(const Angle: real);
    begin

    end;

  procedure TDXFPolyLine.Scale(const Factor: real);
    var
      i: integer;
    begin
      for i := 0 to pred(LineCount) do
        begin
          FX[i] := FX[i] * Factor;
          FY[i] := FY[i] * Factor;
        end;
    end;

  procedure TDXFPolyLine.Translate(const Point: TPoint);
    var
      i: integer;
    begin
      for i := 0 to pred(LineCount) do
        begin
          FX[i] := FX[i] + Point.X;
          FY[i] := FY[i] + Point.Y;
        end;
    end;

  function TDXFPolyLine.WriteToPoints: TPoints;
    begin
      Result := TPoints(PointsType.Instantiate);
    end;

  { TDXFType }

  function TDXFType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

  function TDXFType.Instantiate: TData;
    begin
      Result := TDXF.CreateFrom(Self);
    end;

initialization

    DXFType := TDXFType.CreateIdent('DXF');
    with SystemBlock do
      Types.AddEntry(DXFType);

end.
