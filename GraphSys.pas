
{*******************************************************}
{                                                       }
{       Gospel Compiler System                          }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GraphSys;

interface

  uses Windows, Classes, SysUtils, Kernel, Graphics;

  type
    TPoint =
      class
        private
          FY: real;
          FX: real;
          FZ: real;
        public
          constructor Create(const aX, aY, aZ: real);
          property X: real read FX;
          property Y: real read FY;
          property Z: real read FZ;
      end;

    TPointsType =
      class(TDataType)
        public
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TPoints =
      class(TData)
        private
          FPoints: TList;
          fColor: TColor;
          function GeTDXFPoint(const i: integer): TPoint;
          function GetPointCount: integer;
        public
          constructor CreateFrom(aDataType: TDataType); override;
          destructor Destroy;  override;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
        public
          property Point[const i: integer]: TPoint read GeTDXFPoint;
          property Color: TColor read fColor write fColor;
          property PointCount: integer read GetPointCount;
          procedure Concat(Points: TPoints);
          procedure AddPoint(aPoint: TPoint);
          procedure Clear;
          procedure Draw(Canvas: TCanvas);
          function WriteToBitmap: TBitmap;
      end;

    TBmpType =
      class(TDataType)
        public
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TBmp =
      class(TData)
        private
          FBitmap: TBitmap;
        public
          constructor CreateFrom(aDataType: TDataType); override;
          destructor Destroy;  override;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function LoadFromFile(const FileName: TFileName): boolean;
          procedure SaveToFile(const FileName: TFileName);
          property Bitmap: TBitmap read FBitmap;
        public
          procedure Draw(Canvas: TCanvas);
          function WriteToPoints(const Color: TColor): TPoints;
          procedure GrayLevel;
          procedure Umbralize(const Umbral: Integer);
      end;

  var
    BitmapType: TBmpType;
    PointsType: TPointsType;

implementation

  uses Dialogs, Constants;

{ TPoint }

  constructor TPoint.Create(const aX, aY, aZ: real);
    begin
      inherited Create;
      FX := aX;
      FY := aY;
      FZ := aZ;
    end;

{ TPoints }

  procedure TPoints.AddPoint(aPoint: TPoint);
    begin
      FPoints.Add(aPoint);
    end;

  procedure TPoints.Assign(Data: TData);
    begin
      if Data is TPoints
        then
          begin
            Clear;
            Concat(TPoints(Data));
          end;
    end;

  procedure TPoints.Clear;
    var
      i: integer;
    begin
      for i := 0 to pred(FPoints.Count) do
        TPoint(FPoints.Items[i]).free;
      FPoints.Clear;
    end;

  procedure TPoints.Concat(Points: TPoints);
    var
      i: integer;
    begin
      for i := 0 to pred(Points.PointCount) do
        with Points.Point[i] do
          AddPoint(TPoint.Create(X, Y, Z));
    end;

  constructor TPoints.CreateFrom(aDataType: TDataType);
    begin
      inherited;
      FPoints := TList.Create;
      FColor := clBlack;
    end;

  destructor TPoints.Destroy;
    begin
      FPoints.free;
      inherited;
    end;

  procedure TPoints.Draw(Canvas: TCanvas);
    var
      i: integer;
    begin
      for i := 0 to pred(FPoints.Count) do
        Canvas.Pixels[trunc(Point[i].X), trunc(Point[i].Y)] := Color;
    end;

  function TPoints.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      Result.Assign(Self);
    end;

  function TPoints.GeTDXFPoint(const i: integer): TPoint;
    begin
      Result := FPoints.Items[i];
    end;

  function TPoints.GetPointCount: integer;
    begin
      Result := FPoints.Count;
    end;

  function TPoints.WriteToBitmap: TBitmap;
    var
      i: integer;
    begin
      Result := TBitmap.Create;
      for i := 0 to pred(FPoints.Count) do
        with Point[i] do
          Result.Canvas.Pixels[trunc(X), trunc(Y)] := Color;
    end;

{ TBmpType }

  function TBmpType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

  function TBmpType.Instantiate: TData;
    begin
      Result := TBmp.CreateFrom(Self);
    end;

{ TBmp }

  procedure TBmp.Assign(Data: TData);
    begin
      if Data is TBmp
        then FBitmap.Assign(TBmp(Data).Bitmap);
    end;

  constructor TBmp.CreateFrom(aDataType: TDataType);
    begin
      inherited;
      FBitmap := TBitmap.Create;
    end;

  destructor TBmp.Destroy;
    begin
      FBitmap.free;
      inherited;
    end;

  procedure TBmp.Draw(Canvas: TCanvas);
    begin
      Canvas.Draw(0, 0, FBitmap);
    end;

  function TBmp.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      Result.Assign(Self);
    end;

  procedure TBmp.GrayLevel;
    var
      x, y: Integer;
      ImageDst: TBitmap;
      R, G, B, Inten: DWORD;   //RGB and Intensity
      Color: COLORREF;
    begin
      ImageDst := TBitmap.Create;
      ImageDst.PixelFormat := pf8bit;
      ImageDst.ReleaseMaskHandle;
      ImageDst.Width := Bitmap.Width;
      ImageDst.Height := Bitmap.Height;
      for x := 0  to pred(Bitmap.Width) do
        for y := 0  to pred(Bitmap.Height) do
          begin
            Color := GetPixel(Bitmap.Canvas.Handle, x, y);
            R := GetRValue(Color);
            G := GetGValue(Color);
            B := GetBValue(Color);
            Inten := Round((R + B + G) / 3);  //GrayLevel
            ImageDst.Canvas.Pixels[x, y] := RGB(Inten, Inten, Inten);
          end;
      FBitmap := ImageDst;
      ImageDst.FreeImage;
    end;

  function TBmp.LoadFromFile(const FileName: TFileName): boolean;
    begin
      Result := false;
      try
        FBitmap.LoadFromFile(FileName);
        Result := true;
      except
        on EFOpenError do
          MessageDlg(Format(emFileNotFound, [FileName]), mtError, [mbOK], 0);
      end;
    end;

  procedure TBmp.SaveToFile(const FileName: TFileName);
    begin
      FBitmap.SaveToFile(FileName);
    end;

  procedure TBmp.Umbralize(const Umbral: Integer);
    var
      x, y: Integer;
      R, G, B, Inten: DWORD;   //RGB and Intensity
      Color: COLORREF;
    begin
      for x := 0  to pred(Bitmap.Width) do
        for y := 0  to pred(Bitmap.Height) do
          begin
            Color := GetPixel(Bitmap.Canvas.Handle, x, y);
            R := GetRValue(Color);
            G := GetGValue(Color);
            B := GetBValue(Color);
            Inten := Round((R + B + G) / 3);  //GrayLevel
            if Inten <= DWORD(Umbral)
              then Bitmap.Canvas.Pixels[x, y] := ClBlack
              else Bitmap.Canvas.Pixels[x, y] := ClWhite;
          end;
    end;

  function TBmp.WriteToPoints(const Color: TColor): TPoints;
    var
      x, y: Integer;
    begin
      Result := TPoints(PointsType.Instantiate);
      for y := 0  to pred(Bitmap.Height) do
        for x := 0 to pred(Bitmap.Width) do
          if Bitmap.Canvas.Pixels[x, y] = Color
            then Result.AddPoint(TPoint.Create(x, y, 0));
    end;

{ TPointsType }

  function TPointsType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

  function TPointsType.Instantiate: TData;
    begin
      Result := TPoints.CreateFrom(Self);
    end;

initialization

    BitmapType := TBmpType.CreateIdent('Bitmap');
    PointsType := TPointsType.CreateIdent('Points');
    with SystemBlock do
      begin
        Types.AddEntry(BitmapType);
        Types.AddEntry(PointsType);
      end;

end.
