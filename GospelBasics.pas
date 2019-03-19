
{*******************************************************}
{                                                       }
{       Gospel Built-in Routine Library                 }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GospelBasics;

interface

  uses Kernel;

  type
    TOrd =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TPred =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TSucc =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    THigh =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TLow =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    THighIndex =
      class(TFunction)
        private
          ParamType: TFloppyParamType;
        public
          constructor Create;
          destructor Destroy;  override;
          procedure Execute; override;
      end;

    TLowIndex =
      class(TFunction)
        private
          ParamType: TFloppyParamType;
        public
          constructor Create;
          destructor Destroy;  override;
          procedure Execute; override;
      end;

    TRound =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TTrunc =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TInt =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TSqr =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;

    TSqrt =
      class(TFunction)
        public
          constructor Create;
          procedure Execute; override;
      end;


implementation

{ TOrd }

  constructor TOrd.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Ord';
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Int';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TOrd.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := TInteger(Items[0]).Value;
    end;

{ TPred }

  constructor TPred.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Pred';
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Int';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TPred.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := Pred(TInteger(Items[0]).Value);
    end;

{ TSucc }

  constructor TSucc.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Succ';
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Int';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TSucc.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := Succ(TInteger(Items[0]).Value);
    end;

{ THigh }

  constructor THigh.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'High';
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Int';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure THigh.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := TInteger(Items[0]).High;
    end;

{ TLow }

  constructor TLow.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Low';
      Data :=  IntegerType.Instantiate;
      Data.Name := 'Int';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TLow.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := TInteger(Items[0]).Low;
    end;

{ THighIndex }

  constructor THighIndex.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'HighIndex';
      ParamType := TFloppyParamType.Create(TArrayType);
      Data :=  ParamType.Instantiate;
      Data.Name := 'Arr';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  destructor THighIndex.Destroy;
    begin
      ParamType.free;
      inherited;
    end;

  procedure THighIndex.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := TArray(TFloppyParam(Items[0]).Parameter).ArrayType.Dimention.High;
    end;

{ TLowIndex }

  constructor TLowIndex.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'LowIndex';
      ParamType := TFloppyParamType.Create(TArrayType);
      Data :=  ParamType.Instantiate;
      Data.Name := 'Arr';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  destructor TLowIndex.Destroy;
    begin
      ParamType.free;
      inherited;
    end;

  procedure TLowIndex.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := TArray(TFloppyParam(Items[0]).Parameter).ArrayType.Dimention.Low;
    end;

{ TRound }

  constructor TRound.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Round';
      Data :=  RealType.Instantiate;
      Data.Name := 'Value';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TRound.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := Round(TReal(Items[0]).Value);
    end;

{ TTrunc }

  constructor TTrunc.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Trunc';
      Data :=  RealType.Instantiate;
      Data.Name := 'Value';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := IntegerType.Instantiate;
    end;

  procedure TTrunc.Execute;
    begin
      with Parameters do
        TInteger(Result).Value := Trunc(TReal(Items[0]).Value);
    end;

{ TInt }

  constructor TInt.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Int';
      Data :=  RealType.Instantiate;
      Data.Name := 'Value';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := RealType.Instantiate;
    end;

  procedure TInt.Execute;
    begin
      with Parameters do
        TReal(Result).Value := Int(TReal(Items[0]).Value);
    end;

{ TSqr }

  constructor TSqr.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Sqr';
      Data :=  RealType.Instantiate;
      Data.Name := 'Value';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := RealType.Instantiate;
    end;

  procedure TSqr.Execute;
    begin
      with Parameters do
        TReal(Result).Value := Sqr(TReal(Items[0]).Value);
    end;

{ TSqrt }

  constructor TSqrt.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'Sqrt';
      Data :=  RealType.Instantiate;
      Data.Name := 'Value';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Result := RealType.Instantiate;
    end;

  procedure TSqrt.Execute;
    begin
      with Parameters do
        TReal(Result).Value := Sqrt(TReal(Items[0]).Value);
    end;

initialization

    with SystemBlock do
      begin
        Functions.AddEntry(TOrd.Create);
        Functions.AddEntry(TPred.Create);
        Functions.AddEntry(TSucc.Create);
        Functions.AddEntry(THigh.Create);
        Functions.AddEntry(TLow.Create);
        Functions.AddEntry(THighIndex.Create);
        Functions.AddEntry(TLowindex.Create);
        Functions.AddEntry(TTrunc.Create);
        Functions.AddEntry(TRound.Create);
        Functions.AddEntry(TInt.Create);
        Functions.AddEntry(TSqr.Create);
        Functions.AddEntry(TSqrt.Create);
      end;

end.
