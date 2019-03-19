
{*******************************************************}
{                                                       }
{       Gospel Built-in Routine Library                 }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GospelMovement;

interface

  uses Kernel;

  type
    TMoveCarStatement =
      class(TProcedure)
        private
          function GetParam(const i: integer): real;
        public
          constructor Create;
          procedure Execute; override;
          property Param[const i: integer]: real read GetParam;
      end;

    TMoveAngStatement =
      class(TProcedure)
        private
          function GetParam(const i: integer): real;
        public
          constructor Create;
          procedure Execute; override;
          property Param[const i: integer]: real read GetParam;
      end;

implementation

{ TMoveCarStatement }

  constructor TMoveCarStatement.Create;
    var
      Data: TData;
    begin
      inherited;
      Name := 'MOVECAR';
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
      Data :=  RealType.Instantiate;
      Data.Name := 'Phi';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Cita';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Psi';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TMoveCarStatement.Execute;
    begin
      if Assigned(MainHandler.MoveCarProc)
        then MainHandler.MoveCarProc(Param[1], Param[2], Param[3], Param[4], Param[5], Param[6]);
    end;

  function TMoveCarStatement.GetParam(const i: integer): real;
    begin
      Result := TReal(Parameters.Items[pred(i)]).Value;
    end;

{ TMoveAngStatement }

  constructor TMoveAngStatement.Create;
    var
      Data: TData;
    begin
      inherited Create;
      Name := 'MOVEANG';
      Data :=  RealType.Instantiate;
      Data.Name := 'Q1';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Q2';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Q3';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Q4';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'Q5';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  procedure TMoveAngStatement.Execute;
    begin
      if Assigned(MainHandler.MoveAngProc)
        then MainHandler.MoveAngProc(Param[1], Param[2], Param[3], Param[4], Param[5]);
    end;

  function TMoveAngStatement.GetParam(const i: integer): real;
    begin
      Result := TReal(Parameters.Items[pred(i)]).Value;
    end;

  initialization

    with SystemBlock do
      begin
        Procedures.AddEntry(TMoveAngStatement.Create);
        Procedures.AddEntry(TMoveCarStatement.Create);
      end;
      
end.
