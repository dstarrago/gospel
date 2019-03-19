
{*******************************************************}
{                                                       }
{       Gospel Built-in Routine Library                 }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit GospelMath;

interface

  uses Kernel;

  type
    TSin =
      class(TSingleMathFunction)
        public
          procedure Execute; override;
      end;

    TCos =
      class(TSingleMathFunction)
        public
          procedure Execute; override;
      end;

    TTan =
      class(TSingleMathFunction)
        public
          procedure Execute; override;
      end;

    TArcTan =
      class(TSingleMathFunction)
        public
          procedure Execute; override;
      end;

    TArcTan2 = 
      class(TBinaryMathFunction)
        public
          procedure Execute; override;
      end;

implementation

  uses Math;

{ TSin }

  procedure TSin.Execute;
    begin
      TReal(Result).Value := Sin(TReal(Parameters.Items[0]).Value);
    end;

{ TCos }

  procedure TCos.Execute;
    begin
      TReal(Result).Value := Cos(TReal(Parameters.Items[0]).Value);
    end;

{ TTan }

  procedure TTan.Execute;
    begin
      TReal(Result).Value := Tan(TReal(Parameters.Items[0]).Value);
    end;

{ TArcTan }

  procedure TArcTan.Execute;
    begin
      TReal(Result).Value := Arctan(TReal(Parameters.Items[0]).Value);
    end;

{ TArcTan2 }

  procedure TArcTan2.Execute;
    begin
      TReal(Result).Value := Arctan2(TReal(Parameters.Items[0]).Value,
                             TReal(Parameters.Items[1]).Value);
    end;

  initialization

    with SystemBlock do
      begin
        Functions.AddEntry(TSin.CreateIdent('SIN'));
        Functions.AddEntry(TCos.CreateIdent('COS'));
        Functions.AddEntry(TTan.CreateIdent('TAN'));
        Functions.AddEntry(TArcTan.CreateIdent('ARCTAN'));
        Functions.AddEntry(TArcTan2.CreateIdent('ARCTAN2'));
      end;
      
end.
