unit CodeEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TCodeEditMoveCaret = procedure(const Col, Row: longint) of object;

  TEditOption = (eoAutoIdentMode, eoBackspaceUnindents, eoSmartTab, eoInsertmode);
  TEditOptions = set of TEditOption;
  TCodeEdit =
    class(TCustomRichEdit)
      private
        FontWidth: longint;
        FontHeight: longint;
        FMoveCaret: TCodeEditMoveCaret;
        FOptions: TEditOptions;
        CurShape1: TBitmap;
        CurShape2: TBitmap;
        FRow: integer;
        FCol: integer;
        FSelecting: boolean;
        FSelectPoint: integer;
        FCheckingHightLight: boolean;
        FFileName: string;
        FFiled: boolean;
        function GetInsertMode: boolean;
        procedure SetOptions(const Value: TEditOptions);
        procedure SetInsertMode(const Value: boolean);
        procedure SetCol(const Value: integer);
        procedure SetRow(const Value: integer);
        procedure SetSelecting(const Value: boolean);
        procedure SetSelectPoint(const Value: integer);
        function GetActualCol: integer;
        function GetActualRow: integer;
        procedure WMSETFOCUS(var Message: TMessage);  message WM_SETFOCUS;
        procedure WMPAINT(var Message: TMessage);  message WM_PAINT;
        // Funciones de edicion
        function Ident: integer;
        procedure InsertTab;
        function DoBackSpaceUnindents: boolean;
        procedure DoBackSpace;
        procedure DoDelete;
        procedure FillRowAndInsert;
        procedure InsertSpaces(const Line: Integer; const Count: integer);
        function IdentCount(const Line: Integer): integer;
        procedure MovePage(const Direction: integer);
        // Utilidades varias
        function LineCount: longint;
        procedure ShapeCaret;
        procedure RemoveRightSpaces;
        function BlankString(const Count: integer): string;
        function FirstNonBlank(const Line: Integer): longint;
        function CurrText: TTextAttributes;
        function CharFromLine(const line: Integer): integer;
        procedure SetCursorShapes;
        procedure UpDateCaretPos;
        procedure SyntaxHighLight;
        procedure DoHighLight(const From, Count: integer);
        procedure CheckSelection(Shift: TShiftState);
        function VisibleLinesCount: integer;
        procedure SetUpdateState(Updating: Boolean);
        procedure UpDateLine(const Line: integer);
        function GetLineEditLength: integer;
        function GetSource: TStrings;
      protected
        procedure KeyDown(var Key: Word; Shift: TShiftState); override;
        procedure KeyPress(var Key: Char); override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure Change; override;
      private
        procedure LocateCaret;
        procedure LocateInsertPoint;
        procedure WriteChar(const Ch: Char);
        property Selecting: boolean read FSelecting write SetSelecting;
        property SelectPoint: integer read FSelectPoint write SetSelectPoint;
        property ActualCol: integer read GetActualCol;
        property ActualRow: integer read GetActualRow;
        property LineEditLength: integer read GetLineEditLength;
      public
        constructor Create(AOwner: TComponent); override;
        procedure UpDateFontSize;
        procedure HighLightLine(const Line: integer);
        procedure CheckHighLightAllText;
        procedure SetLineAttr(const Line: integer; const Color: TColor; const FontStyles: TFontStyles);
        property ReadOnly;
        property OnSelectionChange;
        property Options: TEditOptions read FOptions write SetOptions;
        property InsertMode: boolean read GetInsertMode write setInsertMode;
        property OnChange;
        property OnMoveCaret: TCodeEditMoveCaret read FMoveCaret write FMoveCaret;
        property Col: integer read FCol write SetCol;
        property Row: integer read FRow write SetRow;
        property Source: TStrings read GetSource;
        property FileName: string read FFileName write FFileName;
        property Filed: boolean read FFiled write FFiled;
    end;

  procedure Register;

implementation

  uses RichEdit, ShellAPI, ReInit, Parser;

  const
    MAXCARPERLINE = 255;
    //  TTokenType = (ttSymbol, ttInteger, ttFloat, ttString, ttKeyWord, ttComment, ttUnknow);
    TokenColor: array[TTokenType] of TColor = (clBlack, clBlue, clBlue, clRed, clBlack, clGray, clAqua);
    TokenStyle: array[TTokenType] of TFontStyles = ([], [], [], [], [fsBold], [fsItalic], []);

  var
    Scanner: TScanner;

  procedure Register;
    begin
      RegisterComponents('Gospel', [TCodeEdit]);
    end;

  function Max(const A, B: variant): variant;
    begin
      if A > B
        then Result := A
        else Result := B;
    end;

  function Min(const A, B: longint): longint;
    begin
      if A < B
        then Result := A
        else Result := B;
    end;

  { TCodeEdit }

  procedure TCodeEdit.SetCursorShapes;
    begin
      with CurShape1 do
        begin
          Width := FontWidth;
          Height := FontHeight;
          with Canvas do
            begin
              Brush.Color := clWhite;
              FillRect(Rect(0, 0, Width, Height));
              Brush.Color := clBlack;
              Rectangle(0, 0, Width, MulDiv(4, Height, 5));
            end;
        end;
      with CurShape2 do
        begin
          Width := FontWidth;
          Height := FontHeight;
          with Canvas do
            begin
              Brush.Color := clWhite;
              FillRect(Rect(0, 0, Width, Height));
              Brush.Color := clBlack;
              Rectangle(0, 0, Width, MulDiv(3, Height, 5));
            end;
        end;
    end;

  // Devuelve el indice del primer caracter de una linea

  function TCodeEdit.CharFromLine(const line: integer): integer;
    begin
      Result := SendMessage(Handle, EM_LINEINDEX, line, 0);
    end;

  function TCodeEdit.IdentCount(const Line: longint): integer;

    function GetSpaces(const Ln: longint): integer;
      var
        S: string;
      begin
        Result := 0;
        S := Lines.Strings[Ln];
        while (Result < length(S)) and (S[Result + 1] = #32) do
          inc(Result);
        if Result = length(S)
          then Result := 0;
      end;

    var
      L: integer;
    begin
      L := Line;
      repeat
        Dec(L);
        Result := GetSpaces(L);
      until (Result > 0) or (L = 0) or ((Length(Lines.Strings[L]) > 0)
        and (Lines.Strings[L][1] <> ' '));
    end;

  function TCodeEdit.BlankString(const Count: integer): string;
    begin
      SetLength(Result, Count);
      FillChar(PChar(Result)^, Count, ' ');
    end;

  procedure TCodeEdit.InsertSpaces(const Line: longint; const Count: integer);
    var
      Str: string;
    begin
      Str := Lines.Strings[Line];
      Insert(#13#10 + BlankString(Count), Str, Col);
      Lines.Strings[Line] := Str;
    end;

  function TCodeEdit.Ident: integer;
    var
      Line: longint;
    begin
      // Linea donde se encuentra el cursor
      Line := SendMessage(Handle, EM_LINEFROMCHAR, -1, 0);
      Result := IdentCount(succ(Line));
      InsertSpaces(Line, Result);
    end;

  function TCodeEdit.FirstNonBlank(const Line: longint): longint;
    begin
      if Lines[Line] = ''
        then Result := MAXLONGINT
        else
          begin
            Result := 1;
            while (Result < Length(Lines[Line]))
              and (Lines[Line][Result] = ' ')
                do inc(Result);
          end;
    end;

  function TCodeEdit.DoBackSpaceUnindents: boolean;
    var
      TheRow: longint;
      Line: string;
      index: integer;

    function BlankLine: boolean;
      var
        i: longint;
      begin
        if LineEditLength = 0
          then Result := true
          else
            begin
              i := 1;
              while (i < LineEditLength) and (Lines[ActualRow][i] = ' ')
                do inc(i);
              Result := Lines[ActualRow][i] = ' ';
            end;
      end;

    function CanUnindent: boolean;
      begin
        if Col = 1
          then Result := false
          else
            if BlankLine
              then Result := true
              else
                if ActualCol > LineEditLength
                  then Result := false
                  else
                    if Col = FirstNonBlank(ActualRow)
                      then Result := true
                      else Result := false;
      end;

    begin
      Result := CanUnindent;
      if Result
        then
          begin
            RemoveRightSpaces;
            TheRow := pred(ActualRow);
            index := FirstNonBlank(TheRow);
            while ActualCol <= index do
              begin
                dec(TheRow);
                index := FirstNonBlank(TheRow);
              end;
            Line := Lines[ActualRow];
            Lines[ActualRow] := BlankString(pred(index)) +
              Copy(Line, ActualCol + 1, Length(Line));
            Col := index;
          end;
    end;

  procedure TCodeEdit.InsertTab;
    var
      TheRow: longint;
      Line: string;
      index: integer;
      Count: integer;
    begin
      TheRow := pred(ActualRow);
      while (TheRow >= 0) and (ActualCol >= Length(Lines[TheRow]))
        do dec(TheRow);
      if TheRow >= 0
        then
          begin
            Line := Lines[TheRow];
            index := Col;
            while (index < Length(Line)) and not ((Line[index] = ' ') and (Line[succ(index)] <> ' '))
              do inc(index);
            Line := Lines[ActualRow];
            if Length(Line) < ActualCol
              then Count := Length(Line)
              else Count := ActualCol;
            Insert(BlankString(Index - Count), Line, Col);
            Lines[ActualRow] := Line;
            Col := succ(Index);
          end;
    end;

  procedure TCodeEdit.RemoveRightSpaces;
    var
      i: integer;
      S: string;
    begin
      S := Lines[ActualRow];
      i := Length(S);
      if i > 0
        then
          begin
            while (i > 0) and (S[i] = #32) do dec(i);
            if i < Length(S)
              then Lines[ActualRow] := Copy(S, 0, i);
          end;
    end;

  procedure TCodeEdit.ShapeCaret;
    begin
      {
      if ActualCol > LineEditLength
        then CreateCaret(Handle, CurShape2.Handle, 0, 0)
        else CreateCaret(Handle, CurShape1.Handle, 0, 0);
      ShowCaret(Handle);
      }
    end;

  procedure TCodeEdit.FillRowAndInsert;
    var
      Count: integer;
    begin
      Count := ActualCol - LineEditLength;
      Lines[ActualRow] := Lines[ActualRow] + BlankString(Count);
    end;

  function TCodeEdit.LineCount: longint;
    begin
      Result := SendMessage(Handle, EM_GETLINECOUNT, 0, 0);
    end;

  function TCodeEdit.CurrText: TTextAttributes;
    begin
      if SelLength > 0 then Result := SelAttributes
      else Result := DefAttributes;
    end;

  procedure TCodeEdit.UpDateFontSize;
    var
      BM: TBitmap;
    begin
      BM := TBitmap.Create;
      with BM.Canvas do
        begin
          Font.Name := CurrText.Name;
          Font.Size := CurrText.Size;
          FontWidth := TextWidth('a');
          FontHeight := TextHeight('a');
        end;
      BM.free;
      if (CurShape1 <> nil) and (CurShape2 <> nil)
        then SetCursorShapes;
    end;

  constructor TCodeEdit.Create(AOwner: TComponent);
    begin
      inherited;
      FOptions   := [eoInsertMode, eoAutoIdentMode, eoBackspaceUnindents, eoSmartTab];
      CurShape1  := TBitmap.Create;
      CurShape2  := TBitmap.Create;
      WantTabs   := true;
      WordWrap   := false;
      PlainText  := true;
      ScrollBars := ssBoth;
      FCol       := 1;
      FRow       := 1;
    end;

  procedure TCodeEdit.WMSETFOCUS(var Message: TMessage);
    begin
      inherited;
      UpDateFontSize;
      //CreateCaret(WindowHandle, CurShape1.Handle, 0, 0);
      //ShowCaret(WindowHandle);
    end;

  procedure TCodeEdit.CheckSelection(Shift: TShiftState);
    begin
      if ssShift in Shift
        then Selecting := true
        else Selecting := false;
    end;

  procedure TCodeEdit.KeyDown(var Key: Word; Shift: TShiftState);
    begin
      inherited;
      case Key of
        VK_Left:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then
                begin
                  SelStart := SendMessage(Handle, EM_FINDWORDBREAK, WB_MOVEWORDLEFT, SelStart);
                  UpDateCaretPos;
                end
              else Col := Col - 1;
          end;
        VK_Right:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then
                begin
                  SelStart := SendMessage(Handle, EM_FINDWORDBREAK, WB_MOVEWORDRIGHT, SelStart);
                  UpDateCaretPos;
                end
              else Col := Col + 1;
          end;
        VK_Home:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then Row := succ(SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0))
              else Col := 1;
          end;
        VK_End:
          begin
            CheckSelection(Shift);
            RemoveRightSpaces;
            if ssCtrl in Shift
              then Row := SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0) + pred(VisibleLinesCount)
              else Col := succ(LineEditLength);
          end;
        VK_Up:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then SendMessage(Handle, EM_SCROLL, SB_LINEUP, 0);
            Row := Row - 1;
          end;
        VK_Down:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then SendMessage(Handle, EM_SCROLL, SB_LINEDOWN, 0);
            Row := Row + 1;
          end;
        VK_NEXT:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then
                begin
                  Row := LineCount;
                  Col := succ(LineEditLength);
                end
              else MovePage(SB_PAGEDOWN);
          end;
        VK_PRIOR:
          begin
            CheckSelection(Shift);
            if ssCtrl in Shift
              then
                begin
                  Row := 1;
                  Col := 1;
                end
              else MovePage(SB_PAGEUP);
          end;
        VK_Insert:
          if Shift = []
            then InsertMode := not InsertMode;
        VK_TAB:
          if eoSmartTab in Options
            then InsertTab;
        VK_BACK:
          begin
            if not ((eoBackspaceUnindents in Options) and DoBackSpaceUnindents)
              then DoBackSpace;
          end;
        VK_DELETE:
          begin
            DoDelete;
          end;
      end;
      if Key in [VK_Left, VK_Right, VK_Home, VK_End, VK_Up, VK_Down,
                 VK_NEXT, VK_PRIOR, VK_TAB, VK_BACK, VK_DELETE]
        then Key := 0;
    end;

  procedure TCodeEdit.KeyPress(var Key: Char);
    begin
      inherited;
      if (Key = #13) and (eoAutoIdentMode in Options)
        then
          begin
            Col := succ(Ident);
            Row := Row + 1;
            SyntaxHighLight;
            UpDateLine(pred(ActualRow));
            Key := #0;
          end;
      if (Key = #9) and (eoSmartTab in Options)
        then Key := #0;
      if Key >= #32
        then
          begin
            WriteChar(Key);
            Key := #0;
          end;
    end;

  procedure TCodeEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
    Y: Integer);
    var
      A: TPoint;
      P: TPoint;
    begin
      CheckSelection(Shift);
      if Button = mbLeft
        then
          begin
            SendMessage(Handle, EM_POSFROMCHAR, longint(@A), 0);
            P.X := X + FontWidth div 2 - (X - A.X + FontWidth div 2) mod FontWidth;
            P.Y := Y - (Y - A.Y) mod FontHeight;
            //SetCaretPos(P.X, P.Y);
            Col := succ((P.X - A.X) div FontWidth);
            Row := succ((P.Y - A.Y) div FontHeight);
          end;
    end;

  procedure TCodeEdit.SetOptions(const Value: TEditOptions);
    begin
      FOptions := Value;
    end;

  function TCodeEdit.GetInsertMode: boolean;
    begin
      Result := eoInsertmode in Options;
    end;

  procedure TCodeEdit.setInsertMode(const Value: boolean);
    begin
      if Value
        then FOptions := FOptions + [eoInsertMode]
        else FOptions := FOptions - [eoInsertMode];
    end;

  procedure TCodeEdit.SetCol(const Value: integer);
    begin
      if (Value > 0) and (Value < MAXCARPERLINE) and (Value <> FCol)
        then
          begin
            FCol := Value;
            LocateCaret;
            if Assigned(FMoveCaret)
              then FMoveCaret(Col, Row);
          end;
    end;

  procedure TCodeEdit.SetRow(const Value: integer);
    begin
      if (Value > 0) and (Value <= LineCount) and (Value <> FRow)
        then
          begin
            RemoveRightSpaces;
            FRow := Value;
            LocateCaret;
            if Assigned(FMoveCaret)
              then FMoveCaret(Col, Row);
          end;
    end;

  procedure TCodeEdit.LocateCaret;
    var
      A: TPoint;
    begin
      LocateInsertPoint;
      // Poner en A el margen superior y el margen derecho
      SendMessage(Handle, EM_POSFROMCHAR, longint(@A), 0);
      // Calcular la posicion donde debe ir el cursor y situarlo
      SetCaretPos(A.X + FontWidth * ActualCol, A.Y + FontHeight * ActualRow);
      ShapeCaret;
    end;

  procedure TCodeEdit.LocateInsertPoint;
    var
      CharRange: TCharRange;
      CarPos: integer;
    begin
      CarPos := CharFromLine(ActualRow) + Min(ActualCol, LineEditLength);
      if Selecting
        then
          begin
            CharRange.cpMin := Min(CarPos, SelectPoint);
            CharRange.cpMax := Max(CarPos, SelectPoint);
          end
        else
          begin
            CharRange.cpMin := CarPos;
            CharRange.cpMax := CarPos;
          end;
      SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
    end;

  procedure TCodeEdit.WriteChar(const Ch: Char);
    var
      Text: PChar;
      UpDate: boolean;
    begin
      if ActualCol > LineEditLength
        then FillRowAndInsert;
      UpDate := SelLength > 0;
      Text := PChar(Ch);
      SendMessage(Handle, EM_REPLACESEL, integer(True), integer(@Text));
      if UpDate
        then
          begin
            Selecting := false;
            UpDateCaretPos;
          end
        else Col := Col + 1;
    end;

  procedure TCodeEdit.SetSelecting(const Value: boolean);
    begin
      if Value and not Selecting
        then SelectPoint := SelStart;
      FSelecting := Value;
    end;

  procedure TCodeEdit.SetSelectPoint(const Value: integer);
    begin
      FSelectPoint := Value;
    end;

  function TCodeEdit.GetActualCol: integer;
    begin
      Result := pred(Col);
    end;

  function TCodeEdit.GetActualRow: integer;
    begin
      Result := pred(Row);
    end;

  procedure TCodeEdit.DoBackSpace;
    var
      CharRange: TCharRange;
    begin
      if ActualCol > LineEditLength
        then Col := Col -1
        else
          begin
            SendMessage(Handle, EM_EXGETSEL, 0, Longint(@CharRange));
            if CharRange.cpMin = CharRange.cpMax
              then
                begin
                  CharRange.cpMin := pred(CharRange.cpMax);
                  SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
                end;
            Selecting := false;
            SendMessage(Handle, EM_REPLACESEL, integer(True), integer(PChar('')));
            UpDateCaretPos;
          end;
    end;

  procedure TCodeEdit.DoDelete;
    var
      CharRange: TCharRange;
    begin
      if Col > LineEditLength
        then FillRowAndInsert;
      SendMessage(Handle, EM_EXGETSEL, 0, Longint(@CharRange));
      if CharRange.cpMin = CharRange.cpMax
        then
          begin
            if ActualCol = LineEditLength
              then CharRange.cpMax := CharRange.cpMin + 2
              else CharRange.cpMax := succ(CharRange.cpMin);
            SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
          end;
      Selecting := false;
      SendMessage(Handle, EM_REPLACESEL, integer(True), integer(PChar('')));
      UpDateCaretPos;
    end;

  procedure TCodeEdit.UpDateCaretPos;
    var
      CarPos: integer;
    begin
      CarPos := SelStart;
      Row := succ(SendMessage(Handle, EM_EXLINEFROMCHAR, 0, CarPos));
      Col := succ(CarPos - SendMessage(Handle, EM_LINEINDEX, ActualRow, 0));
      ShapeCaret;
    end;

  procedure TCodeEdit.DoHighLight(const From, Count: integer);
    var
      CharRange: TCharRange;
      ScanResult: boolean;
      CaretPos: integer;
    begin
      FCheckingHightLight := true;
      CaretPos := SelStart;
      Scanner.Source := Text;
      Scanner.Index := From;
      SetUpDateState(true);
      repeat
        ScanResult := Scanner.NextToken;
        if ScanResult
          then
            begin
              CharRange.cpMin := pred(Scanner.Position);
              CharRange.cpMax := CharRange.cpMin + Length(Scanner.Token);
              SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
              SelAttributes.Color := TokenColor[Scanner.TokenType];
              SelAttributes.Style := TokenStyle[Scanner.TokenType];
            end;
      until (Scanner.Index >= From + Count) or not ScanResult;
      LocateCaret;
      SelStart := CaretPos;
      SetUpDateState(false);
      FCheckingHightLight := false;
    end;

  procedure TCodeEdit.SyntaxHighLight;
    begin
      HighLightLine(Row); 
    end;

  procedure TCodeEdit.CheckHighLightAllText;
    begin
      Lines.BeginUpdate;
      DoHighLight(1, SendMessage(Handle, WM_GETTEXTLENGTH, 0, 0));
      Lines.EndUpdate;
    end;

  procedure TCodeEdit.Change;
    begin
      if not FCheckingHightLight
        then SyntaxHighLight;
      inherited;
    end;

  procedure TCodeEdit.WMPAINT(var Message: TMessage);
    begin
      inherited;
      ShapeCaret;
    end;

  procedure TCodeEdit.MovePage(const Direction: integer);
    var
      L, R: integer;
    begin
      L := Row - succ(SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0));
      SendMessage(Handle, EM_SCROLL, Direction, 0);
      R := succ(SendMessage(Handle, EM_GETFIRSTVISIBLELINE, 0, 0));
      if (R + L > LineCount) or ((R = 1) and (L > 0))
        then Row := R
        else Row := R + L;
    end;

  function TCodeEdit.VisibleLinesCount: integer;
    var
      A: TPoint;
    begin
      SendMessage(Handle, EM_POSFROMCHAR, longint(@A), 0);
      Result := (Height - A.y) div FontHeight;
    end;


  procedure TCodeEdit.SetUpdateState(Updating: Boolean);
    begin
      if Showing
        then SendMessage(Handle, WM_SETREDRAW, Ord(not Updating), 0);
      if not Updating
        then
          begin
            {
            Refresh;
            Perform(CM_TEXTCHANGED, 0, 0);
            }
            UpDateLine(ActualRow);
          end;
    end;

  function TCodeEdit.GetLineEditLength: integer;
    begin
      Result := SendMessage(Handle, EM_LINELENGTH,
                SendMessage(Handle, EM_LINEINDEX, ActualRow, 0), 0);
    end;

  procedure TCodeEdit.UpDateLine(const Line: integer);
    var
      R: TRect;
      A, B: TPoint;
      CharLine: integer;
    begin
      CharLine := SendMessage(Handle, EM_LINEINDEX, Line, 0);
      SendMessage(Handle, EM_POSFROMCHAR, longint(@A), CharLine);
      SendMessage(Handle, EM_POSFROMCHAR, longint(@B), CharLine +
      SendMessage(Handle, EM_LINELENGTH, CharLine, 0));
      R := Rect(A.x, A.y, B.x + FontWidth, succ(A.y + FontHeight));
      RedrawWindow(Handle, @R, 0, RDW_INVALIDATE);
    end;

  procedure TCodeEdit.SetLineAttr(const Line: integer; const Color: TColor; const FontStyles: TFontStyles);
    var
      CharRange: TCharRange;
      CaretPos: integer;
    begin
      CharRange.cpMin := SendMessage(Handle, EM_LINEINDEX, pred(Line), 0);
      if CharRange.cpMin > 0
        then
          begin
            FCheckingHightLight := true;
            CharRange.cpMax := CharRange.cpMin + SendMessage(Handle, EM_LINELENGTH, CharRange.cpMin, 0);
            CaretPos := SelStart;
            SendMessage(Handle, WM_SETREDRAW, Ord(False), 0);
            SendMessage(Handle, EM_EXSETSEL, 0, Longint(@CharRange));
            SelAttributes.Color := Color;
            SelAttributes.Style := FontStyles;
            LocateCaret;
            SelStart := CaretPos;
            SendMessage(Handle, WM_SETREDRAW, Ord(True), 0);
            Refresh;
            Perform(CM_TEXTCHANGED, 0, 0);
            FCheckingHightLight := false;
          end;
    end;

  procedure TCodeEdit.HighLightLine(const Line: integer);
    var
      Len: integer;
      Pos: integer;
    begin
      Pos := succ(SendMessage(Handle, EM_LINEINDEX, pred(Line), 0));
      if Pos > 0
        then
          begin
            Len := SendMessage(Handle, EM_LINELENGTH, Pos, 0);
            DoHighLight(Pos, Len);
          end;
    end;

  function TCodeEdit.GetSource: TStrings;
    begin
      Result := Lines;
    end;

initialization
  Scanner   := TScanner.Create;
  Scanner.SkipComments := false;
  Scanner.QuietErrors := true;

finalization
  Scanner.free;

end.
