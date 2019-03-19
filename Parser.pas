
{*******************************************************}
{                                                       }
{       Gospel Compiler System                          }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit Parser;

interface

  uses Classes, SysUtils, Kernel, StdCtrls, Constants, SourceManage;

  const
    Delimiters = ',.;:<>=+-*/()[]{}''"!@#$%^&|';
    Blanks     = ' '#13#10#9;

    // Reserved words

    rwAnd            = 'AND';
    rwArray          = 'ARRAY';
    rwBegin          = 'BEGIN';
    rwColVector      = 'COLVECTOR';
    rwConst          = 'CONST';
    rwDo             = 'DO';
    rwDownto         = 'DOWNTO';
    rwElse           = 'ELSE';
    rwEnd            = 'END';
    rwFile           = 'FILE';
    rwFor            = 'FOR';
    rwFunction       = 'FUNCTION';
    rwIf             = 'IF';
    rwInitialization = 'INITIALIZATION';
    rwInvertion      = 'INV';
    rwMatrix         = 'MATRIX';
    rwMod            = 'MOD';
    rwModule         = 'MODULE';
    rwNot            = 'NOT';
    rwOf             = 'OF';
    rwOr             = 'OR';
    rwOutput         = 'OUTPUT';
    rwProcedure      = 'PROCEDURE';
    rwProgram        = 'PROGRAM';
    rwRecord         = 'RECORD';
    rwRowVector      = 'ROWVECTOR';
    rwRepeat         = 'REPEAT';
    rwThen           = 'THEN';
    rwTo             = 'TO';
    rwType           = 'TYPE';
    rwUntil          = 'UNTIL';
    rwUses           = 'USES';
    rwVar            = 'VAR';
    rwWhile          = 'WHILE';
    rwXor            = 'XOR';

  type
    TTokenType = (ttSymbol, ttInteger, ttFloat, ttString, ttKeyWord, ttComment, ttUnknow);
    EScanner = class(Exception);
    EParser  = class(Exception);

    TScanner =
      class
        private
          SavePos: integer;
          SaveToken: string;
          SaveType: TTokenType;
          FTokenType: TTokenType;
          FIndex:  integer;
          FSource:    string;
          FToken:     string;
          FPosition: integer;
          FSkipComments: boolean;
          FQuietErrors: boolean;
          FPrevious: string;
          FLineIndex: integer;
          procedure SetIndex(const Value: integer);
          procedure SetSource(const Value: string);
          procedure SetToken(const Value: string);
          procedure SetTokenType(const Value: TTokenType);
          function  SkipBlanks: boolean;
          function  GetScanning: boolean;
          function  ScanDigits: boolean;
          function  ScanExponential: boolean;
          procedure ScanComment;
          procedure ScanDisabled;
          procedure ScanSymbol;
          procedure ScanNumber;
          procedure ScanString;
          procedure Restart;
          procedure SetPosition(const Value: integer);
        public
          TokenList: TStringList;
          constructor Create;
          destructor  Destroy; override;
          function  NextToken: boolean;
          procedure SaveState;
          procedure RestoreState;
          property Source: string read FSource write SetSource;
          property Token: string read FToken write SetToken;
          property Index: integer read FIndex write SetIndex;
          property LineIndex: integer read FLineIndex;
          property Position: integer read FPosition write SetPosition;
          property Previous: string read FPrevious;
          property Scanning: boolean read GetScanning;
          property TokenType: TTokenType read FTokenType write SetTokenType;
          property SkipComments: boolean read FSkipComments write FSkipComments;
          property QuietErrors: boolean read FQuietErrors write FQuietErrors;
      end;

    TGetModuleSource = function(const ModuleName: string): string of object;
    TParser =
      class
        private
          FCurrentBlock: TBlock;
          FConsole:      TListBox;
          FModule: TModule;
          FRunner: TRunner;
          FSourceManager: TSourceManager;
          FModuleName: string;
          function GetFrameBlock(BlockType: CBlocks): TBlock;
          procedure SetSource(const Value: string);
          function GetSource: string;
          procedure PrintStr(const Str: string);
          function GetSourceLine: integer;
          procedure SetModuleName(const Value: string);
          procedure InitSource;
          procedure MarkCodeLines;
        public
          Scanner:   TScanner;
          BlockList: TList;
          Editor: TObject;
          property CurrentBlock: TBlock read FCurrentBlock write FCurrentBlock;
          property Source: string read GetSource write SetSource;
          property Console: TListBox read FConsole write FConsole;
          property Runner: TRunner read FRunner;
          property SourceLine: integer read GetSourceLine;
          property ModuleName: string read FModuleName write SetModuleName;
        private
          procedure Restart;
          procedure ConstantError;
          function SearchIdentifier(const What: TSymbolType; var Entry: TSymbolTableEntry): boolean;
          procedure FitParameters(ActualParams: TList; Routine: TProcedure);
          function Parse(const Sample: string; const Essential: boolean = true): boolean;
          function ParseIdentifier(var IdentifierName: string; const Essential: boolean = true): boolean;
          function ParseTypeDeclaration: boolean;
          function ParseType(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
          function ParseOrdinalType(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
          function ParseTypeIdentifier(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
          function ParseEnumerated(var DataType: TDataType): boolean;
          function ParseSubrange(var DataType: TDataType): boolean;
          function ParseArrayDeclaration(var DataType: TDataType): boolean;
          function ParseVectorDeclaration(var DataType: TDataType): boolean;
          function ParseMatrixDeclaration(var DataType: TDataType): boolean;
          function ParseFileDeclaration(var DataType: TDataType): boolean;
          function ParseRecordDeclaration(var DataType: TDataType): boolean;
          function ParseFieldList(var FieldList: TSymbolTable): boolean;
          function ParseVariableDeclaration: boolean;
          function ParseConstantDeclaration: boolean;
          function ParseFunctionDeclaration: boolean;
          function ParseProcedureDeclaration: boolean;
          function ParseActualParameterList(var ParamList: TList): boolean;
          function ParseFormalParameterList(var ParamList: TList): boolean;
          function ParseProcedureHeading(var Proc: TProcedure): boolean;
          function ParseFunctionHeading(var Funct: TFunction): boolean;
          function ParseQualifier(var theModule: TModule): boolean;
          function ParseConstantIdentifier(var Data: TData; const Essential: boolean = true): boolean;
          function ParseVariableIdentifier(var Data: TData; const Essential: boolean = true): boolean;
          function ParseFieldIdentifier(var Data: TData; const Essential: boolean = true): boolean;  //  Esto falta todavia
          function ParseProcedureIdentifier(var Proc: TProcedure; const Essential: boolean = true): boolean;
          function ParseFunctionIdentifier(var Func: TFunction; const Essential: boolean = true): boolean;
          function ParseUnsignedNumber(var Data: TData; const Essential: boolean = true): boolean;
          function ParseIntegerNumber(var Data: TData): boolean;
          function ParseFloatNumber(var Data: TData): boolean;
          function ParseCharacterString(var Data: TData; const Essential: boolean = true): boolean;
          function ParseUnsignedConstant(var Data: TData): boolean;
          function ParseConstant(var Data: TData; const Essential: boolean = true): boolean;
          function ParseVariable(var Data: TData; const Essential: boolean = true): boolean;
          function ParseField(var Data: TData): boolean;
          function ParseArrayIndex(var Data: TData): boolean;
          function ParseFactor(var Expression: TExpression): boolean;
          function ParseSimpleData(var Expression: TExpression): boolean;
          function ParseFunctionCall(var Expression: TExpression): boolean;
          function ParseInvertion(var Expression: TExpression): boolean;
          function ParseDeterminant(var Expression: TExpression): boolean;
          function ParseBooleanNot(var Expression: TExpression): boolean;
          function ParseTransposition(var Expression: TExpression): boolean;
          function ParseSubExpression(var Expression: TExpression): boolean;
          function ParseTerm(var Expression: TExpression): boolean;
          function ParsePower(var Expression: TExpression): boolean;
          function ParseMultiplication(var Expression: TExpression): boolean;
          function ParseDivision(var Expression: TExpression): boolean;
          function ParseInternalMult(var Expression: TExpression): boolean;
          function ParseInternalDiv(var Expression: TExpression): boolean;
          function ParseRemainder(var Expression: TExpression): boolean;
          function ParseBooleanAnd(var Expression: TExpression): boolean;
          function ParseSimpleExpression(var Expression: TExpression): boolean;
          function ParseAddition(var Expression: TExpression): boolean;
          function ParseSubtraction(var Expression: TExpression): boolean;
          function ParseBooleanOr(var Expression: TExpression): boolean;
          function ParseBooleanXor(var Expression: TExpression): boolean;
          function ParseNegation(var Expression: TExpression): boolean;
          function ParseExpression(var Expression: TExpression; const Essential: boolean = true): boolean;
          function ParseEquality(var Expression: TExpression): boolean;
          function ParseInequality(var Expression: TExpression): boolean;
          function ParseLessThan(var Expression: TExpression): boolean;
          function ParseLessThanOrEqualTo(var Expression: TExpression): boolean;
          function ParseGreaterThan(var Expression: TExpression): boolean;
          function ParseGreaterThanOrEqualTo(var Expression: TExpression): boolean;
          function ParseRelation(var Expression: TExpression): boolean;
          function ParseStatement(var Statement: TStatement): boolean;
          function ParseAssignation(var Statement: TStatement): boolean;
          function ParseProcedureCall(var Statement: TStatement): boolean;
          function ParseIf(var Statement: TStatement): boolean;
          function ParseWhile(var Statement: TStatement): boolean;
          function ParseRepeat(var Statement: TStatement): boolean;
          function ParseFor(var Statement: TStatement): boolean;
          function ParseOutPut(var Statement: TStatement): boolean;
          function ParseBlockBody(var Statement: TStatement): boolean;
          function ParseInitialization(var Statement: TStatement): boolean;
          function ParseBlockStatement(var BlockStatement: TBlockStatement): boolean;
          function ParseBlock(var Block: TBlock; const Prefabricated: boolean = false): boolean;
          function ParseDeclarations: boolean;
          function ParseModule(var Block: TBlock): boolean;
          function ParseProgram(var Block: TBlock): boolean;
          function ParseUses(var UsesList: TSymbolTable): boolean;
        public
          constructor Create(aRunner: TRunner; aSourceManager: TSourceManager);
          destructor  Destroy;  override;
          function CheckSyntax: boolean;
          function CompileProgram: TModule;
          function CompileModule: TModule;
      end;

  var
    ReservedWords: TStringList;
    Operators: TStringList;
    CompiledModules: TStringList;

implementation

  uses Windows, RichEdit, GospelEditor;

{ TScanner }

  function TScanner.GetScanning: boolean;
    begin
      Result := Index <= Length(Source);
    end;

  function TScanner.NextToken: boolean;
    var
      LastPos: integer;
      FoundToken: boolean;
      I: integer;
    begin
      TokenType := ttUnknow;
      Result := true;
      Position := Index;
      LastPos := Index;
      try
        repeat
          LastPos := Index;
          FoundToken := true;
          if Scanning
            then
              begin
                case Source[Index] of
                  sbString: ScanString;
                  '0'..'9': ScanNumber;
                  '{':
                    begin
                      ScanDisabled;
                      SkipBlanks;
                      FoundToken := false;
                    end;
                  '/':
                    begin
                      if (Index < Length(Source))
                        and (Source[succ(Index)] = '/')
                        then
                          begin
                            ScanComment;
                            SkipBlanks;
                            if SkipComments
                              then FoundToken := false;
                          end
                        else ScanSymbol;
                    end
                  else ScanSymbol;
                end;
              end
            else Result := false;
        until FoundToken;
      except
        if not QuietErrors
          then raise;
      end;
      Token := Copy(Source, LastPos, Index - LastPos);
      if ReservedWords.Find(UpperCase(Token), I)
        then TokenType := ttKeyWord;
      SkipBlanks;
    end;

  procedure TScanner.ScanSymbol;
    var
      TokenFound: boolean;
      LastPos: integer;

    function FindCouple(const Str: string; Ch: char): boolean;
      begin
        if (Pos(Source[Index], Str) <> 0) and (Source[succ(FIndex)] = Ch)
          then Result := true
          else Result := false;
      end;

    procedure CheckForCouple;
      begin
        if FindCouple('<>:', '=') or FindCouple('<', '>') or
           FindCouple('.', '.') or FindCouple('.', '*') or
           FindCouple('.', '/')
          then inc(FIndex, 2)
          else inc(FIndex)
      end;

    begin
      LastPos := Index;
      TokenType := ttSymbol;
      repeat
        while Scanning and (Pos(Source[Index], Delimiters + Blanks) = 0)
          do Inc(FIndex);
        TokenFound := true;
        if Index = LastPos
          then
            if Index < Length(Source)
              then CheckForCouple
              else inc(FIndex);
      until TokenFound;
    end;

  function TScanner.ScanDigits: boolean;
    var
      LastPos: integer;
    begin
      LastPos := Index;
      while Scanning and (Source[Index] in ['0'..'9'])
        do inc(FIndex);
      Result := Index > LastPos;
    end;

  function TScanner.ScanExponential: boolean;
    begin
      inc(FIndex);
      Result := ScanDigits;
      if not Result
        then
          begin
            Result := (Source[Index] = '+') or (Source[Index] = '-');
            inc(FIndex);
            if Result
              then Result := ScanDigits
              else raise EScanner.Create(emSyntaxErrorInRealNumber);
          end
    end;

  procedure TScanner.ScanNumber;
    begin
      ScanDigits;
      TokenType := ttFloat;
      if (Source[Index] = '.') and
        not((Index < Length(Source)) and (Source[succ(Index)] = '.'))
        then
          begin
            inc(FIndex);
            ScanDigits;
            if UpCase(Source[Index]) = 'E'
              then ScanExponential;
          end
        else
          if UpCase(Source[Index]) = 'E'
            then ScanExponential
            else TokenType := ttInteger;
    end;

  procedure TScanner.ScanString;
    var
      FoundToken: boolean;
    begin
      TokenType := ttString;
      inc(FIndex);
      repeat
        while Scanning and (Source[Index] <> sbString) and (Source[Index] <> #13)
          do inc(FIndex);
        if (Index > Length(Source)) or (Source[Index] = #13)
          then raise EScanner.Create(emUnterminatedString)
          else
            if (Index < Length(Source))
              and (Source[succ(Index)] = sbString)
              then
                begin
                  FoundToken := false;
                  inc(FIndex, 2);
                end
              else
                begin
                  FoundToken := true;
                  inc(FIndex);
                end;
      until FoundToken;
    end;

  procedure TScanner.SetIndex(const Value: integer);
    begin
      FIndex := Value;
    end;

  procedure TScanner.SetSource(const Value: string);
    begin
      FSource := Value;
      FIndex := 1;
      FLineIndex := 0;
      SkipBlanks;
    end;

  procedure TScanner.SetToken(const Value: string);
    begin
      FPrevious := FToken;
      FToken := Value;
      TokenList.Add(Value);
    end;

  procedure TScanner.SetTokenType(const Value: TTokenType);
    begin
      FTokenType := Value;
    end;

  function TScanner.SkipBlanks: boolean;
    var
      LastPos: integer;
    begin
      LastPos := Index;
      while Scanning and (Pos(Source[Index], Blanks) > 0) do
        begin
          if Source[Index] = #13
            then inc(FLineIndex);
          inc(FIndex);
        end;
      Result := Index > LastPos;
    end;

  procedure TScanner.ScanComment;
    begin
      TokenType := ttComment;
      while Scanning and (Source[Index] <> #13)
        do inc(FIndex);
    end;

  procedure TScanner.ScanDisabled;
    begin
      TokenType := ttComment;
      while Scanning and (Source[Index] <> '}') do
        begin
          if Source[Index] = #13
            then inc(FLineIndex);
          inc(FIndex);
        end;
      if Source[Index] = '}'
        then inc(FIndex);
    end;

  procedure TScanner.Restart;
    begin
      Index := 1;
      TokenList.Clear;
    end;

  procedure TScanner.SetPosition(const Value: integer);
    begin
      FPosition := Value;
    end;

  constructor TScanner.Create;
    begin
      inherited;
      FSkipComments := true;
      FQuietErrors  := false;
      TokenList := TStringList.Create;
    end;

  destructor TScanner.Destroy;
    begin
      TokenList.free;
      inherited;
    end;

  procedure TScanner.RestoreState;
    begin
      Index := SavePos;
      Token := SaveToken;
      TokenType := SaveType;
    end;

  procedure TScanner.SaveState;
    begin
      SavePos := Index;
      SaveToken := Token;
      SaveType := TokenType;
    end;

{ TParser }

  function TParser.CheckSyntax: boolean;
    var
      Block: TBlock;
    begin
      Restart;
      Scanner.Restart;
      Scanner.NextToken;
      CurrentBlock := SystemBlock;
      Result := ParseModule(Block);
    end;

  function TParser.CompileProgram: TModule;
    begin
      Restart;
      Scanner.Restart;
      Scanner.NextToken;
      CurrentBlock := SystemBlock;
      InitSource;
      ParseProgram(TBlock(Result));
      MarkCodeLines;
    end;

  procedure TParser.ConstantError;
    var
      Index: integer;
    begin
      if ReservedWords.Find(UpperCase(Scanner.Token), Index) or Operators.Find(UpperCase(Scanner.Token), Index)
        then raise EParser.Create(Format(emToken1ExpectedButToken2Found, [lsConstant, Scanner.Token]))
        else raise EParser.Create(Format(emToken1ExpectedButToken2Found, [lsConstant, lsIdentifier + ' ' + Scanner.Token]));
    end;

  constructor TParser.Create(aRunner: TRunner; aSourceManager: TSourceManager);
    begin
      inherited Create;
      Scanner        := TScanner.Create;
      BlockList      := TList.Create;
      FRunner        := aRunner;
      FSourceManager := aSourceManager;
      BlockList.Add(SystemBlock);
    end;

  destructor TParser.Destroy;
    begin
      BlockList.free;
      Scanner.free;
      inherited;
    end;

  procedure TParser.FitParameters(ActualParams: TList; Routine: TProcedure);
    var
      Index: integer;
    begin
      if ActualParams.Count > Routine.Parameters.Count
        then raise EParser.Create(emTooManyActualParameters)
        else
          if ActualParams.Count < Routine.Parameters.Count
            then raise EParser.Create(emNotEnoughActualParameters)
            else
              if ActualParams.Count > 0
                then
                  begin
                    Index  := 0;
                    repeat
                      if Routine.Variables.HasIdentifier(TData(Routine.Parameters.Items[Index]).Name)
                        then
                          if (TExpression(ActualParams.Items[Index]) is TSimpleData) and
                             (CurrentBlock.SymbolSystem.HasVariable(TSimpleData(ActualParams.Items[Index]).Data.Name))
                            then
                              begin
                                if (TSimpleData(ActualParams.Items[Index]).Data is TOrdinal) and
                                  not(TOrdinal(TSimpleData(ActualParams.Items[Index]).Data).CanBeAssigned)
                                  then raise EParser.Create(emFORLoopVariableCannotBePassedAsVarParameter)
                              end
                            else raise EParser.Create(emConstantObjectCannotBePassedAsVarParameter);
                       if not TData(Routine.Parameters.Items[Index]).Assignable(TExpression(ActualParams.Items[Index]).YieldType)
                        then raise EParser.Create(emTypesOfActualAndFormalVarParametersMustBeIdentical);
                      inc(Index);
                    until Index = Routine.Parameters.Count;
                  end
    end;

  function TParser.GetFrameBlock(BlockType: CBlocks): TBlock;
    begin
      Result := BlockType.Create;
      BlockList.Add(Result);
      Result.Owner := CurrentBlock;
      CurrentBlock := Result;
    end;

  function TParser.GetSource: string;
    begin
      Result := Scanner.Source;
    end;

  function TParser.Parse(const Sample: string; const Essential: boolean = true): boolean;
    begin
      Result := UpperCase(Scanner.Token) = UpperCase(Sample);
      if Result
        then Scanner.NextToken
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found, [Sample, Scanner.Token]));
    end;


  function TParser.ParseActualParameterList(var ParamList: TList): boolean;
    var
      Expression: TExpression;
    begin
      Result := Parse('(', false);
      ParamList := TList.Create;
      if Result
        then
          begin
            repeat
              if ParseExpression(Expression, false) {ParseVariable(Data) or}
                then ParamList.Add(Expression)
                else raise EParser.Create(Format(emElementExpectedButToken2Found, [lsExpression, Scanner.Token]));
            until not Parse(',', false);
            Parse(')');
          end;
    end;

  function TParser.ParseAddition(var Expression: TExpression): boolean;
    begin
      Result := Parse('+', false);
      if Result
        then Expression := TAddition.Create;
    end;

  function TParser.ParseArrayIndex(var Data: TData): boolean;
    var
      Expression: TExpression;
      ExpressionList: TList;
      i: integer;
    begin
      if Parse('[', false)
        then
          begin
            if not(Data.DataType is TArrayType)
              then raise EParser.Create(emArrayTypeRequired);
            ExpressionList := TList.Create;
            ParseExpression(Expression);
            ExpressionList.Add(Expression);
            while Parse(',', false) do
              begin
                ParseExpression(Expression);
                ExpressionList.Add(Expression);
              end;
            Parse(']');
            if (Data.DataType is TRowVectorType) or (Data.DataType is TColVectorType)
              then
                if ExpressionList.Count = 1
                  then
                    if Data.DataType is TRowVectorType
                      then
                        begin
                          Data := TRowVector(Data).Row[1];
                          Data := TArrayIndex.Create(Data, TExpression(ExpressionList[0]));
                        end
                      else
                        begin
                          Data := TArrayIndex.Create(Data, TExpression(ExpressionList[0]));
                          Data := TArrayIndex.Create(Data, TSimpleData.Create(IntegerType.InstantiateWith(1)));
                        end
                  else raise EParser.Create(emInvalidVectorIndexed)
              else
                for i := 0 to pred(ExpressionList.Count) do
                  Data := TArrayIndex.Create(Data, TExpression(ExpressionList[i]));
            ExpressionList.free;
            Result := true;
          end
        else Result := false;
    end;

  function TParser.ParseArrayDeclaration(var DataType: TDataType): boolean;
    var
      OrdinalType:  TDataType;
      OrdinalTypeList: TList;
      i: integer;
    begin
      Result := Parse(rwArray, false);
      if Result
        then
          begin
            OrdinalTypeList := TList.Create;
            Parse('[');
            ParseOrdinalType(OrdinalType, false);
            OrdinalTypeList.Add(OrdinalType);
            while Parse(',', false) do
              begin
                ParseOrdinalType(OrdinalType, false);
                OrdinalTypeList.Add(OrdinalType);
              end;
            Parse(']');
            Parse(rwOf);
            ParseType(DataType, false);
            for i := pred(OrdinalTypeList.Count) downto 0 do
              DataType := TArrayType.CreateWith(TOrdinalType(OrdinalTypeList[i]), DataType);
            OrdinalTypeList.free;
          end;
    end;

  function TParser.ParseAssignation(var Statement: TStatement): boolean;
    var
      Data: TData;
      Expression: TExpression;
      Routine: TFunction;
    begin
      Routine := nil;
      Scanner.SaveState;
      Result := ParseFunctionIdentifier(Routine, false) and (Routine = CurrentBlock);
      if Result
        then Data := Routine.Result
        else
          begin
            Scanner.RestoreState;
            Result := ParseVariable(Data, false);
          end;
      if Result
        then
          begin
            if (Data is TInteger) and not (TInteger(Data).CanBeAssigned)
              then raise EParser.Create(Format(emAssignmentToFORLoopVariable, [Data.Name]));
            if Parse(':=', Routine = nil)
              then
                begin
                  ParseExpression(Expression);
                  if Data.Assignable(Expression.YieldType)
                    then
                      begin
                        Statement := TAssignation.Create(Runner, SourceLine);
                        FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[SourceLine] := Statement;
                        TAssignation(Statement).Data := Data;
                        TAssignation(Statement).Expression := Expression;
                      end
                    else IncompatibleTypesError(Data.DataType.Name, Expression.YieldType.Name);
                end
              else Result := false;
          end;
    end;

  function TParser.ParseBlock(var Block: TBlock; const Prefabricated: boolean = false): boolean;
    var
      Statement: TStatement;
    begin
      if not Prefabricated
        then Block := GetFrameBlock(TProcedure);
      ParseDeclarations;
      Result := ParseBlockBody(Statement);
      CurrentBlock.Body := TBlockStatement(Statement);
      CurrentBlock := Block.Owner;
      if not Result
        then raise EParser.Create(Format(emToken1ExpectedButToken2Found,['Declaration', Scanner.Token]));
    end;

  function TParser.ParseBlockBody(var Statement: TStatement): boolean;
    begin
      Result := Parse(rwBegin, false);
      if Result
        then
          begin
            ParseBlockStatement(TBlockStatement(Statement));
            try
              Parse(rwEnd);
            except
              Statement.free;
              raise;
            end;
          end;
    end;

  function TParser.ParseBlockStatement(var BlockStatement: TBlockStatement): boolean;
    var
      Statement: TStatement;
      Exp: TExpression;
      Index: integer;
      Data: TData;
      LastPos: integer;
      LastToken: string;
      LastTokenType: TTokenType;

      procedure TakeStatement;
        begin
          if ParseStatement(Statement)
            then BlockStatement.Statements.Add(Statement);
        end;

    begin
      Result := true;
      BlockStatement := TBlockStatement.Create(Runner, 0);
      TakeStatement;
      while Parse(';', false) do
        TakeStatement;
      if (UpperCase(Scanner.Token) <> rwEnd) and (UpperCase(Scanner.Token) <> rwUntil)
        then
          begin
            if (UpperCase(Scanner.Token) = rwElse) and (Scanner.Previous = ';')
              then raise EParser.Create(emSemicolonNotAllowedBeforeELSE);
            if ReservedWords.Find(UpperCase(Scanner.Token), Index) or Operators.Find(UpperCase(Scanner.Token), Index)
              then raise EParser.Create(Format(emElementExpectedButToken2Found, [lsStatement, Scanner.Token]));
            LastPos := Scanner.Index;
            LastToken := Scanner.Token;
            LastTokenType := Scanner.TokenType;
            if ParseUnsignedConstant(Data) and Parse(':=', false)
              then raise EParser.Create(emLeftSideCannotBeAssignedTo);
            Scanner.Index := LastPos;
            Scanner.Token := LastToken;
            Scanner.TokenType := LastTokenType;
            if ParseExpression(Exp, false)
              then raise EParser.Create(Format(emStatementExpectedButExpressionFound, [Exp.YieldType.Name]));
            if CurrentBlock.SymbolSystem.HasIdentifier(Scanner.Token)
              then raise EParser.Create(emMissingOperatorOrSemicolon)
              else raise EParser.Create(Format(emUndeclaredIdentifier, [Scanner.Token]));
          end;
    end;

  function TParser.ParseBooleanAnd(var Expression: TExpression): boolean;
    begin
      Result := Parse(rwAnd, false);
      if Result
        then Expression := TBooleanAnd.Create;
    end;

  function TParser.ParseBooleanNot(var Expression: TExpression): boolean;
    var
      Exp: TExpression;
    begin
      Result := Parse(rwNot, false);
      if Result
       then
         begin
           ParseFactor(Exp);
           Expression := TBooleanNot.CreateWith(Exp);
         end;
    end;

  function TParser.ParseBooleanOr(var Expression: TExpression): boolean;
    begin
      Result := Parse(rwOr, false);
      if Result
        then Expression := TBooleanOr.Create;
    end;

  function TParser.ParseBooleanXor(var Expression: TExpression): boolean;
    begin
      Result := Parse(rwXor, false);
      if Result
        then Expression := TBooleanXor.Create;
    end;

  function TParser.ParseCharacterString(var Data: TData; const Essential: boolean = true): boolean;
    begin
      Result := Scanner.TokenType = ttString;
      if Result
        then
          begin
            Data := StringType.Instantiate;
            TString(Data).Value := Scanner.Token;
            Scanner.NextToken;
          end
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found, ['Character string', Scanner.Token]));
    end;

  function TParser.ParseConstant(var Data: TData; const Essential: boolean = true): boolean;
    var
      Neg: boolean;
    begin
      if ParseCharacterString(Data, false)
        then Result := true
        else
          begin
            if Parse('+', false)
              then Neg := false
              else Neg := Parse('-', false);
            Result := ParseConstantIdentifier(Data, false) or ParseUnsignedNumber(Data, false);
            if Result and Neg
              then
                if Data.DataType is TIntegerType
                  then TInteger(Data).Value := - TInteger(Data).Value
                  else
                    if Data.DataType is TRealType
                      then TReal(Data).Value := - TReal(Data).Value;
          end;
      if not Result and Essential
        then ConstantError;
    end;

  function TParser.ParseConstantDeclaration: boolean;
    var
      IdentName: string;
      Data: TData;
    begin
      Result := Parse(rwConst, false);
      if Result
       then
         begin
           ParseIdentifier(IdentName);
           repeat
             Parse('=');
             ParseConstant(Data);
             Data.Name := IdentName;
             CurrentBlock.Constants.AddEntry(Data);
             Parse(';');
           until not ParseIdentifier(IdentName, false);
         end;
    end;

  function TParser.ParseConstantIdentifier(var Data: TData; const Essential: boolean = true): boolean;
    begin
      Result := SearchIdentifier(stConstant, TSymbolTableEntry(Data));
      if Result
        then Scanner.NextToken
        else
          if Essential
            then ConstantError;
    end;

  function TParser.ParseDivision(var Expression: TExpression): boolean;
    begin
      Result := Parse('/', false);
      if Result
        then Expression := TDivision.Create;
    end;

  function TParser.ParseEnumerated(var DataType: TDataType): boolean;
    var
      IdentifierList: TStringList;
      IdentName: string;
      Item: TEnumerated;
      i: integer;
    begin
      Result := Parse('(', false);
      if Result
        then
          begin
            IdentifierList := TStringList.Create;
            try
              ParseIdentifier(IdentName);
              IdentifierList.Add(IdentName);
              while Parse(',', false) do
                begin
                  ParseIdentifier(IdentName);
                  IdentifierList.Add(IdentName);
                end;
              Parse(')');
              DataType := TEnumeratedType.Create;
              for i := 0 to pred(IdentifierList.Count) do
                begin
                  Item := TEnumerated.CreateFrom(DataType);
                  Item.Name := IdentifierList[i];
                  TEnumeratedType(DataType).AddIdentifier(Item);
                  CurrentBlock.Constants.AddEntry(Item);
                end;
            finally
              IdentifierList.free;
            end;
          end;
    end;

  function TParser.ParseEquality(var Expression: TExpression): boolean;
    begin
      Result := Parse('=', false);
      if Result
        then Expression := TEquality.Create;
    end;

  function TParser.ParseExpression(var Expression: TExpression; const Essential: boolean = true): boolean;
    var
      Exp1: TExpression;
      Exp2: TExpression;
    begin
      Result := ParseSimpleExpression(Expression);
      if Result and ParseRelation(Exp1)
        then
          begin
            Result := ParseSimpleExpression(Exp2);
            if Result
              then
                begin
                  TBinaryExpression(Exp1).LeftExp := Expression;
                  TBinaryExpression(Exp1).RightExp := Exp2;
                  Expression := Exp1;
                end;
          end;
      if not Result and Essential
        then raise EParser.Create(Format(emElementExpectedButToken2Found, [lsExpression, Scanner.Token]));
    end;

  function TParser.ParseFactor(var Expression: TExpression): boolean;
    begin
      Result := ParseSimpleData(Expression) or ParseFunctionCall(Expression) or
                ParseBooleanNot(Expression) or ParseSubExpression(Expression) or
                ParseInvertion(Expression) or ParseDeterminant(Expression);
      if Result
        then ParseTransposition(Expression);
    end;

  function TParser.ParseField(var Data: TData): boolean;
    begin
      if Parse('.', false)
        then
          if Data.DataType is TRecordType
            then
              begin
                ParseFieldIdentifier(Data);
                Result := true;
              end
            else raise EParser.Create(emRecordTypeRequired)
        else Result := false;
    end;

  function TParser.ParseFieldIdentifier(var Data: TData; const Essential: boolean = true): boolean;
    var
      i: integer;
    begin
      if Data.DataType is TRecordType
        then
          begin
            if Data is TFloatingData
              then
                begin
                  Result := TRecordType(Data.DataType).FieldList.Find(Scanner.Token, i);
                  if Result
                    then Data := TFloatingField.Create(TFloatingData(Data), i);
                end
              else Result := TRecord(Data).FieldList.FindEntry(Scanner.Token, TSymbolTableEntry(Data));
            if Result
              then Scanner.NextToken
              else raise EParser.Create(Format(emUndeclaredIdentifier, [Scanner.Token]))
          end
        else raise EParser.Create(emRecordTypeRequired);
    end;

  function TParser.ParseFieldList(var FieldList: TSymbolTable): boolean;
    var
      IdentifierList: TStringList;
      IdentName: string;
      DataType: TDataType;
      Data: TData;
      i: integer;
    begin
      IdentifierList := TStringList.Create;
      FieldList      := TSymbolTable.Create;
      try
        try
          repeat
            IdentifierList.Clear;
            Result := ParseIdentifier(IdentName, false);
            IdentifierList.Add(IdentName);
            if Result
              then
                begin
                  while Parse(',', false) do
                    begin
                      ParseIdentifier(IdentName);
                      IdentifierList.Add(IdentName);
                    end;
                  Parse(':');
                  ParseType(DataType, false);
                  for i := 0 to pred(IdentifierList.Count) do
                    begin
                      Data := DataType.Instantiate;
                      Data.Name := IdentifierList[i];
                      FieldList.AddEntry(Data);
                    end;
                end;
          until not (Result and Parse(';', false));
        except
          FieldList.free;
          raise;
        end;
      finally
        IdentifierList.free;
      end;
      Result := true;
    end;

  function TParser.ParseFileDeclaration(var DataType: TDataType): boolean;
    begin
      Result := Parse(rwFile, false);
      if Result
        then DataType := TFileType.Create;
    end;

  function TParser.ParseFor(var Statement: TStatement): boolean;
    var
      Expression: TExpression;
      DoStatement: TStatement;
      Data: TData;
      Line: integer;
    begin
      Result := Parse(rwFor, false);
      if Result
        then
          begin
            Line := succ(SourceLine);
            ParseVariableIdentifier(Data);
            try
              Parse(':=');
            except
              if (Scanner.Token = '[') or (Scanner.Token = '.')
                then raise EParser.Create(emForLoopControlVariableMustBeSimpleLocalVariable)
                else raise;
            end;
            if not (Data is TOrdinal)
              then raise EParser.Create(emForLoopControlVariableMustHaveOrdinalType);
            TInteger(Data).CanBeAssigned := false;
            ParseExpression(Expression);
            if Data.DataType <> Expression.YieldType
              then IncompatibleTypesError(Data.DataType.Name, Expression.YieldType.Name);
            Statement := TForStatement.Create(Runner, Line);
            try
              TForStatement(Statement).Counter := TInteger(Data);
              TForStatement(Statement).InitialExp := Expression;
              if Parse(rwTo, false)
                then TForStatement(Statement).LoopDirection := ldTo
                else
                  if Parse(rwDownto, false)
                    then TForStatement(Statement).LoopDirection := ldDownto
                    else raise EParser.Create(Format(emToken1ExpectedButToken2Found, ['''TO''', Scanner.Token]));
              ParseExpression(Expression);
              if Data.DataType <> Expression.YieldType
                then IncompatibleTypesError(Data.DataType.Name, Expression.YieldType.Name);
              TForStatement(Statement).FinalExp := Expression;
              Parse(rwDo);
              ParseStatement(DoStatement);
              TForStatement(Statement).Statement := DoStatement;
              TInteger(Data).CanBeAssigned := true;
              FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[Line] := Statement;
            except
              Statement.free;
              raise;
            end;
          end;
    end;

  function TParser.ParseFormalParameterList(var ParamList: TList): boolean;
    var
      Storehouse: TStringList;
      IdentName: string;
      SymbolTable: TSymbolTable;
      DataType: TDataType;
      Data: TData;
      i: integer;
    begin
      ParamList  := TList.Create;
      Result := Parse('(', false);
      if Result
        then
          begin
            Storehouse := TStringList.Create;
            try
              repeat
                if Parse(rwConst, false)
                  then SymbolTable := CurrentBlock.Constants
                  else
                    begin
                      Parse(rwVar, false);
                      SymbolTable := CurrentBlock.Variables;
                    end;
                ParseIdentifier(IdentName);
                Storehouse.Add(IdentName);
                while Parse(',', false) do
                  begin
                    ParseIdentifier(IdentName);
                    Storehouse.Add(IdentName);
                  end;
                try
                  Parse(':');
                except
                  raise EParser.Create(emMissingParameterType);
                end;
                ParseType(DataType, false);
                for i := 0 to pred(Storehouse.Count) do
                  begin
                    Data := DataType.Instantiate;
                    Data.Name := Storehouse[i];
                    SymbolTable.AddEntry(Data);
                    ParamList.Add(Data);
                  end;
                Storehouse.Clear;
              until not Parse(';', false);
            finally
              Storehouse.free;
            end;
            Parse(')');
          end;
    end;

  function TParser.ParseFunctionCall(var Expression: TExpression): boolean;
    var
      Parameters: TList;
      Routine: TFunction;
      Statement: TStatement;
    begin
      Result := ParseFunctionIdentifier(Routine, false);
      if Result
        then
          begin
            ParseActualParameterList(Parameters);
            FitParameters(Parameters, Routine);
            Statement := TFunctionCallStatement.Create(Runner, SourceLine);
            TFunctionCallStatement(Statement).Routine := Routine;
            TFunctionCallStatement(Statement).Parameters := Parameters;
            Expression := TFunctionEvaluation.Create;
            TFunctionEvaluation(Expression).FunctionCall := TFunctionCallStatement(Statement);
          end;
    end;

  function TParser.ParseFunctionDeclaration: boolean;
    var
      Funct: TFunction;
    begin
      Result := ParseFunctionHeading(Funct);
      if Result
        then
          begin
            CurrentBlock.Owner.Functions.AddEntry(Funct);
            Parse(';');
            ParseBlock(TBlock(Funct), true);       // or Directive
            Parse(';');
          end;
    end;

  function TParser.ParseFunctionHeading(var Funct: TFunction): boolean;
    var
      IdentName: string;
      DataType: TDataType;
      ParamList: TList;
    begin
      Result := Parse(rwFunction, false);
      if Result
        then
          begin
            ParseIdentifier(IdentName);
            Funct := TFunction(GetFrameBlock(TFunction));
            Funct.Name := IdentName;
            ParseFormalParameterList(ParamList);
            try
              Parse(':');
            except
              raise EParser.Create(emFunctionNeedsResultType);
            end;
            ParseTypeIdentifier(DataType, false);
            Funct.Result := DataType.Instantiate;
            Funct.Parameters := ParamList;
          end;
    end;

  function TParser.ParseFunctionIdentifier(var Func: TFunction; const Essential: boolean = true): boolean;
    begin
      Result := SearchIdentifier(stFunction, TSymbolTableEntry(Func));
      if Result
        then Scanner.NextToken
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found,['Function', Scanner.Token]));
    end;

  function TParser.ParseGreaterThan(var Expression: TExpression): boolean;
    begin
      Result := Parse('>', false);
      if Result
        then Expression := TGreaterThan.Create;
    end;

  function TParser.ParseGreaterThanOrEqualTo(var Expression: TExpression): boolean;
    begin
      Result := Parse('>=', false);
      if Result
        then Expression := TGreaterThanOrEqualTo.Create;
    end;

  function TParser.ParseIdentifier(var IdentifierName: string; const Essential: boolean = true): boolean;
    var
      Index: integer;
    begin
      if not (ReservedWords.Find(UpperCase(Scanner.Token), Index) or Operators.Find(UpperCase(Scanner.Token), Index))
         and (UpCase(Scanner.Token[1]) in ['A'..'Z'])
        then
          begin
            Result := true;
            IdentifierName := Scanner.Token;
            Scanner.NextToken;
          end
        else
          begin
            Result := false;
            if Essential
              then raise EParser.Create(Format(emElementExpectedButToken2Found, [lsIdentifier, Scanner.Token]));
          end;
    end;

  function TParser.ParseIf(var Statement: TStatement): boolean;
    var
      Expression: TExpression;
      ThenStatement: TStatement;
      ElseStatement: TStatement;
      Line: integer;
    begin
      Result := Parse(rwIf, false);
      if Result
        then
          begin
            Line := succ(SourceLine);
            ParseExpression(Expression);
            if Expression.YieldType <> BooleanType
              then raise EParser.Create(emTypeOfExpressionMustBeBOOLEAN);
            Parse(rwThen);
            ParseStatement(ThenStatement);
            Statement := TIfStatement.Create(Runner, Line);
            TIfStatement(Statement).Expression := Expression;
            TIfStatement(Statement).ThenStatement := ThenStatement;
            FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[Line] := Statement;
            if Parse(rwElse, false)
              then
                begin
                  ParseStatement(ElseStatement);
                  TIfStatement(Statement).ElseStatement := ElseStatement;
                end;
          end;
    end;

  function TParser.ParseInequality(var Expression: TExpression): boolean;
    begin
      Result := Parse('<>', false);
      if Result
        then Expression := TInequality.Create;
    end;

  function TParser.ParseLessThan(var Expression: TExpression): boolean;
    begin
      Result := Parse('<', false);
      if Result
        then Expression := TLessThan.Create;
    end;

  function TParser.ParseLessThanOrEqualTo(var Expression: TExpression): boolean;
    begin
      Result := Parse('<=', false);
      if Result
        then Expression := TLessThanOrEqualTo.Create;
    end;

  function TParser.ParseModule(var Block: TBlock): boolean;
    var
      IdentName: string;
      UsesList: TSymbolTable;
      Statement: TStatement;
    begin
      Result := false;
      try
        Result := Parse(rwModule);
      except
        if Scanner.Scanning
          then raise;
      end;
      if Result
        then
          begin
            ParseIdentifier(IdentName);
            Parse(';');
            Block := GetFrameBlock(TModule);
            FModule := TModule(Block);
            if ParseUses(UsesList)
              then TModule(Block).Includes := UsesList;
            ParseDeclarations;
            if ParseInitialization(Statement)
              then Block.Body := TBlockStatement(Statement);
            Parse(rwEnd);
            Parse('.');
            Block.Name := IdentName;
          end;
    end;

  function TParser.ParseMultiplication(var Expression: TExpression): boolean;
    begin
      Result := Parse('*', false);
      if Result
        then Expression := TMultiplication.Create;
    end;

  function TParser.ParseNegation(var Expression: TExpression): boolean;
    begin
      Result := Parse('-', false);
      if Result
        then Expression := TNegation.Create;
    end;

  function TParser.ParseOrdinalType(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
    begin
      Result := ParseTypeIdentifier(DataType, Copy, false) or ParseEnumerated(DataType) or
                ParseSubrange(DataType);
      if not Result and Essential
        then raise EParser.Create(emOrdinalTypeRequired);
    end;

  function TParser.ParseOutPut(var Statement: TStatement): boolean;
    var
      Expression: TExpression;
    begin
      Result := Parse(rwOutput, false);
      if Result
        then
          begin
            ParseExpression(Expression);
            Statement := TOutputStatement.Create(Runner, SourceLine);
            FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[SourceLine] := Statement;
            TOutputStatement(Statement).Expression := Expression;
            TOutputStatement(Statement).OutPut := PrintStr;
          end;
    end;

  function TParser.ParseProcedureCall(var Statement: TStatement): boolean;
    var
      Parameters: TList;
      Routine: TProcedure;
    begin
      Result := ParseProcedureIdentifier(Routine, false);
      if not Result
        then Result := ParseFunctionIdentifier(TFunction(Routine), false);
      if Result
        then
          begin
            ParseActualParameterList(Parameters);
            FitParameters(Parameters, Routine);
            Statement := TProcedureCallStatement.Create(Runner, SourceLine);
            FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[SourceLine] := Statement;
            TProcedureCallStatement(Statement).Routine := Routine;
            TProcedureCallStatement(Statement).Parameters := Parameters;
          end;
    end;

  function TParser.ParseProcedureDeclaration: boolean;
    var
      Proc: TProcedure;
      Name: string;
    begin
      Result := ParseProcedureHeading(Proc);
      if Result
        then
          begin
            CurrentBlock.Owner.Procedures.AddEntry(Proc);
            try
              Parse(';');
            except
              if Parse(':', false) and ParseIdentifier(Name, false)
                then raise EParser.Create(emProcedureCannotHaveResultType)
                else raise;
            end;
            ParseBlock(TBlock(Proc), true);       // or Directive
            Parse(';');
          end;
    end;

  function TParser.ParseProcedureHeading(var Proc: TProcedure): boolean;
    var
      IdentName: string;
      ParamList: TList;
    begin
      Result := Parse(rwProcedure, false);
      if Result
        then
          begin
            ParseIdentifier(IdentName);
            Proc := TProcedure(GetFrameBlock(TProcedure));
            Proc.Name := IdentName;
            ParseFormalParameterList(ParamList);
            Proc.Parameters := ParamList;
          end;
    end;

  function TParser.ParseProcedureIdentifier(var Proc: TProcedure; const Essential: boolean = true): boolean;
    begin
      Result := SearchIdentifier(stProcedure, TSymbolTableEntry(Proc));
      if Result
        then Scanner.NextToken
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found,['Procedure', Scanner.Token]));
    end;

  function TParser.ParseRecordDeclaration(var DataType: TDataType): boolean;
    var
      FieldList: TSymbolTable;
    begin
      Result := Parse(rwRecord, false);
      if Result
        then
          begin
            ParseFieldList(FieldList);
            Parse(rwEnd);
            DataType := TRecordType.CreateWith(FieldList);
          end;
    end;

  function TParser.ParseRelation(var Expression: TExpression): boolean;
    begin
      Result := ParseEquality(Expression) or ParseInequality(Expression) or
                ParseLessThan(Expression) or ParseLessThanOrEqualTo(Expression) or
                ParseGreaterThan(Expression) or ParseGreaterThanOrEqualTo(Expression);
    end;

  function TParser.ParseRemainder(var Expression: TExpression): boolean;
    begin
      Result := Parse(rwMod, false);
      if Result
        then Expression := TRemainder.Create;
    end;

  function TParser.ParseRepeat(var Statement: TStatement): boolean;
    var
      Expression: TExpression;
      BlockStatement: TBlockStatement;
    begin
      Result := Parse(rwRepeat, false);
      if Result
        then
          begin
            ParseBlockStatement(BlockStatement);
            try
              Parse(rwUntil);
              ParseExpression(Expression);
              if Expression.YieldType <> BooleanType
                then raise EParser.Create(emTypeOfExpressionMustBeBOOLEAN);
              Statement := TRepeatStatement.Create(Runner, 0);
              BlockStatement.SourceLine := SourceLine;
              FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[SourceLine] := Statement;
              TRepeatStatement(Statement).Expression := Expression;
              TRepeatStatement(Statement).BlockStatement := BlockStatement;
            except
              BlockStatement.free;
              raise;
            end;
          end;
    end;

  function TParser.ParseSimpleData(var Expression: TExpression): boolean;
    var
      Data: TData;
    begin
      Result := ParseUnsignedConstant(Data) or ParseVariable(Data, false);
      if Result
        then Expression := TSimpleData.Create(Data);
    end;

  function TParser.ParseSimpleExpression(var Expression: TExpression): boolean;
    var
      Exp1: TExpression;
      Exp2: TExpression;
      Neg: boolean;
    begin
      if Parse('+', false)
        then Neg := false
        else Neg := ParseNegation(Expression);
      Result := ParseTerm(Expression);
      while Result and (ParseAddition(Exp1) or ParseSubtraction(Exp1) or
            ParseBooleanOr(Exp1) or ParseBooleanXor(Exp1)) do
        begin
          Result := ParseTerm(Exp2);
          if Result
            then
              begin
                TBinaryExpression(Exp1).LeftExp := Expression;
                TBinaryExpression(Exp1).RightExp := Exp2;
                Expression := Exp1;
              end;
        end;
      if Result and Neg    
        then Expression := TNegation.CreateWith(Expression);
    end;

  function TParser.ParseStatement(var Statement: TStatement): boolean;
    begin
      Statement := nil;
      Result := ParseAssignation(Statement) or ParseProcedureCall(Statement) or
                ParseBlockBody(Statement) or ParseIf(Statement) or
                ParseRepeat(Statement) or ParseFor(Statement) or
                ParseWhile(Statement) or ParseOutput(Statement);
    end;

  function TParser.ParseSubExpression(var Expression: TExpression): boolean;
    begin
      Result := Parse('(', false);
      if Result
        then
          begin
            ParseExpression(Expression);
            Parse(')');
          end
    end;

  function TParser.ParseSubrange(var DataType: TDataType): boolean;
    var
      VLow, VHigh: integer;
      Data: TData;
    begin
      Result := ParseConstant(Data, false);
      if Result
        then
          begin
            if Data.DataType is TIntegerType
              then VLow := TInteger(Data).Value
              else raise EParser.Create(emOrdinalTypeRequired);
            Data.free;
            Parse('..');
            ParseConstant(Data);
            if Data.DataType is TIntegerType
              then VHigh := TInteger(Data).Value
              else raise EParser.Create(emOrdinalTypeRequired);
            Data.free;
            if VLow > VHigh
              then raise EParser.Create(emLowBoundExceedsHighBound);
            DataType := TSubrangeType.Create(VLow, VHigh);
          end;
    end;

  function TParser.ParseSubtraction(var Expression: TExpression): boolean;
    begin
      Result := Parse('-', false);
      if Result
        then Expression := TSubtraction.Create;
    end;

  function TParser.ParseTerm(var Expression: TExpression): boolean;
    var
      Exp1: TExpression;
      Exp2: TExpression;
    begin
      Result := ParseFactor(Expression);
      while Result and (ParsePower(Exp1) or
            ParseMultiplication(Exp1) or ParseDivision(Exp1) or
            ParseRemainder(Exp1) or ParseBooleanAnd(Exp1) or
            ParseInternalMult(Exp1) or ParseInternalDiv(Exp1)) do
        begin
          Result := ParseFactor(Exp2);
          if Result
            then
              begin
                TBinaryExpression(Exp1).LeftExp := Expression;
                TBinaryExpression(Exp1).RightExp := Exp2;
                Expression := Exp1;
              end;
        end;
    end;

  function TParser.ParseType(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
    begin
      Result := ParseOrdinalType(DataType, Copy, false) or ParseArrayDeclaration(DataType) or
                ParseFileDeclaration(DataType) or ParseRecordDeclaration(DataType) or
                ParseMatrixDeclaration(DataType) or ParseVectorDeclaration(DataType);
      if not Result and Essential
        then EParser.Create(emConstantOrTypeIdentifierExpected);
    end;

  function TParser.ParseTypeDeclaration: boolean;
    var
      IdentName: string;
      DataType: TDataType;
    begin
      Result := Parse(rwType, false);
      if Result
        then
          begin
            ParseIdentifier(IdentName);
            repeat
              Parse('=');
              ParseType(DataType, true);
              DataType.Name := IdentName;
              CurrentBlock.Types.AddEntry(DataType);
              Parse(';');
            until not ParseIdentifier(IdentName, false);
          end;
    end;

  function TParser.ParseTypeIdentifier(var DataType: TDataType; const Copy: boolean; const Essential: boolean = true): boolean;
    var
      SymbolTableEntry: TSymbolTableEntry;
    begin
      Result := SearchIdentifier(stType, SymbolTableEntry);
      if Result
        then
          begin
            if Copy
              then
                begin
                  DataType := TDataType(SymbolTableEntry).ClassType.Create as TDataType;
                  DataType.Name := SymbolTableEntry.Name;
                end
              else DataType := TDataType(SymbolTableEntry);
            Scanner.NextToken;
          end
        else
          if Essential
            then EParser.Create(emNameIsNotTypeIdentifier);
    end;

  function TParser.ParseUnsignedConstant(var Data: TData): boolean;
    begin
      Result := ParseConstantIdentifier(Data, false);
      if Result
        then 
          repeat
          until not (ParseField(Data) or ParseArrayIndex(Data))
        else Result := ParseUnsignedNumber(Data, false) or
                       ParseCharacterString(Data, false);
    end;

  function TParser.ParseUnsignedNumber(var Data: TData; const Essential: boolean = true): boolean;
    begin
      Result := ParseIntegerNumber(Data) or ParseFloatNumber(Data);
      if not Result and Essential
        then raise EParser.Create(Format(emToken1ExpectedButToken2Found, ['Unsigned number', Scanner.Token]));
    end;

  function TParser.ParseVariable(var Data: TData; const Essential: boolean = true): boolean;
    begin
      Result := ParseVariableIdentifier(Data, false); //or ParseFieldIdentifier(false);
      if Result
        then 
          repeat
          until not (ParseField(Data) or ParseArrayIndex(Data))
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found, [lsVariable, Scanner.Token]));
    end;

  function TParser.ParseVariableDeclaration: boolean;
    var
      VarIdentifier: TStringList;
      IdentName: string;
      DataType: TDataType;
      Data: TData;
      i: integer;
    begin
      Result := Parse(rwVar, false);
      if Result
        then
          begin
            VarIdentifier := TStringList.Create;
            try
              ParseIdentifier(IdentName);
              repeat
                VarIdentifier.Add(IdentName);
                while Parse(',', false) do
                  begin
                    ParseIdentifier(IdentName);
                    VarIdentifier.Add(IdentName);
                  end;
                try
                  Parse(':');
                except
                  raise EParser.Create(Format(emToken1ExpectedButToken2Found,[''','' or '':''', Scanner.Token]));
                end;
                ParseType(DataType, false);
                Parse(';');
                for i := 0 to pred(VarIdentifier.Count) do
                  begin
                    Data := DataType.Instantiate;
                    Data.Name := VarIdentifier[i];
                    CurrentBlock.Variables.AddEntry(Data);
                  end;
                VarIdentifier.Clear;
              until not ParseIdentifier(IdentName, false);
            finally
              VarIdentifier.free;
            end;
          end;
    end;

  function TParser.ParseVariableIdentifier(var Data: TData; const Essential: boolean = true): boolean;
    begin
      Result := SearchIdentifier(stVariable, TSymbolTableEntry(Data));
      if Result
        then Scanner.NextToken
        else
          if Essential
            then raise EParser.Create(Format(emToken1ExpectedButToken2Found,['Variable', Scanner.Token]));
    end;

  function TParser.ParseWhile(var Statement: TStatement): boolean;
    var
      Expression: TExpression;
      DoStatement: TStatement;
      BlockStatement: TBlockStatement;
      Line: integer;
    begin
      Result := Parse(rwWhile, false);
      if Result
        then
          begin
            Line := succ(SourceLine);
            ParseExpression(Expression);
            if Expression.YieldType <> BooleanType
              then raise EParser.Create(emTypeOfExpressionMustBeBOOLEAN);
            Parse(rwDo);
            ParseStatement(DoStatement);
            Statement := TWhileStatement.Create(Runner, Line);
            BlockStatement := TBlockStatement.Create(Runner, Line);
            BlockStatement.Statements.Add(DoStatement);
            TWhileStatement(Statement).Expression := Expression;
            TWhileStatement(Statement).BlockStatement := BlockStatement;
            FSourceManager.Editor[FSourceManager.EditorNumber(ModuleName)].Lines.Objects[Line] := Statement;
          end;
    end;

  procedure TParser.PrintStr(const Str: string);
    begin
      if Console <> nil
        then Console.Items.Add(Str);
    end;

  procedure TParser.Restart;
    begin
      BlockList.Clear;
      BlockList.Add(SystemBlock);
      if ModuleName <> ''
        then Source := FSourceManager.Source[ModuleName];
    end;

  procedure TParser.SetSource(const Value: string);
    begin
      Scanner.Source := Value;
    end;

  procedure InitializeReservedWordsAndOperators;
    begin
      ReservedWords.Add(rwAnd      );
      ReservedWords.Add(rwArray    );
      ReservedWords.Add(rwBegin    );
      ReservedWords.Add(rwConst    );
      ReservedWords.Add(rwColVector);
      ReservedWords.Add(rwDo       );
      ReservedWords.Add(rwDownto   );
      ReservedWords.Add(rwElse     );
      ReservedWords.Add(rwEnd      );
      ReservedWords.Add(rwFile     );
      ReservedWords.Add(rwFor      );
      ReservedWords.Add(rwFunction );
      ReservedWords.Add(rwIf       );
      ReservedWords.Add(rwInitialization);
      ReservedWords.Add(rwInvertion);
      ReservedWords.Add(rwMatrix   );
      ReservedWords.Add(rwMod      );
      ReservedWords.Add(rwModule   );
      ReservedWords.Add(rwNot      );
      ReservedWords.Add(rwOf       );
      ReservedWords.Add(rwOr       );
      ReservedWords.Add(rwProcedure);
      ReservedWords.Add(rwProgram  );
      ReservedWords.Add(rwRecord   );
      ReservedWords.Add(rwRepeat   );
      ReservedWords.Add(rwRowVector);
      ReservedWords.Add(rwThen     );
      ReservedWords.Add(rwTo       );
      ReservedWords.Add(rwType     );
      ReservedWords.Add(rwUntil    );
      ReservedWords.Add(rwUses     );
      ReservedWords.Add(rwVar      );
      ReservedWords.Add(rwWhile    );
      ReservedWords.Add(rwXor      );
      Operators.Add(':=');
      Operators.Add('>');
      Operators.Add('<');
      Operators.Add('=');
      Operators.Add('>=');
      Operators.Add('<=');
      Operators.Add('<>');
      Operators.Add('+');
      Operators.Add('-');
      Operators.Add('*');
      Operators.Add('/');
      Operators.Add('[');
      Operators.Add(']');
      Operators.Add('(');
      Operators.Add(')');
      Operators.Add(',');
      Operators.Add(';');
    end;

  function TParser.ParseMatrixDeclaration(var DataType: TDataType): boolean;
    var
      Bound1, Bound2: integer;
      Data: TInteger;
    begin
      Result := Parse(rwMatrix, false);
      if Result
        then
          begin
            Parse('[');
            if not ParseIntegerNumber(TData(Data))
              then raise EParser.Create(emTypeOfExpressionMustBeINTEGER);
            Bound1 := Data.Value;
            Data.free;
            Parse(',');
            if not ParseIntegerNumber(TData(Data))
              then raise EParser.Create(emTypeOfExpressionMustBeINTEGER);
            Bound2 := Data.Value;
            Data.free;
            Parse(']');
            DataType := TMatrixType.CreateWith(Bound1, Bound2);
          end;
    end;

  function TParser.ParseFloatNumber(var Data: TData): boolean;
    begin
      if Scanner.TokenType = ttFloat
        then
          begin
            Data := RealType.Instantiate;
            TReal(Data).Value := StrToFloat(Scanner.Token);
            Scanner.NextToken;
            Result := true;
          end
        else Result := false;
    end;

  function TParser.ParseIntegerNumber(var Data: TData): boolean;
    begin
      if Scanner.TokenType = ttInteger
        then
          begin
            Data := IntegerType.Instantiate;
            TInteger(Data).Value := StrToInt(Scanner.Token);
            Scanner.NextToken;
            Result := true;
          end
        else Result := false;
    end;

  function TParser.ParseInternalDiv(var Expression: TExpression): boolean;
    begin
      Result := Parse('./', false);
      if Result
        then Expression := TInternalDiv.Create;
    end;

  function TParser.ParseInternalMult(var Expression: TExpression): boolean;
    begin
      Result := Parse('.*', false);
      if Result
        then Expression := TInternalMult.Create;
    end;

  function TParser.ParseInvertion(var Expression: TExpression): boolean;
    var
      Exp: TExpression;
    begin
      Result := Parse(rwInvertion, false);
      if Result
       then
         begin
           ParseFactor(Exp);
           Expression := TInvertion.CreateWith(Exp);
         end;
    end;

  function TParser.ParseTransposition(var Expression: TExpression): boolean;
    begin
      Result := Parse('''', false);
      if Result
        then Expression := TTransposition.CreateWith(Expression);
    end;

  function TParser.ParseVectorDeclaration(var DataType: TDataType): boolean;
    var
      Bound: integer;
      Data: TInteger;
      RowVect: boolean;
    begin
      Result := Parse(rwRowVector, false);
      RowVect := Result;
      if not Result
        then Result := Parse(rwColVector, false);
      if Result
        then
          begin
            Parse('[');
            if not ParseIntegerNumber(TData(Data))
              then raise EParser.Create(emTypeOfExpressionMustBeINTEGER);
            Bound := Data.Value;
            Data.free;
            Parse(']');
            if RowVect
              then DataType := TRowVectorType.CreateWith(Bound)
              else DataType := TColVectorType.CreateWith(Bound);
          end;
    end;

  function TParser.ParseDeterminant(var Expression: TExpression): boolean;
    begin
      Result := Parse('|', false);
      if Result
        then
          begin
            ParseExpression(Expression);
            Parse('|');
            Expression := TDeterminant.CreateWith(Expression);
          end;
    end;

  function TParser.ParseUses(var UsesList: TSymbolTable): boolean;
    var
      IdentName: string;
      Parser: TParser;

    procedure IncludeModule(const Name: string);
      var
        Index: integer;
      begin
        Index := CompiledModules.IndexOf(Name);
        if Index = -1
          then
            begin
              Parser.ModuleName := Name;
              UsesList.AddEntry(Parser.CompileModule);
            end
          else UsesList.AddEntry(TModule(CompiledModules.Objects[Index]));
      end;

    begin
      Result := Parse(rwUses, false);
      if Result
        then
          begin
            UsesList := TSymbolTable.Create;
            Parser := TParser.Create(Runner, FSourceManager);
            Parser.FConsole := Console;
            ParseIdentifier(IdentName);
            IncludeModule(IdentName);
            while Parse(',', false) do
              begin
                ParseIdentifier(IdentName);
                IncludeModule(IdentName);
              end;
            Parse(';');
          end;
    end;

  function TParser.SearchIdentifier(const What: TSymbolType; var Entry: TSymbolTableEntry): boolean;
    var
      i: integer;
      aModule: TModule;
    begin
      Scanner.SaveState;
      if ParseQualifier(aModule)
        then Result := aModule.SymbolSystem.Search(What, Scanner.Token, Entry)
        else Result := CurrentBlock.SymbolSystem.Search(What, Scanner.Token, Entry);
      if not Result and (FModule.Includes <> nil)
        then
          begin
            i := pred(FModule.Includes.Count);
            while (i >= 0) and not Result do
              begin
                Result := TBlock(FModule.Includes.Items[i]).SymbolSystem.Search(What, Scanner.Token, Entry);
                dec(i);
              end;
          end;
      if not Result
        then Scanner.RestoreState;
    end;

  function TParser.GetSourceLine: integer;
    {
    var
      Index: integer;
    }
    begin
      Result := Scanner.LineIndex;
      {
      Index := FSourceManager.EditorNumber(ModuleName);
      if Index >= 0
        then //Result := SendMessage(FSourceManager.Editor[Index].Handle, EM_EXLINEFROMCHAR, 0, Scanner.Index)
          Result := FSourceManager.Editor[Index].Scanner.LineIndex
        else Result := 0;
      }
    end;

  function TParser.ParseProgram(var Block: TBlock): boolean;
    var
      IdentName: string;
      UsesList: TSymbolTable;
    begin
      Result := false;
      try
        Result := Parse(rwProgram);
      except
        if Scanner.Scanning
          then raise;
      end;
      if Result
        then
          begin
            ParseIdentifier(IdentName);
            Parse(';');
            Block := GetFrameBlock(TModule);
            FModule := TModule(Block);
            if ParseUses(UsesList)
              then TModule(Block).Includes := UsesList;
            ParseBlock(Block, true);
            Parse('.');
            Block.Name := IdentName;
          end;
    end;

  function TParser.ParseDeclarations: boolean;
    var
      Find: boolean;
    begin
      Result := false;
      repeat
        Find := (ParseConstantDeclaration or ParseVariableDeclaration
            or ParseTypeDeclaration or ParseProcedureDeclaration
            or ParseFunctionDeclaration);
        Result := Result or Find;
      until not Find;
    end;

  function TParser.ParseInitialization(var Statement: TStatement): boolean;
    begin
      Result := Parse(rwInitialization, false);
      if Result
        then
          begin
            ParseBlockStatement(TBlockStatement(Statement));
            try
              Parse(rwEnd);
            except
              Statement.free;
              raise;
            end;
          end;
    end;

  function TParser.CompileModule: TModule;
    begin
      Restart;
      Scanner.Restart;
      Scanner.NextToken;
      CurrentBlock := SystemBlock;
      InitSource;
      ParseModule(TBlock(Result));
      MarkCodeLines;
    end;

  procedure TParser.SetModuleName(const Value: string);
    begin
      FModuleName := Value;
    end;

  function TParser.ParseQualifier(var theModule: TModule): boolean;
    var
      Entry: TSymbolTableEntry;
    begin
      if FModule.Includes = nil
        then Result := false
        else
          begin
            Result := FModule.Includes.FindEntry(Scanner.Token, Entry);
            if Result
              then
                begin
                  Scanner.NextToken;
                  Parse('.');
                  theModule := TModule(Entry);
                end;
          end;
    end;

  function TParser.ParsePower(var Expression: TExpression): boolean;
    begin
      Result := Parse('^', false);
      if Result
        then Expression := TPower.Create;
    end;

  procedure TParser.MarkCodeLines;
    var
      EditNumber: integer;
    begin
      with FSourceManager do
        begin
          EditNumber := EditorNumber(ModuleName);
          if EditNumber <> -1
            then MarkCodeLines(Editor[EditNumber]);
        end;
    end;

  procedure TParser.InitSource;
    var
      EditNumber: integer;
      Edit: TCodeEdit;
      i: integer;
    begin
      with FSourceManager do
        begin
          EditNumber := EditorNumber(ModuleName);
          if EditNumber <> -1
            then
              begin
                RemoveCodeMarks;
                Edit := Editor[EditNumber];
                for i := 0 to pred(Edit.Lines.Count) do
                  Edit.Lines.Objects[i] := nil;
              end;
        end;
    end;

initialization

    ReservedWords := TStringList.Create;
    Operators := TStringList.Create;
    CompiledModules := TStringList.Create;
    ReservedWords.Sorted := true;
    InitializeReservedWordsAndOperators;

  finalization

    ReservedWords.free;
    Operators.free;
    CompiledModules.free;

end.


