
{*******************************************************}
{                                                       }
{       Gospel Compiler System                          }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit Kernel;

interface

  uses Windows, Classes, SysUtils, Constants, Threads;

{*******************************************************}
{                                                       }
{                   Predefined Symbols                  }
{                                                       }
{*******************************************************}

  const
    sbString  = '"';

    psBoolean     = 'Boolean';
    psInteger     = 'Integer';
    psReal        = 'Real';
    psString      = 'String';
    psRecord      = 'Record';
    psArray       = 'Array';
    psMatrix      = 'Matrix';
    psColVector   = 'ColVector';
    psRowVector   = 'RowVector';
    psTrue        = 'True';
    psFalse       = 'False';


  procedure IncompatibleTypesError(const Type1, Type2: string);

  type
    ESystem = class(Exception);

    TData    = class;
    TSimple  = class;
    TOrdinal = class;
    TInteger = class;
    TReal    = class;
    TString  = class;


{*******************************************************}
{                                                       }
{               Symbol manage system                    }
{                                                       }
{*******************************************************}


    TSymbolTableEntry =
      class
        private
          FName: string;
          procedure SetName(const Value: string);
        public
          constructor CreateIdent(const aName: string);
          property Name: string read FName write SetName;
      end;

    TSymbolTable =
      class(TList)
        private
          function GetEntry(const Index: integer): TSymbolTableEntry;
        public
          property Entry[const Index: integer]: TSymbolTableEntry read GetEntry;  default;
          function Find(const S: string; var Index: Integer): boolean; virtual;
          function FindEntry(const S: string; var Entry: TSymbolTableEntry): boolean;
          procedure AddEntry(anEntry: TSymbolTableEntry);
          function HasIdentifier(const Named: string): boolean;
          procedure Clear; override;
      end;

    TBlock = class;
    TSymbolType = (stConstant, stVariable, stType, stProcedure, stFunction);
    TSymbolSystem =
      class
        private
          FProcedures: TSymbolTable;
          FVariables: TSymbolTable;
          FFunctions: TSymbolTable;
          FConstants: TSymbolTable;
          FTypes: TSymbolTable;
          FBlockOwner: TBlock;
        public
          constructor Create(aBlockOwner: TBlock);
          destructor  Destroy;  override;
          property BlockOwner: TBlock read FBlockOwner write FBlockOwner;
          property Constants: TSymbolTable read FConstants write FConstants;
          property Types: TSymbolTable read FTypes write FTypes;
          property Variables: TSymbolTable read FVariables write FVariables;
          property Procedures: TSymbolTable read FProcedures write FProcedures;
          property Functions: TSymbolTable read FFunctions write FFunctions;
          function Search(const What: TSymbolType; const Named: string; var Entry: TSymbolTableEntry): boolean;
          function HasIdentifier(const Named: string): boolean;
          function HasVariable(const Named: string): boolean;
          procedure Clear;
      end;



{*******************************************************}
{                                                       }
{                       Data Types                      }
{                                                       }
{*******************************************************}

    CDataType = class of TDataType;
    TDataType =
      class(TSymbolTableEntry)
        public
          function Instantiate: TData;  virtual; abstract;
          function Assignable(AssignType: TDataType): boolean; virtual; abstract;
      end;

    TSimpleType =
      class(TDataType)
      end;

    TOrdinalType =
      class(TSimpleType)
        public
          function Low: integer;  virtual; abstract;
          function High: integer; virtual; abstract;
          function Length: integer; virtual;
      end;

    TIntegerType =
      class(TOrdinalType)
        public
          function Instantiate: TData;  override;
          function InstantiateWith(const Value: integer): TData;
          function Assignable(AssignType: TDataType): boolean; override;
          function Low: integer;  override;
          function High: integer; override;
      end;

    TSubrangeType =
      class(TIntegerType)
        public
          LowValue:  integer;
          HighValue: integer;
          constructor Create(const ALowValue, AHighValue: integer);
          function Instantiate: TData;  override;
          function Low: integer;  override;
          function High: integer; override;
      end;

    TEnumerated = class;
    TEnumeratedType =
      class(TSubrangeType)
        public
          IdentifierList: TList;
          constructor Create;
          constructor CreateIdent(const aName: string);
          destructor  Destroy;  override;
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
          function Low: integer;  override;
          function High: integer; override;
          procedure AddIdentifier(Ident: TEnumerated);
      end;

    TBooleanType =
      class(TEnumeratedType)
        public
          function Instantiate: TData;  override;
      end;

    TRealType =
      class(TSimpleType)
        public
          function Instantiate: TData;  override;
          function InstantiateWith(const Value: extended): TData;
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TStringType =
      class(TDataType)
        public
          function Instantiate: TData;  override;
          function InstantiateWith(const Value: string): TData;  
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TStructuredType =
      class(TDataType)
        public
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TRecordType =
      class(TStructuredType)
        public
          FieldList: TSymbolTable;   // List of TDataType
          function Instantiate: TData;  override;
          constructor CreateWith(aFieldList: TSymbolTable);
          destructor  Destroy;  override;
      end;

    TArrayType =
      class(TStructuredType)
        private
          FBaseType: TDataType;
          FDimention: TOrdinalType;
        public
          constructor CreateWith(aDimention: TOrdinalType; ElemType: TDataType);
          destructor  Destroy;  override;
          function Instantiate: TData;  override;
          property BaseType: TDataType read FBaseType;
          property Dimention: TOrdinalType read FDimention;
      end;

    TProtoVectorType =
      class(TArrayType)
        private
          FElements: integer;
        public
          constructor CreateWith(const ElementsCount: integer);
          function Instantiate: TData;  override;
          property Elements: integer read FElements;
      end;

    TMatrixType =
      class(TArrayType)
        private
          FRows: integer;
          FCols: integer;
        public
          constructor CreateWith(const RowsCount, ColsCount: integer);
          destructor  Destroy;  override;
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
          property Rows: integer read FRows;
          property Cols: integer read FCols;
      end;

    TRowVectorType =
      class(TMatrixType)
        public
          constructor CreateWith(const ElementsCount: integer);
          function Instantiate: TData;  override;
      end;

    TColVectorType =
      class(TMatrixType)
        public
          constructor CreateWith(const ElementsCount: integer);
          function Instantiate: TData;  override;
      end;

    TFileType =
      class(TStructuredType)
        public
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
      end;

    TFloppyParamType =
      class(TDataType)
        private
          FBaseClass: CDataType;
        public
          constructor Create(aBaseClass: CDataType);
          function Instantiate: TData;  override;
          function Assignable(AssignType: TDataType): boolean; override;
          property BaseClass: CDataType read FBaseClass;
      end;



{*******************************************************}
{                                                       }
{                    Data Instances                     }
{                                                       }
{*******************************************************}

    TData =
      class(TSymbolTableEntry)
        private
          FDataType: TDataType;
        public
          constructor CreateFrom(aDataType: TDataType);  virtual;
          property DataType: TDataType read FDataType write FDataType;
          procedure Assign(Data: TData);  virtual; abstract;
          function Assignable(AssignType: TDataType): boolean; virtual;
          function Duplicate: TData;  virtual;
          function CastToReal: TReal;        virtual;
          function CastToString: TString;    virtual;
          // Implementation of Relational methods
          function Equality(Data: TData): TData;  virtual;
          function Inequality(Data: TData): TData;  virtual;
          function LessThan(Data: TData): TData;  virtual;
          function GreaterThan(Data: TData): TData;  virtual;
          function LessThanOrEqualTo(Data: TData): TData; virtual;
          function GreaterThanOrEqualTo(Data: TData): TData;  virtual;
          // Implementation of Arithmetic methods
          function Addition(Data: TData): TData;  virtual;
          function Subtraction(Data: TData): TData;  virtual;
          function Multiplication(Data: TData): TData;  virtual;
          function Division(Data: TData): TData;  virtual;
          function Remainder(Data: TData): TData;  virtual;
          function Negation: TData;  virtual;
          // Implementation of Boolean methods
          function BNot: TData;  virtual;
          function BAnd(Data: TData): TData;  virtual;
          function BOr(Data: TData): TData;  virtual;
          function BXor(Data: TData): TData;  virtual;
      end;

    TSimple =
      class(TData)
      end;

    TOrdinal =
      class(TSimple)
        private
          FCanBeAssigned: boolean;
        public
          constructor CreateFrom(aDataType: TDataType);  override;
          function Low: integer;  virtual;
          function High: integer; virtual;
          function Length: integer; virtual;
          property CanBeAssigned: boolean read FCanBeAssigned write FCanBeAssigned;
      end;

    TInteger =
      class(TOrdinal)
        private
          FValue: integer;
          procedure SetValue(const Value: integer);
        public
          property Value: integer read FValue write SetValue;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToReal: TReal;        override;
          function CastToString: TString;    override;
          // Implementation of Relational methods
          function Equality(Data: TData): TData;  override;
          function Inequality(Data: TData): TData;  override;
          function LessThan(Data: TData): TData;  override;
          function GreaterThan(Data: TData): TData;  override;
          function LessThanOrEqualTo(Data: TData): TData;  override;
          function GreaterThanOrEqualTo(Data: TData): TData;  override;
          // Implementation of Arithmetic methods
          function Addition(Data: TData): TData;  override;
          function Subtraction(Data: TData): TData;  override;
          function Multiplication(Data: TData): TData;  override;
          function Division(Data: TData): TData;  override;
          function Remainder(Data: TData): TData;  override;
          function Power(Data: TData): TData;
          function Negation: TData;  override;
      end;

    TSubrange =
      class(TInteger)
        private
          procedure SetValue(const Value: integer);
          function GetHighValue: integer;
          function GetLowValue: integer;
          function GetValue: integer;
        public
          procedure Assign(Data: TData);  override;
          property LowValue: integer read GetLowValue;
          property HighValue: integer read GetHighValue;
          property Value: integer read GetValue write SetValue;
      end;

    TEnumerated =
      class(TSubrange)
        public
          procedure Assign(Data: TData);  override;
          function CastToString: TString;    override;
      end;

    TBoolean =
      class(TEnumerated)
        private
          function GetValue: boolean;
          procedure SetValue(const Value: boolean);
        public
          property Value: boolean read GetValue write SetValue;
          // Implementation of Boolean methods
          function BNot: TData;  override;
          function BAnd(Data: TData): TData;  override;
          function BOr(Data: TData): TData;  override;
          function BXor(Data: TData): TData;  override;
      end;

    TReal =
      class(TSimple)
        private
          FValue: extended;
          procedure SetValue(const Value: extended);
        public
          property Value: extended read FValue write SetValue;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToReal: TReal;        override;
          function CastToString: TString;    override;
          // Implementation of Relational methods
          function Equality(Data: TData): TData;  override;
          function Inequality(Data: TData): TData;  override;
          function LessThan(Data: TData): TData;  override;
          function GreaterThan(Data: TData): TData;  override;
          function LessThanOrEqualTo(Data: TData): TData;  override;
          function GreaterThanOrEqualTo(Data: TData): TData;  override;
          // Implementation of Arithmetic methods
          function Addition(Data: TData): TData;  override;
          function Subtraction(Data: TData): TData;  override;
          function Multiplication(Data: TData): TData;  override;
          function Division(Data: TData): TData;  override;
          function Remainder(Data: TData): TData;  override;
          function Power(Data: TData): TData;
          function Negation: TData;  override;
      end;

    TString =
      class(TData)
        private
          FValue: string;
          procedure SetValue(const Value: string);
          function ExtractQuotedString(const Str: string): string;
        public
          property Value: string read FValue write SetValue;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToString: TString;    override;
          function Addition(Data: TData): TData;  override;
      end;

    TStructured =
      class(TData)
      end;

    TRecord =
      class(TStructured)
        private
          function GetField(const i: integer): TData;
        public
          FieldList: TSymbolTable;
          constructor CreateFrom(aDataType: TDataType); override;
          destructor  Destroy;  override;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToString: TString;    override;
          // Implementation of Relational methods
          function Equality(Data: TData): TData;  override;
          function Inequality(Data: TData): TData;  override;
          property Field[const i: integer]: TData read GetField;
      end;

    TArray =
      class(TStructured)
        private
          FElements: array of TData;
          function GetElement(const i: integer): TData;
          procedure SetElement(const i: integer; const Value: TData);
          function GetArrayType: TArrayType;
          procedure ReleaseElements;
          procedure AllocateElements;
        public
          constructor CreateFrom(aDataType: TDataType); override;
          destructor  Destroy;  override;
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToString: TString;    override;
          function Value(const Index: integer): TData;
          property Element[const i: integer]: TData read GetElement write SetElement;
          property ArrayType: TArrayType read GetArrayType;
          // Implementation of Relational methods
          function Equality(Data: TData): TData;  override;
          function Inequality(Data: TData): TData;  override;
      end;

    TProtoVector =
      class(TArray)
        private
          function GetElement(const Index: integer): TReal;
        public
          function Addition(Data: TData): TData;  override;
          function Subtraction(Data: TData): TData;  override;
          function Negation: TData;  override;
          function Contraction(Data: TData): TData;
          function Dilation(Data: TData): TData;
          function InternalMult(Data: TData): TData;
          function InternalDiv(Data: TData): TData;
          property Element[const Index: integer]: TReal read GetElement;
      end;

    TMatrix =
      class(TArray)
        private
          procedure SwapRows(const r1, r2: integer);
          function StepDown(const Col: integer; const Normalize: boolean): boolean;      // esto retorna false si la matrix es singular
          function StepUp(const Col: integer; const Normalize: boolean): boolean;        // esto retorna false si la matrix es singular
          function DoRungDown(const BreakOnSingular: boolean; const Normalize: boolean): boolean;  // esto retorna false si la matrix es singular
          function DoRungUp(const BreakOnSingular: boolean; const Normalize: boolean): boolean;    // esto retorna false si la matrix es singular
          function GetRank: integer;
          function GetSingular: boolean;
          function GetElement(const aRow, aCol: integer): TReal;
          function GetRow(const i: integer): TProtoVector;
          procedure SetRow(const i: integer; const Value: TProtoVector);
        public
          function Addition(Data: TData): TData;  override;      // +
          function Subtraction(Data: TData): TData;  override;   // -
          function Multiplication(Data: TData): TData;  override;// *
          function Negation: TData;  override;                   // -
          function Contraction(Data: TData): TData; virtual;     // *
          function Dilation(Data: TData): TData;    virtual;     // /
          function InternalMult(Data: TData): TData; virtual;    // .*
          function InternalDiv(Data: TData): TData; virtual;     // ./
          function Transposition: TData; virtual;                // '
          function Invertion: TData; virtual;                    // inv
          function Determinant: TData; virtual;                  // | |
        public
          function RungDown: TData;
          function RungUp: TData;
          function Rank: TData;
          property Singular: boolean read GetSingular;
          property Row[const i: integer]: TProtoVector read GetRow write SetRow;
          property Element[const Row, Col: integer]: TReal read GetElement;
      end;

    TRowVector =
      class(TMatrix)
        private
          function GetElement(const i: integer): TReal;
        public
          property Element[const i: integer]: TReal read GetElement;
      end;

    TColVector =
      class(TMatrix)
        private
          function GetElement(const i: integer): TReal;
        public
          property Element[const i: integer]: TReal read GetElement;
      end;

    TFile =
      class(TData)
        public
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
      end;

    TFloatingData =
      class(TData)
        protected
          function GetValue: TData;  virtual; abstract;
        public
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          function CastToString: TString;    override;
          property Value: TData read GetValue;
      end;

    TExpression = class;
    TArrayIndex =
      class(TFloatingData)
        private
          FArray_: TData;
          FExpression: TExpression;
        protected
          function GetValue: TData; override;
        public
          constructor Create(anArray: TData; anExpression: TExpression);
          destructor  Destroy;  override;
          property Array_: TData read FArray_;
          property Expression: TExpression read FExpression;
      end;

    TFloatingField =
      class(TFloatingData)
        private
          FRecord: TFloatingData;
          FFieldId: integer;
        protected
          function GetValue: TData; override;
        public
          constructor Create(aRecord: TFloatingData; const aFieldId: integer);
          property Record_: TFloatingData read FRecord;
          property FieldId: integer read FFieldId;
      end;

    TFloppyParam =
      class(TData)
        private
          FParameter: TData;
        public
          procedure Assign(Data: TData);  override;
          function Duplicate: TData;  override;
          property Parameter: TData read FParameter;
      end;



{*******************************************************}
{                                                       }
{                       Modularity                      }
{                                                       }
{*******************************************************}

    CBlocks = class of TBlock;
    TBlockStatement = class;
    TBlock =
      class(TSymbolTableEntry)
        private
          FSymbolSystem: TSymbolSystem;
          FBody: TBlockStatement;
          FOwner: TBlock;
          function GetConstants: TSymbolTable;
          function GetFunctions: TSymbolTable;
          function GetProcedures: TSymbolTable;
          function GetTypes: TSymbolTable;
          function GetVariables: TSymbolTable;
        public
          constructor Create;
          destructor  Destroy;  override;
          property Owner: TBlock read FOwner write FOwner;
          property SymbolSystem: TSymbolSystem read FSymbolSystem write FSymbolSystem;
          property Body: TBlockStatement read FBody write FBody;
          property Constants: TSymbolTable read GetConstants;
          property Variables: TSymbolTable read GetVariables;
          property Types: TSymbolTable read GetTypes;
          property Procedures: TSymbolTable read GetProcedures;
          property Functions: TSymbolTable read GetFunctions;
      end;

    TModule =
      class(TBlock)
        private
          FIncludes: TSymbolTable;
          FFileName: TFileName;
          procedure SetFileName(const Value: TFileName);
        public
          property FileName: TFileName read FFileName write SetFileName;
          property Includes: TSymbolTable read FIncludes write FIncludes;
      end;

    TProcedure =
      class(TBlock)
        private
          FParameters: TList;
          function GetParameter(const Index: integer): TData;
        public
          constructor Create;
          destructor Destroy;  override;
          property Parameters: TList read FParameters write FParameters;
          property Parameter[const Index: integer]: TData read GetParameter;
          procedure Execute; virtual;
      end;

    TFunction =
      class(TProcedure)
        private
          FResult: TData;
          procedure SetResult(const Value: TData);
        public
          property Result: TData read FResult write SetResult;
      end;

    TSingleMathFunction =
      class(TFunction)
        public
          constructor CreateIdent(const aName: string);
      end;

    TBinaryMathFunction =
      class(TFunction)
        public
          constructor CreateIdent(const aName: string);
      end;



{*******************************************************}
{                                                       }
{                       Expressions                     }
{                                                       }
{*******************************************************}

    TExpression =
      class
        protected
          function GetYieldType: TDataType; virtual;
          procedure TypeCheck;  virtual;
          procedure Forbitness;  virtual;
        public
          function Evaluate: TData;  virtual;
          property YieldType: TDataType read GetYieldType;
      end;

    TBinaryExpression =
      class(TExpression)
        private
          FRightExp: TExpression;
          FLeftExp: TExpression;
          procedure SetLeftExp(const Value: TExpression);
          procedure SetRightExp(const Value: TExpression);
        protected
          function GetYieldType: TDataType; override;
          procedure TypeCheck;  override;
        public
          constructor CreateWith(aLeftExp, aRightExp: TExpression);
          property LeftExp: TExpression read FLeftExp write SetLeftExp;
          property RightExp: TExpression read FRightExp write SetRightExp;
      end;

    TUnaryExpression =
      class(TExpression)
        private
          FExp: TExpression;
          procedure SetExp(const Value: TExpression);
        protected
          function GetYieldType: TDataType; override;
        public
          constructor CreateWith(anExpression: TExpression);
          property Exp: TExpression read FExp write SetExp;
      end;

    TCastingToReal =
      class(TUnaryExpression)
        protected
          procedure Forbitness;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TCastingToString =
      class(TUnaryExpression)
        protected
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TRelation =
      class(TBinaryExpression)
        protected
          function GetYieldType: TDataType; override;
      end;

    TEquality =
      class(TRelation)
        public
          function Evaluate: TData;  override;
      end;

    TInequality =
      class(TRelation)
        public
          function Evaluate: TData;  override;
      end;

    TLessThan =
      class(TRelation)
        public
          procedure Forbitness;  override;
          function Evaluate: TData;  override;
      end;

    TLessThanOrEqualTo =
      class(TRelation)
        public
          procedure Forbitness;  override;
          function Evaluate: TData;  override;
      end;

    TGreaterThan =
      class(TRelation)
        public
          procedure Forbitness;  override;
          function Evaluate: TData;  override;
      end;

    TGreaterThanOrEqualTo =
      class(TRelation)
        public
          procedure Forbitness;  override;
          function Evaluate: TData;  override;
      end;

    TBinArithmetic =
      class(TBinaryExpression)
        protected
          procedure Forbitness;  override;
      end;

    TAddition =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
          procedure TypeCheck;  override;
        public
          function Evaluate: TData;  override;
      end;

    TSubtraction =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TMultiplication =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
          procedure TypeCheck;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TDivision =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
          procedure TypeCheck;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TRemainder =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TPower =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
          procedure TypeCheck;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TInternalMult =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TInternalDiv =
      class(TBinArithmetic)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TBinBoolean =
      class(TBinaryExpression)
        protected
          procedure Forbitness;  override;
      end;

    TBooleanOr =
      class(TBinBoolean)
        public
          function Evaluate: TData;  override;
      end;

    TBooleanXor =
      class(TBinBoolean)
        public
          function Evaluate: TData;  override;
      end;

    TBooleanAnd =
      class(TBinBoolean)
        public
          function Evaluate: TData;  override;
      end;

    TBooleanNot =
      class(TUnaryExpression)
        protected
          procedure TypeCheck;  override;
        public
          function Evaluate: TData;  override;
      end;

    TNegation =
      class(TUnaryExpression)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TInvertion =
      class(TUnaryExpression)
        protected
          procedure Forbitness;  override;
        public
          function Evaluate: TData;  override;
      end;

    TTransposition =
      class(TUnaryExpression)
        protected
          procedure Forbitness;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;

    TDeterminant =
      class(TUnaryExpression)
        protected
          procedure Forbitness;  override;
          function GetYieldType: TDataType; override;
        public
          function Evaluate: TData;  override;
      end;


    TSimpleData =
      class(TExpression)
        private
          FData: TData;
          procedure SetData(const Value: TData);
        protected
          function GetYieldType: TDataType; override;
        public
          constructor Create(aData: TData);
          property Data: TData read FData write SetData;
          function Evaluate: TData;  override;
      end;

    TFunctionCallStatement = class;
    TFunctionEvaluation =
      class(TExpression)
        private
          FFunctionCall: TFunctionCallStatement;
        protected
          function GetYieldType: TDataType; override;
        public
          property FunctionCall: TFunctionCallStatement read FFunctionCall write FFunctionCall;
          function Evaluate: TData;  override;
      end;



{*******************************************************}
{                                                       }
{                 Statements Runner                     }
{                                                       }
{*******************************************************}

    TBreakingExecution = procedure(const Line: integer) of object;
    TStatement = class;

    TRunner =
      class
        private
          FStartPoint: TStatement;
          FLineBreak: integer;
          FOnBreakingExecution: TBreakingExecution;
          FOnResumeExecution: TBreakingExecution;
          RunInstance: TExclusiveThread;
          FRuning: boolean;
          FOnTerminateEvent: TNotifyEvent;
          LastStatement: TStatement;
          procedure SetStartPoint(const Value: TStatement);
          procedure ManageRun(const params : array of const);
        public
          Breaking: boolean;
          FStepOver: boolean;
          constructor Create(AnOnBreakingExecution, AnOnResumeExecution: TBreakingExecution);
          destructor Destroy;  override;
          procedure RunApplication;
          procedure Run(Statement: TStatement);
          procedure TraceInto;
          procedure StepOver;
          procedure RunToLine(const Line: integer);
          procedure Pause;
          procedure Reset;
          property StartPoint: TStatement read FStartPoint write SetStartPoint;
          property OnBreakingExecution: TBreakingExecution read FOnBreakingExecution write FOnBreakingExecution;
          property OnResumeExecution: TBreakingExecution read FOnResumeExecution write FOnResumeExecution;
          property Runing: boolean read FRuning write FRuning;
          property OnTerminateEvent: TNotifyEvent read FOnTerminateEvent write FOnTerminateEvent;
      end;


{*******************************************************}
{                                                       }
{                       Statements                      }
{                                                       }
{*******************************************************}

    TStatement =
      class
        private
          FSourceLine: integer;
          FRunner: TRunner;
          FHasBreakpoint: boolean;
          procedure SetHasBreakpoint(const Value: boolean);
        public
          constructor Create(aRunner: TRunner; const aSourceLine: integer);
          procedure Execute; virtual; abstract;
          property SourceLine: integer read FSourceLine write FSourceLine;
          property Runner: TRunner read FRunner;
          property HasBreakpoint: boolean read FHasBreakpoint write SetHasBreakpoint;
      end;

    TAssignation =
      class(TStatement)
        private
          FData: TData;
          FExpression: TExpression;
        public
          property Data: TData read FData write FData;
          property Expression: TExpression read FExpression write FExpression;
          procedure Execute; override;
      end;

    TBlockStatement =
      class(TStatement)
        private
          FStatements: TList;
        public
          constructor Create(aRunner: TRunner; const aSourceLine: integer);
          destructor  Destroy;  override;
          property Statements: TList read FStatements write FStatements;
          procedure Execute; override;
      end;

    TIfStatement =
      class(TStatement)
        private
          FExpression: TExpression;
          FElseStatement: TStatement;
          FThenStatement: TStatement;
          function GetHasElseClause: boolean;
        public
          property Expression: TExpression read FExpression write FExpression;
          property ThenStatement: TStatement read FThenStatement write FThenStatement;
          property ElseStatement: TStatement read FElseStatement write FElseStatement;
          property HasElseClause: boolean read GetHasElseClause;
          procedure Execute; override;
      end;

    TWhileStatement =
      class(TStatement)
        private
          FExpression: TExpression;
          FBlockStatement: TBlockStatement;
          function EvalExpression: boolean;
        public
          property BlockStatement: TBlockStatement read FBlockStatement write FBlockStatement;
          property Expression: TExpression read FExpression write FExpression;
         procedure Execute; override;
      end;

    TRepeatStatement =
      class(TStatement)
        private
          FExpression: TExpression;
          FBlockStatement: TBlockStatement;
          function EvalExpression: boolean;
        public
          property BlockStatement: TBlockStatement read FBlockStatement write FBlockStatement;
          property Expression: TExpression read FExpression write FExpression;
          procedure Execute; override;
      end;

    TLoopDirection = (ldTo, ldDownto);
    TForStatement =
      class(TStatement)
        private
          FCounter: TInteger;
          FFinalExp: TExpression;
          FInitialExp: TExpression;
          FLoopDirection: TLoopDirection;
          FStatement: TStatement;
        public
          property Counter: TInteger read FCounter write FCounter;
          property InitialExp: TExpression read FInitialExp write FInitialExp;
          property FinalExp: TExpression read FFinalExp write FFinalExp;
          property Statement: TStatement read FStatement write FStatement;
          property LoopDirection: TLoopDirection read FLoopDirection write FLoopDirection;
          procedure Execute; override;
      end;

    TProcedureCallStatement =
      class(TStatement)
        private
          FRoutine: TProcedure;
          FParameters: TList;
          procedure ParameterPassing;
          procedure PushParams(Stack: TList);
          procedure PopParams(Stack: TList);
        public
          property Routine: TProcedure read FRoutine write FRoutine;
          property Parameters: TList read FParameters write FParameters;
          procedure Execute; override;
      end;

    TFunctionCallStatement =
      class(TProcedureCallStatement)
        private
          function GetRoutine: TFunction;
          procedure SetRoutine(const Value: TFunction);
        public
          property Routine: TFunction read GetRoutine write SetRoutine;
      end;

    TOutPutMsg = procedure(const Str: string) of object;
    TOutputStatement =
      class(TStatement)
        private
          FExpression: TExpression;
          FOutPut: TOutPutMsg;
        public
          property Expression: TExpression read FExpression write FExpression;
          property OutPut: TOutPutMsg read FOutPut write FOutPut;
          procedure Execute; override;
      end;

    TMoveCarProc = procedure (const aX, aY, aZ, aPhi, aCita, aPsi: real) of object;
    TMoveAngProc = procedure (const Q1, Q2, Q3, Q4, Q5: real) of object;

    TMainHandler =
      class
        private
          FMoveCarProc:  TMoveCarProc;
          FMoveAngProc:  TMoveAngProc;
        public
          property MoveCarProc: TMoveCarProc read FMoveCarProc write FMoveCarProc;
          property MoveAngProc: TMoveAngProc read FMoveAngProc write FMoveAngProc;
      end;

  var
    IntegerType: TIntegerType;
    RealType:    TRealType;
    StringType:  TStringType;
    BooleanType: TBooleanType;
    TrueIdent:   TEnumerated;
    FalseIdent:  TEnumerated;
    SystemBlock: TBlock;
    MainHandler: TMainHandler;

implementation

  uses Math;

  const
    DEFAULTARRAYSIZE = 16;

  procedure IncompatibleTypesError(const Type1, Type2: string);
    begin
      if Type1 = Type2
        then
          if Type1 = psMatrix
            then raise ESystem.Create(emMatrixTypeMismatch)
            else raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[Type1, Type2]));
    end;

{ TIntegerType }

  function TIntegerType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := (AssignType is TIntegerType) and not (AssignType is TEnumeratedType);
    end;

  function TIntegerType.High: integer;
    begin
      Result := System.High(integer);
    end;

  function TIntegerType.Instantiate: TData;
    begin
      Result := TInteger.CreateFrom(Self);
    end;

  function TIntegerType.InstantiateWith(const Value: integer): TData;
    begin
      Result := TInteger.CreateFrom(Self);
      TInteger(Result).Value := Value;
    end;

  function TIntegerType.Low: integer;
    begin
      Result := System.Low(integer);
    end;

{ TEnumeratedType }

  procedure TEnumeratedType.AddIdentifier(Ident: TEnumerated);
    begin
      Ident.Value := IdentifierList.Count;
      IdentifierList.Add(Ident);
      HighValue := IdentifierList.Count;
    end;

  function TEnumeratedType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

  constructor TEnumeratedType.Create;
    begin
      inherited Create(0, 0);
      IdentifierList := TList.Create;
    end;

  constructor TEnumeratedType.CreateIdent(const aName: string);
    begin
      inherited CreateIdent(aName);
      IdentifierList := TList.Create;
    end;

  destructor TEnumeratedType.Destroy;
    begin
      IdentifierList.free;
      inherited;
    end;

  function TEnumeratedType.High: integer;
    begin
      Result := pred(IdentifierList.Count);
    end;

  function TEnumeratedType.Instantiate: TData;
    begin
      Result := TEnumerated.CreateFrom(Self);
    end;

  function TEnumeratedType.Low: integer;
    begin
      Result := 0;
    end;

{ TSubrangeType }

  constructor TSubrangeType.Create(const ALowValue, AHighValue: integer);
    begin
      inherited Create;
      LowValue := ALowValue;
      HighValue := AHighValue;
    end;

  function TSubrangeType.High: integer;
    begin
      Result := HighValue;
    end;

  function TSubrangeType.Instantiate: TData;
    begin
      Result := TSubrange.CreateFrom(Self);
    end;

  function TSubrangeType.Low: integer;
    begin
      Result := LowValue;
    end;

{ TRealType }

  function TRealType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := (AssignType = RealType) or
                ((AssignType is TIntegerType) and
                not (AssignType is TEnumeratedType));
    end;

  function TRealType.Instantiate: TData;
    begin
      Result := TReal.CreateFrom(Self);
    end;

  function TRealType.InstantiateWith(const Value: extended): TData;
    begin
      Result := TReal.CreateFrom(Self);
      TReal(Result).Value := Value;
    end;

{ TRecordType }

  constructor TRecordType.CreateWith(aFieldList: TSymbolTable);
    begin
      inherited Create;
      FieldList := aFieldList;
      Name := psRecord;
    end;

  destructor TRecordType.Destroy;
    begin
      FieldList.free;
      inherited;
    end;

  function TRecordType.Instantiate: TData;
    begin
      Result := TRecord.CreateFrom(Self);
    end;

{ TArrayType }

  constructor TArrayType.CreateWith(aDimention: TOrdinalType; ElemType: TDataType);
    begin
      inherited Create;
      FBaseType := ElemType;
      FDimention := aDimention;
      Name := psArray;
    end;

  destructor TArrayType.Destroy;
    begin
      FDimention.free;
      inherited;
    end;

  function TArrayType.Instantiate: TData;
    begin
      Result := TArray.CreateFrom(Self);
    end;

{ TInteger }

  function TInteger.Addition(Data: TData): TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := Value + TInteger(Data).Value;
    end;

  procedure TInteger.Assign(Data: TData);
    begin
      if Data is TInteger
        then Value := TInteger(Data).Value
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TInteger.CastToReal: TReal;
    begin
      Result := TReal(RealType.Instantiate);
      TReal(Result).Value := Value;
    end;

  function TInteger.CastToString: TString;
    begin
      Result := TString(StringType.Instantiate);
      TString(Result).Value := IntToStr(Value);
    end;

  function TInteger.Division(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Value / TInteger(Data).Value;
    end;

  function TInteger.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      TInteger(Result).Value := Value;
    end;

  function TInteger.Equality(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value = TInteger(Data).Value;
    end;

  function TInteger.GreaterThan(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value > TInteger(Data).Value;
    end;

  function TInteger.GreaterThanOrEqualTo(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value >= TInteger(Data).Value;
    end;

  function TInteger.Inequality(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value <> TInteger(Data).Value;
    end;

  function TInteger.LessThan(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value < TInteger(Data).Value;
    end;

  function TInteger.LessThanOrEqualTo(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).value := Value <= TInteger(Data).Value;
    end;

  function TInteger.Multiplication(Data: TData): TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := Value * TInteger(Data).Value;
    end;

  function TInteger.Negation: TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := - Value;
    end;

  function TInteger.Power(Data: TData): TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := Trunc(Math.Power(Value, TInteger(Data).Value));
    end;

  function TInteger.Remainder(Data: TData): TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := Value mod TInteger(Data).Value;
    end;

  procedure TInteger.SetValue(const Value: integer);
    begin
      FValue := Value;
    end;

  function TInteger.Subtraction(Data: TData): TData;
    begin
      Result := IntegerType.Instantiate;
      TInteger(Result).Value := Value - TInteger(Data).Value;
    end;

{ TEnumerated }

  procedure TEnumerated.Assign(Data: TData);
    begin
      if Data.DataType = DataType
        then Value := TEnumerated(Data).Value
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TEnumerated.CastToString: TString;
    begin
      Result := TString(StringType.Instantiate);
      TString(Result).Value := TEnumerated(TEnumeratedType(DataType).IdentifierList.Items[Value]).Name;
    end;

{ TSubrange }

  procedure TSubrange.Assign(Data: TData);
    begin
      if Data is TInteger
        then Value := TInteger(Data).Value
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TSubrange.GetHighValue: integer;
    begin
      Result := TSubrangeType(DataType).HighValue;
    end;

  function TSubrange.GetLowValue: integer;
    begin
      Result := TSubrangeType(DataType).LowValue;
    end;

  function TSubrange.GetValue: integer;
    begin
      Result := inherited Value;
    end;

  procedure TSubrange.SetValue(const Value: integer);
    begin
      if (Value >= LowValue) and (Value <= HighValue)
        then FValue := Value
        else raise ESystem.Create(Format('Value out of range %d - %d', [LowValue, HighValue]));
    end;

{ TReal }

  function TReal.Addition(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Value + TReal(Data).Value;
    end;

  procedure TReal.Assign(Data: TData);
    begin
      if Data is TReal
        then Value := TReal(Data).Value
        else
          if Data is TInteger
            then Value := TInteger(Data).Value
            else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TReal.CastToReal: TReal;
    begin
      Result := TReal(Duplicate);
    end;

  function TReal.CastToString: TString;
    begin
      Result := TString(StringType.Instantiate);
      TString(Result).Value := FloatToStr(Value);
    end;

  function TReal.Division(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Value / TReal(Data).Value;
    end;

  function TReal.Duplicate: TData;
    begin
      Result := TReal.CreateFrom(DataType);
      TReal(Result).Value := Value;
    end;

  function TReal.Equality(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value = TReal(Data).Value;
    end;

  function TReal.GreaterThan(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value > TReal(Data).Value;
    end;

  function TReal.GreaterThanOrEqualTo(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value >= TReal(Data).Value;
    end;

  function TReal.Inequality(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value <> TReal(Data).Value;
    end;

  function TReal.LessThan(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value < TReal(Data).Value;
    end;

  function TReal.LessThanOrEqualTo(Data: TData): TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Value <= TReal(Data).Value;
    end;

  function TReal.Multiplication(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Value * TReal(Data).Value;
    end;

  function TReal.Negation: TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := - Value;
    end;

  function TReal.Power(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Math.Power(Value, TReal(Data).Value);
    end;

  function TReal.Remainder(Data: TData): TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  procedure TReal.SetValue(const Value: extended);
    begin
      FValue := Value;
    end;

  function TReal.Subtraction(Data: TData): TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := Value - TReal(Data).Value;
    end;

{ TRecord }

  procedure TRecord.Assign(Data: TData);
    var
      i: integer;
    begin
      if Data.DataType = DataType
        then
          for i := 0 to pred(FieldList.Count) do
            Field[i].Assign(TRecord(Data).Field[i])
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TRecord.CastToString: TString;
    var
      i: integer;
      Str: string;
      TmpStr: TString;
    begin
      Result := TString(StringType.Instantiate);
      Str := '(';
      for i := 0 to pred(FieldList.Count) do
        begin
          Str := Str + Field[i].Name + ': ';
          TmpStr := Field[i].CastToString;
          Str := Str + TmpStr.Value;
          TmpStr.free;
          if i < pred(FieldList.Count)
            then Str := Str + '; ';
        end;
      Str := Str + ')';
      TString(Result).Value := Str;
    end;

  constructor TRecord.CreateFrom(aDataType: TDataType);
    var
      i: integer;
      Data: TData;
    begin
      inherited CreateFrom(aDataType);
      FieldList := TSymbolTable.Create;
      for i := 0 to pred(TRecordType(DataType).FieldList.Count) do
        begin
          Data := TData(TRecordType(DataType).FieldList[i]).Duplicate;
          Data.Name := TRecordType(DataType).FieldList[i].Name;
          FieldList.AddEntry(Data);
        end;
    end;

  destructor TRecord.Destroy;
    begin
      FieldList.free;
      inherited;
    end;

  function TRecord.Duplicate: TData;
    var
      i: integer;
    begin
      Result := TRecord.CreateFrom(DataType);
      for i := 0 to pred(FieldList.Count) do
        TRecord(Result).Field[i].Assign(Field[i]);
    end;

  function TRecord.Equality(Data: TData): TData;
    var
      i: integer;
      Equals: boolean;
      Comp: TBoolean;
    begin
      Equals := true;
      i := 0;
      while (i < Fieldlist.Count) and Equals do
        begin
          Comp := TBoolean(Field[i].Equality(TRecord(Data).Field[i]));
          Equals := Comp.Value;
          Comp.free;
          inc(i);
        end;
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Equals;
    end;

  function TRecord.GetField(const i: integer): TData;
    begin
      Result := TData(FieldList[i]);
    end;

  function TRecord.Inequality(Data: TData): TData;
    begin
      Result := Equality(Data);
      TBoolean(Result).Value := not TBoolean(Result).Value;
    end;

{ TArray }

  procedure TArray.AllocateElements;
    var
      i: integer;
    begin
      for i := 0 to pred(ArrayType.Dimention.Length) do
        FElements[i] := ArrayType.BaseType.Instantiate;
    end;

  procedure TArray.Assign(Data: TData);
    var
      i: integer;
    begin
      for i := 1 to Length(FElements) do
        Element[i].Assign(TArray(Data).Element[i]);
    end;

  function TArray.CastToString: TString;
    var
      i: integer;
      Str: string;
      TmpStr: TString;
    begin
      Result := TString(StringType.Instantiate);
      Str := '[';
      for i := 1 to ArrayType.Dimention.Length do
        begin
          TmpStr := Element[i].CastToString;
          Str := Str + TmpStr.Value;
          TmpStr.free;
          if i < ArrayType.Dimention.Length
            then Str := Str + ', ';
        end;
      Str := Str + ']';
      TString(Result).Value := Str;
    end;

  constructor TArray.CreateFrom(aDataType: TDataType);
    begin
      inherited CreateFrom(aDataType);
      Setlength(FElements, ArrayType.Dimention.Length);
      //FillChar(FElements, SizeOf(Pointer) * ArrayType.ElementsCount, 0);
      if ArrayType.Dimention.Length <= DEFAULTARRAYSIZE
        then AllocateElements;
    end;

  destructor TArray.Destroy;
    begin
      ReleaseElements;
      inherited;
    end;

  function TArray.Duplicate: TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to ArrayType.Dimention.Length do
        TArray(Result).Element[i].Assign(Element[i]);
    end;

  function TArray.Equality(Data: TData): TData;
    var
      i: integer;
      Equals: boolean;
      Comp: TBoolean;
    begin
      Equals := true;
      i := 1;
      while (i <= ArrayType.Dimention.Length) and Equals do
        begin
          Comp := TBoolean(Element[i].Equality(TArray(Data).Element[i]));
          Equals := Comp.Value;
          Comp.free;
          inc(i);
        end;
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := Equals;
    end;

  function TArray.GetArrayType: TArrayType;
    begin
      Result := TArrayType(FDataType);
    end;

  function TArray.GetElement(const i: integer): TData;
    var
      Index: integer;
    begin
      Index := pred(i);
      if FElements[Index] = nil
        then FElements[Index] := ArrayType.BaseType.Instantiate;
      Result := FElements[Index];
    end;

  function TArray.Inequality(Data: TData): TData;
    begin
      Result := Equality(Data);
      TBoolean(Result).Value := not TBoolean(Result).Value;
    end;

  procedure TArray.ReleaseElements;
    var
      i: integer;
    begin
      for i := 1 to Length(FElements) do
        Element[i].free;
      Finalize(FElements);
    end;

  procedure TArray.SetElement(const i: integer; const Value: TData);
    begin
      FElements[pred(i)] := Value;
    end;

  function TArray.Value(const Index: integer): TData;
    begin
      Result := Element[Index];
    end;

{ TData }

  function TData.Addition(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Assignable(AssignType: TDataType): boolean;
    begin
      Result := DataType.Assignable(AssignType);
    end;

  function TData.BAnd(Data: TData): TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TData.BNot: TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TData.BOr(Data: TData): TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TData.BXor(Data: TData): TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TData.CastToReal: TReal;
    begin
      raise ESystem.Create(emInvalidTypecast);
    end;

  function TData.CastToString: TString;
    begin
      raise ESystem.Create(emInvalidTypecast);
    end;

  constructor TData.CreateFrom(aDataType: TDataType);
    begin
      inherited Create;
      FDataType := aDataType;
    end;

  function TData.Division(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Duplicate: TData;
    begin
      raise ESystem.Create('Can not create abstract type');
    end;

  function TData.Equality(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.GreaterThan(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.GreaterThanOrEqualTo(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Inequality(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.LessThan(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.LessThanOrEqualTo(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Multiplication(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Negation: TData;
    begin
      raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
    end;

  function TData.Remainder(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TData.Subtraction(Data: TData): TData;
    begin
      if DataType.Name = Data.DataType.Name
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType)
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

{ TFileType }

  function TFileType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

  function TFileType.Instantiate: TData;
    begin
      Result := TFile.CreateFrom(Self);
    end;

{ TFile }

  procedure TFile.Assign(Data: TData);
    begin
      // Do nothing
    end;

  function TFile.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      Result.Assign(Self);
    end;

{ TSymbolTableEntry }

  constructor TSymbolTableEntry.CreateIdent(const aName: string);
    begin
      inherited Create;
      FName := aName;
    end;

  procedure TSymbolTableEntry.SetName(const Value: string);
    begin
      FName := Value;
    end;

{ TSymbolTable }

  procedure TSymbolTable.AddEntry(anEntry: TSymbolTableEntry);
    var
      Index: integer;
    begin
      Find(anEntry.Name, Index);
      if (Count > Index) and (Entry[Index].Name = anEntry.Name)
        then raise ESystem.Create(Format(emIdentifierRedeclared, [anEntry.Name]));
      Insert(Index, anEntry);
    end;

  procedure TSymbolTable.Clear;
    var
      i: integer;
    begin
      for i := 0 to pred(Count) do
        TSymbolTableEntry(Items[i]).free;
      inherited Clear;
    end;

  function TSymbolTable.Find(const S: string; var Index: Integer): Boolean;
    var
      L, H, I, C: Integer;
    begin
      Result := False;
      L := 0;
      H := Count - 1;
      while L <= H do
      begin
        I := (L + H) shr 1;
        C := AnsiCompareText(UpperCase(Entry[I].Name), UpperCase(S));
        if C < 0 then L := I + 1 else
        begin
          H := I - 1;
          if C = 0 then
          begin
            Result := True;
            //if Duplicates <> dupAccept then L := I;
            L := I;
          end;
        end;
      end;
      Index := L;
    end;

  function TSymbolTable.FindEntry(const S: string; var Entry: TSymbolTableEntry): boolean;
    var
      Index: integer;
    begin
      Result := Find(S, Index);
      if Result
        then Entry := Self.Entry[Index];
    end;

  function TSymbolTable.GetEntry(const Index: integer): TSymbolTableEntry;
    begin
      Result := TSymbolTableEntry(Items[Index]);
    end;

  function TSymbolTable.HasIdentifier(const Named: string): boolean;
    var
      Index: integer;
    begin
      Result := Find(Named, Index);
    end;

{ TStringType }

  function TStringType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = StringType;
    end;

  function TStringType.Instantiate: TData;
    begin
      Result := TString.CreateFrom(Self);
    end;

  function TStringType.InstantiateWith(const Value: string): TData;
    begin
      Result := TString.CreateFrom(Self);
      TString(Result).Value := Value;
    end;

{ TString }

  function TString.Addition(Data: TData): TData;
    begin
      Result := StringType.Instantiate;
      TString(Result).Value := Value + TString(Data).Value;
    end;

  procedure TString.Assign(Data: TData);
    begin
      if Data is TString
        then Value := TString(Data).Value
        else IncompatibleTypesError(DataType.Name, Data.DataType.Name);
    end;

  function TString.CastToString: TString;
    begin
      Result := TString(Duplicate);
    end;

  function TString.Duplicate: TData;
    begin
      Result := TString.CreateFrom(DataType);
      TString(Result).Value := Value;
    end;

  function TString.ExtractQuotedString(const Str: string): string;
    var
      S: PChar;
    begin
      if (Length(Str) > 0) and (Str[1] = sbString)
        then
          begin
            S := StrNew(PChar(Str));
            Result := AnsiExtractQuotedStr(S, sbString);
            //StrDispose(S);
          end
        else Result := Str;
    end;

  procedure TString.SetValue(const Value: string);
    begin
      FValue := ExtractQuotedString(Value);
    end;

{ TProcedure }

  constructor TProcedure.Create;
    begin
      inherited;
      FParameters := TList.Create;
    end;

  destructor TProcedure.Destroy;
    begin
      FParameters.free;
      inherited;
    end;

  procedure TProcedure.Execute;
    begin
      Body.Runner.Run(Body);
    end;

  function TProcedure.GetParameter(const Index: integer): TData;
    begin
      Result := TData(Parameters[Index]);
    end;

{ TBooleanType }

  function TBooleanType.Instantiate: TData;
    begin
      Result := TBoolean.CreateFrom(Self);
    end;

{ TBoolean }

  function TBoolean.BAnd(Data: TData): TData;
    begin
      if Data is TBoolean
        then
          begin
            Result := BooleanType.Instantiate;
            TBoolean(Result).Value := Value and TBoolean(Data).Value;
          end
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TBoolean.BNot: TData;
    begin
      Result := BooleanType.Instantiate;
      TBoolean(Result).Value := not Value;
    end;

  function TBoolean.BOr(Data: TData): TData;
    begin
      if Data is TBoolean
        then
          begin
            Result := BooleanType.Instantiate;
            TBoolean(Result).Value := Value or TBoolean(Data).Value;
          end
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TBoolean.BXor(Data: TData): TData;
    begin
      if Data is TBoolean
        then
          begin
            Result := BooleanType.Instantiate;
            TBoolean(Result).Value := Value xor TBoolean(Data).Value;
          end
        else raise ESystem.Create(Format(emIncompatibleTypesName1AndName2,[DataType.Name, Data.DataType.Name]));
    end;

  function TBoolean.GetValue: boolean;
    begin
      Result := inherited Value = 1;
    end;

  procedure TBoolean.SetValue(const Value: boolean);
    begin
      if Value
        then inherited Value := 1
        else inherited Value := 0;
    end;

{ TExpression }

  function TExpression.Evaluate: TData;
    begin
      raise ESystem.Create('Illegal expression');
    end;

  procedure TExpression.Forbitness;
    begin
      // There's nothing to forbit
    end;

  function TExpression.GetYieldType: TDataType;
    begin
      raise ESystem.Create('Unknow yield type');
    end;

  procedure TExpression.TypeCheck;
    begin
      Forbitness;
    end;

{ TSimpleData }

  constructor TSimpleData.Create(aData: TData);
    begin
      inherited Create;
      Data := aData;
    end;

  function TSimpleData.Evaluate: TData;
    begin
      Result := Data.Duplicate;
    end;

  function TSimpleData.GetYieldType: TDataType;
    begin
      Result := Data.DataType;
    end;

  procedure TSimpleData.SetData(const Value: TData);
    begin
      FData := Value;
    end;

{ TEquality }

  function TEquality.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.Equality(RData);
      LData.free;
      RData.free;
    end;

{ TInequality }

  function TInequality.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.Inequality(RData);
      LData.free;
      RData.free;
    end;

{ TLessThan }

  function TLessThan.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.LessThan(RData);
      LData.free;
      RData.free;
    end;

  procedure TLessThan.Forbitness;
    begin
      if (LeftExp.YieldType is TStructuredType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TLessThanOrEqualTo }

  function TLessThanOrEqualTo.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.LessThanOrEqualTo(RData);
      LData.free;
      RData.free;
    end;

  procedure TLessThanOrEqualTo.Forbitness;
    begin
      if (LeftExp.YieldType is TStructuredType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TGreaterThan }

  function TGreaterThan.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.GreaterThan(RData);
      LData.free;
      RData.free;
    end;

  procedure TGreaterThan.Forbitness;
    begin
      if (LeftExp.YieldType is TStructuredType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TGreaterThanOrEqualTo }

  function TGreaterThanOrEqualTo.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.GreaterThanOrEqualTo(RData);
      LData.free;
      RData.free;
    end;

  procedure TGreaterThanOrEqualTo.Forbitness;
    begin
      if (LeftExp.YieldType is TStructuredType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TAddition }

  function TAddition.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.Addition(RData);
      LData.free;
      RData.free;
    end;

  procedure TAddition.Forbitness;
    begin
      inherited;
      if ((LeftExp.YieldType is TMatrixType) and not(RightExp.YieldType is TMatrixType)) or
         ((RightExp.YieldType is TMatrixType) and not(LeftExp.YieldType is TMatrixType))
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
      if ((LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TMatrixType)) and
         ((TMatrixType(LeftExp.YieldType).Cols <> TMatrixType(LeftExp.YieldType).Cols) or
          (TMatrixType(LeftExp.YieldType).Rows <> TMatrixType(LeftExp.YieldType).Rows))
        then raise ESystem.Create(emMatrixTypeMismatch);
    end;

  procedure TAddition.TypeCheck;
    begin
      inherited;
      if (LeftExp.YieldType is TStringType) and not (RightExp.YieldType is TStringType)
        then RightExp := TCastingToString.CreateWith(RightExp)
        else
          if not (LeftExp.YieldType is TStringType) and (RightExp.YieldType is TStringType)
            then LeftExp := TCastingToString.CreateWith(LeftExp);
    end;

{ TSubtraction }

  function TSubtraction.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.Subtraction(RData);
      LData.free;
      RData.free;
    end;

  procedure TSubtraction.Forbitness;
    begin
      inherited;
      if ((LeftExp.YieldType is TMatrixType) and not(RightExp.YieldType is TMatrixType)) or
         ((RightExp.YieldType is TMatrixType) and not(LeftExp.YieldType is TMatrixType))
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
      if (LeftExp.YieldType is TStringType) or (RightExp.YieldType is TStringType)
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
      if ((LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TMatrixType)) and
         ((TMatrixType(LeftExp.YieldType).Cols <> TMatrixType(LeftExp.YieldType).Cols) or
          (TMatrixType(LeftExp.YieldType).Rows <> TMatrixType(LeftExp.YieldType).Rows))
        then raise ESystem.Create(emMatrixTypeMismatch);
    end;

{ TBooleanOr }

  function TBooleanOr.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.BOr(RData);
      LData.free;
      RData.free;
    end;

{ TBooleanXor }

  function TBooleanXor.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.BXor(RData);
      LData.free;
      RData.free;
    end;

{ TMultiplication }

  function TMultiplication.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      if (LData is TMatrix) and (RData is TReal)
        then Result := TMatrix(LData).Dilation(RData)
        else
          if (LData is TReal) and (RData is TMatrix)
            then Result := TMatrix(RData).Dilation(LData)
            else Result := LData.Multiplication(RData);
      LData.free;
      RData.free;
    end;

  procedure TMultiplication.Forbitness;
    begin
      inherited;
      if ((LeftExp.YieldType is TMatrixType) and not((RightExp.YieldType is TSimpleType) or (RightExp.YieldType is TMatrixType))) or
         ((RightExp.YieldType is TMatrixType) and not((LeftExp.YieldType is TSimpleType) or (LeftExp.YieldType is TMatrixType)))
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
      if ((LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TMatrixType)) and
         (TMatrixType(LeftExp.YieldType).Cols <> TMatrixType(RightExp.YieldType).Rows)
        then raise ESystem.Create(emMatrixTypeMismatch);
      if (LeftExp.YieldType is TStringType) or (RightExp.YieldType is TStringType)
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
    end;

  function TMultiplication.GetYieldType: TDataType;
    begin
      if (LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TSimpleType)
        then Result := LeftExp.YieldType
        else
          if (RightExp.YieldType is TMatrixType) and (LeftExp.YieldType is TSimpleType)
            then Result := RightExp.YieldType
            else
              if (LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TMatrixType)
                then Result := TMatrixType.CreateWith(TMatrixType(LeftExp.YieldType).Rows,
                               TMatrixType(RightExp.YieldType).Cols)
                else Result := inherited GetYieldType;
    end;

  procedure TMultiplication.TypeCheck;
    begin
      inherited;
      if (LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TIntegerType)
        then RightExp := TCastingToReal.CreateWith(RightExp)
        else
          if (RightExp.YieldType is TMatrixType) and (LeftExp.YieldType is TIntegerType)
            then LeftExp := TCastingToReal.CreateWith(LeftExp);
    end;

{ TDivision }

  function TDivision.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      if (LData is TMatrix) and (RData is TReal)
        then Result := TMatrix(LData).Contraction(RData)
        else
          if (LData is TReal) and (RData is TMatrix)
            then Result := TMatrix(RData).Contraction(LData)
            else Result := LData.Division(RData);
      LData.free;
      RData.free;
    end;

  procedure TDivision.Forbitness;
    begin
      inherited;
      if (LeftExp.YieldType is TStringType) or (RightExp.YieldType is TStringType)
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
      if ((LeftExp.YieldType is TMatrixType) and not(RightExp.YieldType is TSimpleType)) or
         ((LeftExp.YieldType is TSimpleType) and (RightExp.YieldType is TMatrixType))
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TDivision.GetYieldType: TDataType;
    begin
      if (LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TSimpleType)
        then Result := LeftExp.YieldType
        else
          if (RightExp.YieldType is TMatrixType) and (LeftExp.YieldType is TSimpleType)
            then Result := RightExp.YieldType
            else Result := RealType;
    end;

  procedure TDivision.TypeCheck;
    begin
      inherited;
      if (LeftExp.YieldType is TMatrixType) and (RightExp.YieldType is TIntegerType)
        then RightExp := TCastingToReal.CreateWith(RightExp)
        else
          if (RightExp.YieldType is TMatrixType) and (LeftExp.YieldType is TIntegerType)
            then LeftExp := TCastingToReal.CreateWith(LeftExp);
    end;

{ TRemainder }

  function TRemainder.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.Remainder(RData);
      LData.free;
      RData.free;
    end;

  procedure TRemainder.Forbitness;
    begin
      if not(LeftExp.YieldType is TIntegerType) or not(RightExp.YieldType is TIntegerType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TBooleanAnd }

  function TBooleanAnd.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := LData.BAnd(RData);
      LData.free;
      RData.free;
    end;

{ TBooleanNot }

  function TBooleanNot.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := Data.BNot;
      Data.free;
    end;

  procedure TBooleanNot.TypeCheck;
    begin
      if not(Exp.YieldType is TBooleanType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TNegation }

  function TNegation.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := Data.Negation;
      Data.free;
    end;

  procedure TNegation.Forbitness;
    begin
      if (Exp.YieldType is TStringType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TUnaryExpression }

  constructor TUnaryExpression.CreateWith(anExpression: TExpression);
    begin
      inherited Create;
      Exp := anExpression;
    end;

  function TUnaryExpression.GetYieldType: TDataType;
    begin
      Result := Exp.YieldType;
    end;

  procedure TUnaryExpression.SetExp(const Value: TExpression);
    begin
      FExp := Value;
      if FExp <> nil
        then TypeCheck;
    end;

{ TBinaryExpression }

  constructor TBinaryExpression.CreateWith(aLeftExp, aRightExp: TExpression);
    begin
      inherited Create;
      LeftExp := aLeftExp;
      RightExp := aRightExp;
    end;

  function TBinaryExpression.GetYieldType: TDataType;
    begin
      if (LeftExp <> nil) and (RightExp <> nil)
        then Result := LeftExp.YieldType
        else raise ESystem.Create('Can''t perform ''YieldType'' operation');
    end;

  procedure TBinaryExpression.SetLeftExp(const Value: TExpression);
    begin
      FLeftExp := Value;
      if (FRightExp <> nil) and (FLeftExp <> nil)
        then TypeCheck;
    end;

  procedure TBinaryExpression.SetRightExp(const Value: TExpression);
    begin
      FRightExp := Value;
      if (FRightExp <> nil) and (FLeftExp <> nil)
        then TypeCheck;
    end;

  procedure TBinaryExpression.TypeCheck;
    begin
      inherited;
      if (LeftExp.YieldType is TIntegerType) and (RightExp.YieldType is TRealType)
        then LeftExp := TCastingToReal.CreateWith(LeftExp)
        else
          if (LeftExp.YieldType is TRealType) and (RightExp.YieldType is TIntegerType)
            then RightExp := TCastingToReal.CreateWith(RightExp);
    end;

{ TBlockStatement }

  constructor TBlockStatement.Create(aRunner: TRunner; const aSourceLine: integer);
    begin
      inherited;
      FStatements := TList.Create;
    end;

  destructor TBlockStatement.Destroy;
    begin
      FStatements.free;
      inherited;
    end;

  procedure TBlockStatement.Execute;
    var
      i: integer;
    begin
      i := 0;
      while (i < Statements.Count) and not ThreadIsTerminated do
        begin
          Runner.Run(TStatement(Statements.Items[i]));
          inc(i);
        end;
    end;

{ TIfStatement }

  procedure TIfStatement.Execute;
    var
      Bool: TBoolean;
    begin
      if not ThreadIsTerminated
        then
          begin
            Bool := TBoolean(Expression.Evaluate);
            if Bool.Value
              then Runner.Run(ThenStatement)
              else
                if HasElseClause
                  then Runner.Run(ElseStatement);
            Bool.free;
          end;
    end;

  function TIfStatement.GetHasElseClause: boolean;
    begin
      Result := ElseStatement <> nil;
    end;

{ TWhileStatement }

  function TWhileStatement.EvalExpression: boolean;
    var
      Bool: TBoolean;
    begin
      Bool := TBoolean(Expression.Evaluate);
      Result := Bool.Value;
      Bool.free;
    end;

  procedure TWhileStatement.Execute;
    begin
      while EvalExpression and not ThreadIsTerminated
        do Runner.Run(BlockStatement);
    end;

{ TRepeatStatement }

  function TRepeatStatement.EvalExpression: boolean;
    var
      Bool: TBoolean;
    begin
      Bool := TBoolean(Expression.Evaluate);
      Result := Bool.Value;
      Bool.free;
    end;

  procedure TRepeatStatement.Execute;
    begin
      repeat
        Runner.Run(BlockStatement);
      until EvalExpression or ThreadIsTerminated;
    end;

{ TAssignation }

  procedure TAssignation.Execute;
    var
      aData: TData;
    begin
      if not ThreadIsTerminated
        then
          begin
            aData := Expression.Evaluate;
            Data.Assign(aData);
            aData.free;
          end;
    end;

{ TSymbolSystem }

  procedure TSymbolSystem.Clear;
    begin
      Constants.Clear;
      Variables.Clear;
      Types.Clear;
      Procedures.Clear;
      Functions.Clear;
    end;

  constructor TSymbolSystem.Create(aBlockOwner: TBlock);
    begin
      inherited Create;
      FBlockOwner := aBlockOwner;
      Constants  := TSymbolTable.Create;
      Variables  := TSymbolTable.Create;
      Types      := TSymbolTable.Create;
      Procedures := TSymbolTable.Create;
      Functions  := TSymbolTable.Create;
    end;

  destructor TSymbolSystem.Destroy;
    begin
      Constants.free;
      Variables.free;
      Types.free;
      Procedures.free;
      Functions.free;
    end;

  function TSymbolSystem.HasIdentifier(const Named: string): boolean;
    begin
      Result := Constants.HasIdentifier(Named) or
                Variables.HasIdentifier(Named) or
                Types.HasIdentifier(Named) or
                Procedures.HasIdentifier(Named) or
                Functions.HasIdentifier(Named);
    end;

  function TSymbolSystem.HasVariable(const Named: string): boolean;
    var
      Entry: TSymbolTableEntry;
    begin
      Result := Search(stVariable, Named, Entry);
    end;

  function TSymbolSystem.Search(const What: TSymbolType; const Named: string; var Entry: TSymbolTableEntry): boolean;
    begin
      case What of
        stConstant:  Result := Constants.FindEntry(Named, Entry);
        stVariable:  Result := Variables.FindEntry(Named, Entry);
        stType:      Result := Types.FindEntry(Named, Entry);
        stProcedure: Result := Procedures.FindEntry(Named, Entry);
        stFunction:  Result := Functions.FindEntry(Named, Entry);
        else         Result := false;
      end;
      if not Result and (BlockOwner.Owner <> nil)
        then Result := BlockOwner.Owner.SymbolSystem.Search(What, Named, Entry);
    end;

{ TBlock }

  constructor TBlock.Create;
    begin
      inherited;
      FSymbolSystem := TSymbolSystem.Create(Self);
    end;

  destructor TBlock.Destroy;
    begin
      FSymbolSystem.free;
      inherited;
    end;

  function TBlock.GetConstants: TSymbolTable;
    begin
      Result := SymbolSystem.Constants;
    end;

  function TBlock.GetFunctions: TSymbolTable;
    begin
      Result := SymbolSystem.Functions;
    end;

  function TBlock.GetProcedures: TSymbolTable;
    begin
      Result := SymbolSystem.Procedures;
    end;

  function TBlock.GetTypes: TSymbolTable;
    begin
      Result := SymbolSystem.Types;
    end;

  function TBlock.GetVariables: TSymbolTable;
    begin
      Result := SymbolSystem.Variables;
    end;

{ TForStatement }

  procedure TForStatement.Execute;
    var
      FinalValue: TInteger;
    begin
      Counter.Value := TInteger(InitialExp.Evaluate).Value;
      FinalValue    := TInteger(FinalExp.Evaluate);
      while (Counter.Value <= FinalValue.Value) and not ThreadIsTerminated do
        begin
          if Statement <> nil
            then Runner.Run(Statement);
          if LoopDirection = ldTo
            then Counter.Value := Succ(Counter.Value)
            else Counter.Value := Pred(Counter.Value);
        end;
      FinalValue.free;
    end;

{ TOutputStatement }

  procedure TOutputStatement.Execute;
    var
      Data: TData;
      Str: TString;
    begin
      if Assigned(FOutput) and not ThreadIsTerminated
        then
          begin
            Data := Expression.Evaluate;
            Str := Data.CastToString;
            OutPut(Str.Value);
            Data.free;
            Str.free;
          end;
    end;

{ TProcedureCallStatement }

  procedure TProcedureCallStatement.Execute;
    var
      Stack: TList;
    begin
      if not ThreadIsTerminated
        then
          begin
            Stack := TList.Create;
            PushParams(Stack);
            ParameterPassing;
            Routine.Execute;
            PopParams(Stack);
            Stack.free;
          end;
    end;

  procedure TProcedureCallStatement.ParameterPassing;
    var
      i: integer;
      Data: TData;
    begin
      for i := 0 to pred(Parameters.Count) do
        begin
          Data := TExpression(Parameters.Items[i]).Evaluate;
          TData(Routine.Parameter[i]).Assign(Data);
          Data.free;
        end;
    end;

  procedure TProcedureCallStatement.PopParams(Stack: TList);
    var
      i: integer;
    begin
      for i := 0 to pred(Routine.Parameters.Count) do
        begin
          if Routine.Variables.HasIdentifier(Routine.Parameter[i].Name)
            then TSimpleData(Parameters.Items[i]).Data.Assign(Routine.Parameter[i]);
          TData(Routine.Parameters.Items[i]).Assign(TData(Stack.Items[i]));
          TData(Stack.Items[i]).free;
        end;
    end;

  procedure TProcedureCallStatement.PushParams(Stack: TList);
    var
      i: integer;
    begin
      for i := 0 to pred(Routine.Parameters.Count) do
        Stack.Add(Routine.Parameter[i].Duplicate);
    end;

{ TFunctionEvaluation }

  function TFunctionEvaluation.Evaluate: TData;
    begin
      FunctionCall.Execute;
      Result := FunctionCall.Routine.Result.Duplicate;
    end;

  function TFunctionEvaluation.GetYieldType: TDataType;
    begin
      Result := FunctionCall.Routine.Result.DataType;
    end;

{ TFunction }

  procedure TFunction.SetResult(const Value: TData);
    begin
      FResult := Value;
      FResult.Name := 'Result';
      Variables.AddEntry(FResult);
    end;

{ TFunctionCallStatement }

  function TFunctionCallStatement.GetRoutine: TFunction;
    begin
      Result := TFunction(inherited Routine);
    end;

  procedure TFunctionCallStatement.SetRoutine(const Value: TFunction);
    begin
      inherited Routine := TProcedure(Value);
    end;

{ TSingleMathFunction }

  constructor TSingleMathFunction.CreateIdent(const aName: string);
    var
      Data: TData;
    begin
      inherited Create;
      Name := aName;
      Result := RealType.Instantiate;
      Data :=  RealType.Instantiate;
      Data.Name := 'X';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

{ TBinaryMathFunction }

  constructor TBinaryMathFunction.CreateIdent(const aName: string);
    var
      Data: TData;
    begin
      inherited Create;
      Name := aName;
      Result := RealType.Instantiate;
      Data :=  RealType.Instantiate;
      Data.Name := 'X1';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
      Data :=  RealType.Instantiate;
      Data.Name := 'X2';
      Constants.AddEntry(Data);
      Parameters.Add(Data);
    end;

  { TCastingToReal }

  function TCastingToReal.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := Data.CastToReal;
      Data.free;
    end;

  procedure TCastingToReal.Forbitness;
    begin
      if Exp.YieldType is TStringType
        then raise ESystem.Create(emInvalidTypecast);
    end;

  function TCastingToReal.GetYieldType: TDataType;
    begin
      Result := RealType;
    end;

{ TCastingToString }

  function TCastingToString.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := Data.CastToString;
      Data.free;
    end;

  function TCastingToString.GetYieldType: TDataType;
    begin
      Result := StringType;
    end;

{ TRelation }

  function TRelation.GetYieldType: TDataType;
    begin
      Result := BooleanType;
    end;

{ TBinArithmetic }

  procedure TBinArithmetic.Forbitness;
    begin
      if ((LeftExp.YieldType is TStructuredType) and not(LeftExp.YieldType is TMatrixType)) or
         ((RightExp.YieldType is TStructuredType) and not(RightExp.YieldType is TMatrixType))
        then IncompatibleTypesError(LeftExp.YieldType.Name, RightExp.YieldType.Name);
    end;

{ TBinBoolean }

  procedure TBinBoolean.Forbitness;
    begin
      if not(LeftExp.YieldType is TBooleanType) or not(RightExp.YieldType is TBooleanType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

{ TOrdinal }

  constructor TOrdinal.CreateFrom(aDataType: TDataType);
    begin
      inherited;
      FCanBeAssigned := true;
    end;

  function TOrdinal.High: integer;
    begin
      Result := TOrdinalType(DataType).High;
    end;

  function TOrdinal.Length: integer;
    begin
      Result := TOrdinalType(DataType).Length;
    end;

  function TOrdinal.Low: integer;
    begin
      Result := TOrdinalType(DataType).Low;
    end;

{ TOrdinalType }

  function TOrdinalType.Length: integer;
    begin
      Result := succ(High - Low);
    end;


{ TArrayIndex }

  constructor TArrayIndex.Create(anArray: TData; anExpression: TExpression);
    begin
      if not (anArray.DataType is TArrayType)
        then raise ESystem.Create(emArrayTypeRequired);
      inherited CreateFrom(TArrayType(anArray.DataType).BaseType);
      FArray_ := anArray;
      FExpression := anExpression;
    end;

  destructor TArrayIndex.Destroy;
    begin
      FExpression.free;
      inherited;
    end;

  function TArrayIndex.GetValue: TData;
    var
      Data: TData;
    begin
      Data := Expression.Evaluate;
      if Array_ is TArray
        then Result := TArray(Array_).Value(TInteger(Data).Value)
        else Result := TArray(TFloatingData(Array_).Value).Value(TInteger(Data).Value);
      Data.free;
    end;

{ TFloatingField }

  constructor TFloatingField.Create(aRecord: TFloatingData; const aFieldId: integer);
    begin
      inherited CreateFrom(TData(TRecordType(aRecord.DataType).FieldList.items[aFieldId]).DataType);
      FRecord := aRecord;
      FFieldId := aFieldId;
    end;

  function TFloatingField.GetValue: TData;
    begin
      Result := TRecord(Record_.Value).Field[FieldId];
    end;

{ TFloatingData }

  procedure TFloatingData.Assign(Data: TData);
    begin
      Value.Assign(Data);
    end;

  function TFloatingData.CastToString: TString;
    begin
      Result := Value.CastToString;
    end;

  function TFloatingData.Duplicate: TData;
    begin
      Result := Value.Duplicate;
    end;

{ TStructuredType }

  function TStructuredType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := AssignType = Self;
    end;

{ TMatrixType }

  function TMatrixType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := (AssignType is TMatrixType) and
                (TMatrixType(AssignType).Cols = Cols) and
                (TMatrixType(AssignType).Rows = Rows);
    end;

  constructor TMatrixType.CreateWith(const RowsCount, ColsCount: integer);
    begin
      inherited CreateWith(TSubrangeType.Create(1, RowsCount), TProtoVectorType.CreateWith(ColsCount));
      FRows := RowsCount;
      FCols := ColsCount;
      Name := psMatrix;
    end;

  destructor TMatrixType.Destroy;
    begin
      FBaseType.free;
      inherited;
    end;

  function TMatrixType.Instantiate: TData;
    begin
      Result := TMatrix.CreateFrom(Self);
    end;

{ TInvertion }

  function TInvertion.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := TMatrix(Data).Invertion;
      Data.free;
    end;

  procedure TInvertion.Forbitness;
    begin
      if not(Exp.YieldType is TMatrixType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
      if TMatrixType(Exp.YieldType).Rows <> TMatrixType(Exp.YieldType).Cols
        then raise ESystem.Create(emSquareMatrixRequired);
    end;

{ TVectorType }

  constructor TRowVectorType.CreateWith(const ElementsCount: integer);
    begin
      inherited CreateWith(1, ElementsCount);
      Name := psRowVector;
    end;

  function TRowVectorType.Instantiate: TData;
    begin
      Result := TRowVector.CreateFrom(Self);
    end;

{ TInternalMult }

  function TInternalMult.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := TMatrix(LData).InternalMult(RData);
      LData.free;
      RData.free;
    end;

  procedure TInternalMult.Forbitness;
    begin
      if not(LeftExp.YieldType is TMatrixType) or not(RightExp.YieldType is TMatrixType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
      if ((TMatrixType(LeftExp.YieldType).Cols <> TMatrixType(LeftExp.YieldType).Cols) or
          (TMatrixType(LeftExp.YieldType).Rows <> TMatrixType(LeftExp.YieldType).Rows))
        then raise ESystem.Create(emMatrixTypeMismatch);
    end;

{ TInternalDiv }

  function TInternalDiv.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      Result := TMatrix(LData).InternalDiv(RData);
      LData.free;
      RData.free;
    end;

  procedure TInternalDiv.Forbitness;
    begin
      if not(LeftExp.YieldType is TMatrixType) or not(RightExp.YieldType is TMatrixType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
      if ((TMatrixType(LeftExp.YieldType).Cols <> TMatrixType(LeftExp.YieldType).Cols) or
          (TMatrixType(LeftExp.YieldType).Rows <> TMatrixType(LeftExp.YieldType).Rows))
        then raise ESystem.Create(emMatrixTypeMismatch);
    end;

{ TTransposition }

  function TTransposition.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := TMatrix(Data).Transposition;
      Data.free;
    end;

  procedure TTransposition.Forbitness;
    begin
      if not(Exp.YieldType is TMatrixType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TTransposition.GetYieldType: TDataType;
    begin
      Result := TMatrixType.CreateWith(TMatrixType(Exp.YieldType).Cols,
                TMatrixType(Exp.YieldType).Rows);
    end;

{ TRowVector }

  function TRowVector.GetElement(const i: integer): TReal;
    begin
      Result := inherited Element[1, i];
    end;

{ TDeterminant }

  function TDeterminant.Evaluate: TData;
    var
      Data: TData;
    begin
      Data := Exp.Evaluate;
      Result := TMatrix(Data).Determinant;
      Data.free;
    end;

  procedure TDeterminant.Forbitness;
    begin
      if not(Exp.YieldType is TMatrixType)
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
      if TMatrixType(Exp.YieldType).Rows <> TMatrixType(Exp.YieldType).Cols
        then raise ESystem.Create(emSquareMatrixRequired);
    end;

  function TDeterminant.GetYieldType: TDataType;
    begin
      Result := RealType;
    end;

{ TMatrix }

  function TMatrix.Addition(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].Addition(TMatrix(Data).Row[i]));
    end;

  function TMatrix.Contraction(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].Contraction(Data));
    end;

  function TMatrix.Determinant: TData;
    var
      Matrix: TMatrix;
      i: integer;
    begin
      if TMatrixType(DataType).Rows <> TMatrixType(DataType).Cols
        then raise ESystem.Create(emSquareMatrixRequired);
      Matrix := TMatrix(RungDown);
      Result := RealType.Instantiate;
      TReal(Result).Value := 1;
      for i := 1 to TMatrixType(DataType).Cols do
        TReal(Result).Value := TReal(Result).Value * Matrix.Element[i, i].Value;
      Matrix.free;
    end;

  function TMatrix.Dilation(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].Dilation(Data));
    end;

  function TMatrix.DoRungDown(const BreakOnSingular, Normalize: boolean): boolean;
    var
      i: integer;
    begin
      i := 1;
      Result := true;
      while (i <= TMatrixType(DataType).Rows) and Result do
        begin
          Result := StepDown(i, Normalize) or not BreakOnSingular;
          inc(i);
        end;
    end;

  function TMatrix.DoRungUp(const BreakOnSingular, Normalize: boolean): boolean;
    var
      i: integer;
    begin
      i := TMatrixType(DataType).Rows;
      Result := true;
      while (i >= 1) and Result do
        begin
          Result := StepUp(i, Normalize) or not BreakOnSingular;
          dec(i);
        end;
    end;

  function TMatrix.GetElement(const aRow, aCol: integer): TReal;
    begin
      Result := TReal(Row[aRow].Element[aCol]);
    end;

  function TMatrix.GetRank: integer;
    var
      Matrix: TMatrix;
    begin
      if TMatrixType(DataType).Rows <> TMatrixType(DataType).Cols
        then raise ESystem.Create(emSquareMatrixRequired);
      Matrix := TMatrix(Duplicate);
      Result := 1;
      while (Result <= TMatrixType(DataType).Rows) and Matrix.StepDown(Result, false)
        do inc(Result);
      dec(Result);
    end;

  function TMatrix.GetRow(const i: integer): TProtoVector;
    begin
      Result := TProtoVector(inherited Element[i]);
    end;

  function TMatrix.GetSingular: boolean;
    var
      Matrix: TMatrix;
    begin
      Matrix := TMatrix(Duplicate);
      Result := Matrix.DoRungDown(true, false);
      Matrix.free;
    end;

  function TMatrix.InternalDiv(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].InternalDiv(TMatrix(Data).Row[i]));
    end;

  function TMatrix.InternalMult(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].InternalMult(TMatrix(Data).Row[i]));
    end;

  function TMatrix.Invertion: TData;
    var
      i, j: integer;
      Data: TData;
    begin
      if TMatrixType(DataType).Rows <> TMatrixType(DataType).Cols
        then raise ESystem.Create(emSquareMatrixRequired);
      Data := TMatrixType.CreateWith(TMatrixType(DataType).Rows, 2 * TMatrixType(DataType).Cols).Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        begin
          TMatrix(Data).Element[i, TMatrixType(DataType).Rows + i].Value := 1;
          for j := 1 to TMatrixType(DataType).Cols do
            TMatrix(Data).Element[i, j].Value := Element[i, j].Value;
        end;
      if TMatrix(Data).DoRungDown(true, true)
        then TMatrix(Data).DoRungUp(true, true)
        else raise ESystem.Create(emMatrixMustBeNoSingular);
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        for j := 1 to TMatrixType(DataType).Cols do
          TMatrix(Result).Element[i, j].Value := TMatrix(Data).Element[i, TMatrixType(DataType).Rows + j].Value;
    end;

  function TMatrix.Multiplication(Data: TData): TData;
    var
      i, j, k: integer;
      Value: real;
    begin
      Result := TMatrixType.CreateWith(TMatrixType(DataType).Rows,
                TMatrixType(Data.DataType).Cols).Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        for j := 1 to TMatrixType(Data.DataType).Cols do
          begin
            Value := 0;
            for k := 1 to TMatrixType(DataType).Cols do
              Value := Value + Element[i, k].Value * TMatrix(Data).Element[k, j].Value;
            TMatrix(Result).Element[i, j].Value := Value;
          end;
    end;

  function TMatrix.Negation: TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].Negation);
    end;

  function TMatrix.Rank: TData;
    begin
      Result := RealType.Instantiate;
      TReal(Result).Value := GetRank;
    end;

  function TMatrix.RungDown: TData;
    begin
      Result := Duplicate;
      TMatrix(Result).DoRungDown(false, false);
    end;

  function TMatrix.RungUp: TData;
    begin
      Result := Duplicate;
      TMatrix(Result).DoRungUp(false, false);
    end;

  procedure TMatrix.SetRow(const i: integer; const Value: TProtoVector);
    begin
      inherited Element[i] := Value;
    end;

  function TMatrix.StepDown(const Col: integer; const Normalize: boolean): boolean;
    var
      r, i: integer;
      Factor: TData;
      Pivot: TData;
      BaseRow: TData;
    begin
      if Col <= TMatrixType(DataType).Rows
        then
          begin
            r := Col;
            while (r < TMatrixType(DataType).Rows) and (Element[r, Col].Value = 0)
              do inc(r);
            if Element[r, Col].Value <> 0
              then
                begin
                  if r <> Col
                    then SwapRows(r, Col);
                  Result := true;
                  Factor := TReal.Create;
                  TReal(Factor).Value := TReal(Element[Col, Col]).Value;
                  BaseRow := Row[Col].Contraction(Factor);
                  for i := succ(Col) to TMatrixType(DataType).Rows do
                    begin
                      TReal(Factor).Value := TReal(Element[i, Col]).Value;
                      Pivot := TProtoVector(BaseRow).Dilation(Factor);
                      Row[i].Assign(Row[i].Subtraction(Pivot));
                      Pivot.free;
                    end;
                  if Normalize
                    then Row[Col].Assign(BaseRow);
                  Factor.free;
                  BaseRow.free;
                end
              else Result := false
          end
        else Result := TReal(Element[Col, Col]).Value <> 0;
    end;

  function TMatrix.StepUp(const Col: integer; const Normalize: boolean): boolean;
    var
      r, i: integer;
      Factor: TData;
      Pivot: TData;
      BaseRow: TData;
    begin
      if Col <= TMatrixType(DataType).Rows
        then
          begin
            r := Col;
            while (Element[r, Col].Value = 0) and (r > 1)
              do dec(r);
            if Element[r, Col].Value <> 0
              then
                begin
                  if r <> Col
                    then SwapRows(r, Col);
                  Result := true;
                  Factor := TReal.Create;
                  TReal(Factor).Value := TReal(Element[Col, Col]).Value;
                  BaseRow := Row[Col].Contraction(Factor);
                  for i := pred(Col) downto 1 do
                    begin
                      TReal(Factor).Value := TReal(Element[i, Col]).Value;
                      Pivot := TProtoVector(BaseRow).Dilation(Factor);
                      Row[i].Assign(Row[i].Subtraction(Pivot));
                      Pivot.free;
                    end;
                  if Normalize
                    then Row[Col].Assign(BaseRow);
                  Factor.free;
                  BaseRow.free;
                end
              else Result := false
          end
        else Result := TReal(Element[Col, Col]).Value <> 0;
    end;

  function TMatrix.Subtraction(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        TMatrix(Result).Row[i].Assign(Row[i].Subtraction(TMatrix(Data).Row[i]));
    end;

  procedure TMatrix.SwapRows(const r1, r2: integer);
    var
      Vector: TProtoVector;
    begin
      Vector := Row[r1];
      Row[r1] := Row[r2];
      Row[r2] := Vector;
    end;

  function TMatrix.Transposition: TData;
    var
      i, j: integer;
    begin
      Result := TMatrixType.CreateWith(TMatrixType(DataType).Cols,
                TMatrixType(DataType).Rows).Instantiate;
      for i := 1 to TMatrixType(DataType).Rows do
        for j := 1 to TMatrixType(DataType).Cols do
          TMatrix(Result).Element[j, i].Value := Element[i, j].Value;
    end;

{ TProtoVector }

  constructor TProtoVectorType.CreateWith(const ElementsCount: integer);
    begin
      inherited CreateWith(TSubrangeType.Create(1, ElementsCount), RealType);
      FElements := ElementsCount;
    end;

{ TProtoVector }

  function TProtoVector.Addition(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value + TProtoVector(Data).Element[i].Value;
    end;

  function TProtoVector.Contraction(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value / TReal(Data).Value;
    end;

  function TProtoVector.Dilation(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value * TReal(Data).Value;
    end;

  function TProtoVector.GetElement(const Index: integer): TReal;
    begin
      Result := TReal(inherited Element[Index]);
    end;

  function TProtoVector.InternalDiv(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value / TProtoVector(Data).Element[i].Value;
    end;

  function TProtoVector.InternalMult(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value * TProtoVector(Data).Element[i].Value;
    end;

  function TProtoVector.Negation: TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := - Element[i].Value;
    end;

  function TProtoVector.Subtraction(Data: TData): TData;
    var
      i: integer;
    begin
      Result := DataType.Instantiate;
      for i := 1 to TProtoVectorType(DataType).Elements do
        TProtoVector(Result).Element[i].Value := Element[i].Value - TProtoVector(Data).Element[i].Value;
    end;

  function TProtoVectorType.Instantiate: TData;
    begin
      Result := TProtoVector.CreateFrom(Self);
    end;

{ TColVectorType }

  constructor TColVectorType.CreateWith(const ElementsCount: integer);
    begin
      inherited CreateWith(ElementsCount, 1);
      Name := psColVector;
   end;

  function TColVectorType.Instantiate: TData;
    begin
      Result := TColVector.CreateFrom(Self);
    end;

{ TColVector }

  function TColVector.GetElement(const i: integer): TReal;
    begin
      Result := inherited Element[i, 1];
    end;

  procedure TModule.SetFileName(const Value: TFileName);
    begin
      FFileName := Value;
    end;

{ TRunner }

  procedure TRunner.Run(Statement: TStatement);

   function DoBreaking: boolean;
     begin
       Result := Breaking and (Statement.SourceLine > 0) and
              not ((Statement is TBlockStatement)
              and (LastStatement <> nil)
              and ((LastStatement is TRepeatStatement)
              or (LastStatement is TWhileStatement)))
     end;

    begin
      if not ThreadIsTerminated
        then
          begin
            //if FLineBreak = Statement.SourceLine
            //  then Breaking := true;
            if DoBreaking or Statement.HasBreakPoint
              then
                begin
                  FOnBreakingExecution(Statement.SourceLine);
                  RunInstance.Suspended := true;
                  FOnResumeExecution(Statement.SourceLine);
                end;
            LastStatement := Statement;
            if FStepOver and (Statement is TProcedureCallStatement)
              then
                begin
                  Breaking := false;
                  Statement.Execute;
                  Breaking := true;
                end
              else Statement.Execute;
          end;
    end;

  procedure TRunner.RunApplication;
    begin
      //FStepOver := false;
      FLineBreak := 0;
      if not Runing
        then
          begin
            Runing := true;
            if StartPoint <> nil
              then
                with RunInstance do
                  begin
                    Defer(ManageRun, []);
                    UnTerminate;
                  end;
          end;
      RunInstance.Suspended := false;
    end;

  procedure TRunner.RunToLine(const Line: integer);
    begin
      FLineBreak := Line;
      FStepOver := false;
      Breaking := false;
      RunInstance.Suspended := false;
    end;

  procedure TRunner.SetStartPoint(const Value: TStatement);
    begin
      FStartPoint := Value;
    end;

  procedure TRunner.Pause;
    begin
      RunInstance.Suspended := true;
    end;

  procedure TRunner.Reset;
    begin
      RunInstance.Suspended := false;
      RunInstance.Terminate;
      Runing := false;
    end;

  procedure TRunner.StepOver;
    begin
      Breaking := true;
      FStepOver := true;
      FLineBreak := 0;
    end;

  procedure TRunner.TraceInto;
    begin
      Breaking := true;
      FStepOver := false;
      FLineBreak := 0;
      RunInstance.Suspended := false;
    end;

  constructor TRunner.Create(AnOnBreakingExecution, AnOnResumeExecution: TBreakingExecution);
    begin
      inherited Create;
      FOnBreakingExecution := AnOnBreakingExecution;
      FOnResumeExecution := AnOnResumeExecution;
      RunInstance := TExclusiveThread.Create(priNormal);
    end;

  destructor TRunner.Destroy;
    begin
      if Runing
        then RunInstance.Terminate;
      RunInstance.free;
      inherited;
    end;

  procedure TRunner.ManageRun(const params: array of const);
    begin
      Run(StartPoint);
      RunInstance.Terminate;
      Runing := false;
      if Assigned(fOnTerminateEvent)
        then fOnTerminateEvent(Self);
    end;

{ TStatement }

  constructor TStatement.Create(aRunner: TRunner; const aSourceLine: integer);
    begin
      inherited Create;
      FRunner := aRunner;
      SourceLine := aSourceLine;
    end;

  procedure TStatement.SetHasBreakpoint(const Value: boolean);
    begin
      FHasBreakpoint := Value;
    end;

{ TPower }

  function TPower.Evaluate: TData;
    var
      LData: TData;
      RData: TData;
    begin
      LData := LeftExp.Evaluate;
      RData := RightExp.Evaluate;
      if LData.DataType is TIntegerType
        then Result := TInteger(LData).Power(RData)
        else Result := TReal(LData).Power(RData);
      LData.free;
      RData.free;
    end;

  procedure TPower.Forbitness;
    begin
      if not(((LeftExp.YieldType is TIntegerType) or (LeftExp.YieldType is TRealType)) and
             ((RightExp.YieldType is TIntegerType) or (RightExp.YieldType is TRealType)))
        then raise ESystem.Create(emOperatorNotApplicableToThisOperandType);
    end;

  function TPower.GetYieldType: TDataType;
    begin
      if (LeftExp.YieldType is TIntegerType) and (RightExp.YieldType is TIntegerType)
        then Result := IntegerType
        else Result := RealType;
    end;

  procedure TPower.TypeCheck;
    begin
      if (LeftExp.YieldType is TIntegerType) and (RightExp.YieldType is TRealType)
        then LeftExp := TCastingToReal.CreateWith(LeftExp)
        else
          if (LeftExp.YieldType is TRealType) and (RightExp.YieldType is TIntegerType)
            then RightExp := TCastingToReal.CreateWith(RightExp);
    end;

{ TFloppyParamType }

  function TFloppyParamType.Assignable(AssignType: TDataType): boolean;
    begin
      Result := (BaseClass = nil) or (AssignType is BaseClass);
    end;

  constructor TFloppyParamType.Create(aBaseClass: CDataType);
    begin
      inherited Create;
      FBaseClass := aBaseClass;
    end;

  function TFloppyParamType.Instantiate: TData;
    begin
      Result := TFloppyParam.CreateFrom(Self);
    end;

{ TFloppyParam }

  procedure TFloppyParam.Assign(Data: TData);
    begin
      if Data.DataType.ClassType <> TFloppyParamType
        then
          begin
            FParameter.free;
            FParameter := Data.DataType.Instantiate;
            FParameter.Assign(Data);
          end;
    end;

  function TFloppyParam.Duplicate: TData;
    begin
      Result := DataType.Instantiate;
      {
      if FParameter <> nil
        then TFloppyParam(Result).FParameter := FParameter.Duplicate;
      }
    end;

initialization

    SystemBlock := TBlock.Create;
    IntegerType := TIntegerType.CreateIdent(psInteger);
    RealType    := TRealType.CreateIdent(psReal);
    StringType  := TStringType.CreateIdent(psString);
    BooleanType := TBooleanType.CreateIdent(psBoolean);
    TrueIdent   := TEnumerated.CreateIdent(psTrue);
    TrueIdent.DataType := BooleanType;
    FalseIdent  := TEnumerated.CreateIdent(psFalse);
    FalseIdent.DataType := BooleanType;
    BooleanType.AddIdentifier(FalseIdent);
    BooleanType.AddIdentifier(TrueIdent);
    with SystemBlock do
      begin
        Name := 'GlobalBlock';
        Types.AddEntry(IntegerType);
        Types.AddEntry(RealType);
        Types.AddEntry(StringType);
        Types.AddEntry(BooleanType);
        Constants.AddEntry(TrueIdent);
        Constants.AddEntry(FalseIdent);
      end;
    MainHandler := TMainHandler.Create;

  finalization

    SystemBlock.free;
    MainHandler.free;

end.
