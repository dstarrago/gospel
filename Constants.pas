
{*******************************************************}
{                                                       }
{       Gospel Compiler System                          }
{                                                       }
{       Copyright (C) 1999 Voltus Corporation           }
{                                                       }
{*******************************************************}

unit Constants;

interface

{*******************************************************}
{                                                       }
{                    Error Message                      }
{                                                       }
{*******************************************************}

resourcestring

  emSemicolonNotAllowedBeforeELSE = ''';'' not allowed before ''ELSE''';
  emClause1ExpectedButClause2Found = '''%s'' clause expected, but ''%s'' found';  // todavia
  emNameIsNotTypeIdentifier = '''%s'' is not a type identifier';
  emToken1ExpectedButToken2Found = '''%s'' expected but ''%s'' found';
  emElementExpectedButToken2Found = '%s expected but ''%s'' found';
  emArrayTypeRequired = 'Array type required';
  emAssignmentToFORLoopVariable = 'Assignment to FOR-Loop variable ''%s''';
  emCannotUseReservedUnitName = 'Cannot use reserved unit name ''%s''';  // todavia
  emCaseLabelOutsideOfRangeOfCaseExpression = 'Case label outside of range of case expression';  // todavia
  emCircularUnitReferenceTo = 'Circular unit reference to ''%s''';  //  todavia
  emCompileTerminatedByUser = 'Compile terminated by user';  //  todavia
  emConstantExpressionExpected = 'Constant expression expected';       // ver esto
  emConstantExpressionViolatesSubrangeBounds = 'Constant expression violates subrange bounds'; // y esto
  emConstantObjectCannotBePassedAsVarParameter = 'Constant object cannot be passed as var parameter';
  emConstantOrTypeIdentifierExpected = 'Constant or type identifier expected';
  emCouldNotCompileUsedUnit = 'Could not compile used unit ''%s''';  //  todavia
  emDeclarationOfNameDiffersFromPreviousDeclaration = 'Declaration of ''%s'' differs from previous declaration';  //  todavia
  emExpressionHasNoValue = 'Expression has no value';  // todavia ( y ver)
  emExpressionTooComplicated = 'Expression too complicated';  // todavia ( y ver)
  emFileNotFound = 'File not found: ''%s''';  //  todavia
  emFileTypeNotAllowedHere = 'File type not allowed here';  //  todavia
  emForLoopControlVariableMustBeSimpleLocalVariable = 'For loop control variable must be simple local variable';
  emForLoopControlVariableMustHaveOrdinalType = 'For loop control variable must have ordinal type';
  emFOROrWHILELoopExecutesZeroTimes = 'FOR or WHILE loop executes zero times - deleted';  // Esto es un warning
  emFORLoopVariableCannotBePassedAsVarParameter = 'FOR-Loop variable ''%s'' cannot be passed as var parameter';
  emFunctionNeedsResultType = 'Function needs result type';
  emIdentifierRedeclared = 'Identifier redeclared: ''%s''';
  emIllegalCharacterInInputFile = 'Illegal character in input file: ''%s''';  // ya veremos donde va
  emIncompatibleTypes = 'Incompatible types';  //  todavia (hay que ver donde va)
  emIncompatibleTypesName1AndName2 = 'Incompatible types: ''%s'' and ''%s''';
  emIntegerConstantTooLarge = 'Integer constant too large';  //  todavia
  emInternalError = 'Internal error: %d';  //  todavia
  emInvalidFunctionResultType = 'Invalid function result type';  //  todavia (faltan los files)
  emInvalidTypecast = 'Invalid typecast';  //  todavia (faltan los typecast)
  emInvalidVectorIndexed = 'Invalid vector indexed';
  emLeftSideCannotBeAssignedTo = 'Left side cannot be assigned to';
  emLineTooLong = 'Line too long (more than 255 characters)';  //  todavia
  emLowBoundExceedsHighBound = 'Low bound exceeds high bound';
  emMatrixTypeMismatch = 'Matrix type mismatch';
  emMatrixMustBeNoSingular = 'Matrix must be no singular';
  emMissingOperatorOrSemicolon = 'Missing operator or semicolon';
  emMissingParameterType = 'Missing parameter type';
  emNotEnoughActualParameters = 'Not enough actual parameters';
  emNumberOfElementsDiffersFromDeclaration = 'Number of elements differs from declaration';  //  todavia (no existe inicializacion de variables o constantes con tipo)
  emOperatorNotApplicableToThisOperandType = 'Operator not applicable to this operand type';
  emOrdinalTypeRequired = 'Ordinal type required';
  emProcedureCannotHaveResultType = 'Procedure cannot have a result type';
  emProgramOrUnitRecursivelyUsesItself = 'Program or unit ''%s'' recursively uses itself';  //  todavia
  emRecordTypeRequired = 'Record type required';
  emReturnValueOfFunctionMightBeUndefined = 'Return value of function ''%s'' might be undefined';  //  todavia
  emStatementExpectedButExpressionFound = 'Statement expected, but expression of type ''%s'' found';
  emSquareMatrixRequired = 'Square matrix requared';
  emSyntaxErrorInRealNumber = 'Syntax error in real number';
  emTextAfterFinalEND = 'Text after final ''END.''';  //  Esto es un Warning
  emTooManyActualParameters = 'Too many actual parameters';
  emTypeNameIsNotYetCompletelyDefined = 'Type ''%s'' is not yet completely defined';  //  todavia
  emTypeOfExpressionMustBeBOOLEAN = 'Type of expression must be BOOLEAN';
  emTypeOfExpressionMustBeINTEGER = 'Type of expression must be INTEGER';  
  emTypesOfActualAndFormalVarParametersMustBeIdentical = 'Types of actual and formal var parameters must be identical';
  emUndeclaredIdentifier = 'Undeclared identifier: ''%s''';
  emUnexpectedEndOfFileInComment = 'Unexpected end of file in comment started on line %d';  //  Todavia (coments {} )
  emUnknownDirective = 'Unknown directive: ''%s''';  //  todavia
  emUnsatisfiedForwardDeclaration = 'Unsatisfied forward declaration: ''%s''';  // todavia
  emUnterminatedString = 'Unterminated string';
  emValueAssignedToNameNeverUsed = 'Value assigned to ''%s'' never used';  //  todavia
  emVariableNameIsDeclaredButNeverUsed = 'Variable ''%s'' is declared but never used in ''%s''';  //  todavia
  emVariableNameMightNotHaveBeenInitialized = 'Variable ''%s'' might not have been initialized';  //  todavia

  lsConstant = 'Constant';
  lsVariable = 'Variable';
  lsType = 'Type';
  lsIdentifier = 'Identifier';
  lsStatement = 'Statement';
  lsDirective = 'Directive';
  lsExpression = 'Expression';

implementation

end.
