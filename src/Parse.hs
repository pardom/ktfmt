module Parse where

import           AST
import           Control.Applicative     hiding ( (<|>)
                                                , many
                                                , optional
                                                )
import           Control.Monad
import           Parse.Util
import           Parse.Util.Keyword
import           Parse.Util.Operator
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.String

-- = Syntax

-- start
-- kotlinFile
--   : preamble topLevelObject*
--   ;
kotlinFile :: Parser KotlinFile
kotlinFile = KotlinFile <$> preamble <*> many topLevelObject

-- start
-- script
--   : preamble expression*
--   ;
script :: Parser Script
script = Script <$> preamble <*> many expression

-- preamble (used by script, kotlinFile)
--   : fileAnnotations? packageHeader? import*
--   ;
preamble :: Parser Preamble
preamble =
  Preamble
    <$> optionMaybe fileAnnotations
    <*> optionMaybe packageHeader
    <*> many _import

-- fileAnnotations (used by preamble)
--   : fileAnnotation*
--   ;
fileAnnotations :: Parser FileAnnotations
fileAnnotations = FileAnnotations <$> many fileAnnotation

-- fileAnnotation (used by fileAnnotations)
--   : "@" "file" ":" ("[" unescapedAnnotation+ "]" | unescapedAnnotation)
--   ;
fileAnnotation :: Parser FileAnnotation
fileAnnotation =
  FileAnnotation
    <$> (  char '@'
        *> string "file"
        *> spaces
        *> char ':'
        *> spaces
        *> optional (char '[')
        *> many1 unescapedAnnotation
        <* optional (char ']')
        )

-- packageHeader (used by preamble)
--   : modifiers "package" SimpleName{"."} SEMI?
--   ;
packageHeader :: Parser PackageHeader
packageHeader =
  PackageHeader
    <$> modifiers
    <*> (keywordPackage *> simpleName `sepBy1` operatorAccessor)
    <*> optionMaybe semi

-- import (used by preamble)
--   : "import" SimpleName{"."} ("." "*" | "as" SimpleName)? SEMI?
--   ;
_import :: Parser Import
_import = try importWildcard <|> try importAlias <|> importSimple

importSimple :: Parser Import
importSimple =
  ImportSimple
    <$> (  keywordImport
        *> spaces
        *> (simpleName `sepBy1` operatorAccessor)
        <* spaces
        )
    <*> optionMaybe semi

importWildcard :: Parser Import
importWildcard =
  ImportWildcard
    <$> (  keywordImport
        *> spaces
        *> (simpleName `endBy` operatorAccessor)
        <* star
        <* spaces
        )
    <*> optionMaybe semi

importAlias :: Parser Import
importAlias =
  ImportAlias
    <$> (  keywordImport
        *> spaces
        *> (simpleName `sepBy1` operatorAccessor)
        <* spaces
        )
    <*> (keywordAs *> spaces *> simpleName <* spaces)
    <*> optionMaybe semi

-- topLevelObject (used by kotlinFile)
--   : class
--   : object
--   : function
--   : property
--   : typeAlias
--   ;
topLevelObject :: Parser TopLevelObject
topLevelObject =
  (TopLevelObjectClass <$> _class)
    <|> (TopLevelObjectObject <$> object)
    <|> (TopLevelObjectFunction <$> function)
    <|> (TopLevelObjectProperty <$> property)
    <|> (TopLevelObjectTypeAlias <$> typeAlias)

-- typeAlias (used by memberDeclaration, declaration, topLevelObject)
--   : modifiers "typealias" SimpleName typeParameters? "=" type
--   ;
typeAlias :: Parser TypeAlias
typeAlias =
  TypeAlias
    <$> (modifiers <* spaces)
    <*> (keywordTypealias *> spaces *> simpleName <* spaces)
    <*> (optionMaybe typeParameters <* spaces)
    <*> (operatorEqual *> spaces *> _type <* spaces <* try semi)

-- == Classes

-- class (used by memberDeclaration, declaration, topLevelObject)
--   : modifiers ("class" | "interface") SimpleName
--       typeParameters?
--       primaryConstructor?
--       (":" annotations delegationSpecifier{","})?
--       typeConstraints
--       (classBody? | enumClassBody)
--   ;
_class :: Parser Class
_class = undefined

-- primaryConstructor (used by class, object)
--   : (modifiers "constructor")? ("(" functionParameter{","} ")")
--   ;
primaryConstructor :: Parser PrimaryConstructor
primaryConstructor =
  PrimaryConstructor
    <$> optionMaybe (modifiers <* spaces <* keywordConstructor <* spaces)
    <*> (parens (functionParameter `sepBy1` comma) <* spaces)

-- classBody (used by objectLiteral, enumEntry, class, companionObject, object)
--   : ("{" members "}")?
--   ;
classBody :: Parser ClassBody
classBody = ClassBody <$> optionMaybe
  (  spaces
  *> (between (spaces *> char '{') (spaces *> char '}') members <* spaces)
  )

members :: Parser Members
members = Members <$> many memberDeclaration

delegationSpecifier :: Parser DelegationSpecifier
delegationSpecifier =
  (DelegationSpecifierConstructorInvocation <$> constructorInvocation)
    <|> (DelegationSpecifierUserType <$> userType)
    <|> (DelegationSpecifierExplicitDelegation <$> explicitDelegation)

explicitDelegation :: Parser ExplicitDelegation
explicitDelegation =
  ExplicitDelegation
    <$> userType
    <*> (spaces *> keywordBy *> spaces *> expression)

typeParameters :: Parser TypeParameters
typeParameters =
  TypeParameters <$> (char '<' *> (typeParameter `sepBy1` comma) <* char '>')

typeParameter :: Parser TypeParameter
typeParameter =
  TypeParameter
    <$> (pad modifiers)
    <*> (pad simpleName)
    <*> (pad (optionMaybe (pad operatorTypeSeparator *> userType)))

typeConstraints :: Parser TypeConstraints
typeConstraints = TypeConstraints
  <$> pad (pad (string "where") *> (typeConstraint `sepBy1` pad comma))

typeConstraint :: Parser TypeConstraint
typeConstraint =
  TypeConstraint
    <$> (pad annotations)
    <*> (pad simpleName)
    <*> (pad (pad operatorTypeSeparator *> _type))

-- === Class members

-- memberDeclaration (used by members)
--   : companionObject
--   : object
--   : function
--   : property
--   : class
--   : typeAlias
--   : anonymousInitializer
--   : secondaryConstructor
--   ;
memberDeclaration :: Parser MemberDeclaration
memberDeclaration =
  (MemberDeclarationCompanionObject <$> companionObject)
    <|> (MemberDeclarationObject <$> object)
    <|> (MemberDeclarationFunction <$> function)
    <|> (MemberDeclarationProperty <$> property)
    <|> (MemberDeclarationClass <$> _class)
    <|> (MemberDeclarationAnonymousInitializer <$> anonymousInitializer)
    <|> (MemberDeclarationSecondaryConstructor <$> secondaryConstructor)

-- anonymousInitializer (used by memberDeclaration)
--   : "init" block
--   ;
anonymousInitializer :: Parser AnonymousInitializer
anonymousInitializer =
  AnonymousInitializer <$> (spaces *> keywordInit *> spaces *> block <* spaces)

-- companionObject (used by memberDeclaration)
--   : modifiers "companion" "object" SimpleName? (":" delegationSpecifier{","})? classBody?
--   ;
companionObject :: Parser CompanionObject
companionObject =
  CompanionObject
    <$> (modifiers <* keywordCompanion <* keywordObject)
    <*> (optionMaybe simpleName)
    <*> (optionMaybe
          (operatorTypeSeparator *> (delegationSpecifier `sepBy1` comma))
        )
    <*> (optionMaybe classBody)

-- valueParameters (used by secondaryConstructor, function)
--   : "(" functionParameter{","}? ")"
--   ;
valueParameters :: Parser ValueParameters
valueParameters =
  ValueParameters <$> parens (optionMaybe (functionParameter `sepBy1` comma))

-- functionParameter (used by valueParameters, primaryConstructor)
--   : modifiers ("val" | "var")? parameter ("=" expression)?
--   ;
functionParameter :: Parser FunctionParameter
functionParameter = undefined

-- block (used by catchBlock, anonymousInitializer, secondaryConstructor, functionBody, controlStructureBody, try, finallyBlock)
--   : "{" statements "}"
--   ;
block :: Parser Block
block = Block <$> braces statements

-- function (used by memberDeclaration, declaration, topLevelObject)
--   : modifiers "fun"
--       typeParameters?
--       (type ".")?
--       SimpleName
--       typeParameters? valueParameters (":" type)?
--       typeConstraints
--       functionBody?
--   ;
function :: Parser Function
function = undefined

-- functionBody (used by getter, setter, function)
--   : block
--   : "=" expression
--   ;
functionBody :: Parser FunctionBody
functionBody =
  (FunctionBodyBlock <$> pad block)
    <|> (FunctionBodyExpression <$> pad (pad operatorAssign *> expression))

-- variableDeclarationEntry (used by for, lambdaParameter, property, multipleVariableDeclarations)
--   : SimpleName (":" type)?
--   ;
variableDeclarationEntry :: Parser VariableDeclarationEntry
variableDeclarationEntry =
  VariableDeclarationEntry <$> simpleName <*> optionMaybe _type

-- multipleVariableDeclarations (used by for, lambdaParameter, property)
--   : "(" variableDeclarationEntry{","} ")"
--   ;
multipleVariableDeclarations :: Parser MultipleVariableDeclarations
multipleVariableDeclarations = MultipleVariableDeclarations
  <$> parens (sepBy1 variableDeclarationEntry comma)

-- property (used by memberDeclaration, declaration, topLevelObject)
--   : modifiers ("val" | "var")
--       typeParameters?
--       (type ".")?
--       (multipleVariableDeclarations | variableDeclarationEntry)
--       typeConstraints
--       ("by" | "=" expression SEMI?)?
--       (getter? setter? | setter? getter?) SEMI?
--   ;
property :: Parser Property
property = undefined

-- getter (used by property)
--   : modifiers "get"
--   : modifiers "get" "(" ")" (":" type)? functionBody
--   ;
getter :: Parser Getter
getter = undefined

-- setter (used by property)
--   : modifiers "set"
--   : modifiers "set" "(" modifiers (SimpleName | parameter) ")" functionBody
--   ;
setter :: Parser Setter
setter = undefined

-- parameter (used by functionType, setter, functionParameter)
--   : SimpleName ":" type
--   ;
parameter :: Parser Parameter
parameter =
  Parameter <$> (pad simpleName) <*> (pad operatorTypeSeparator *> _type)

-- object (used by memberDeclaration, declaration, topLevelObject)
--   : modifiers "object" SimpleName primaryConstructor? (":" delegationSpecifier{","})? classBody?
--   ;
object :: Parser Object
object =
  Object
    <$> modifiers
    <*> (keywordObject *> simpleName)
    <*> optionMaybe primaryConstructor
    <*> optionMaybe
          (operatorTypeSeparator *> delegationSpecifier `sepBy1` comma)
    <*> optionMaybe classBody

-- secondaryConstructor (used by memberDeclaration)
--   : modifiers "constructor" valueParameters (":" constructorDelegationCall)? block
--   ;
secondaryConstructor :: Parser SecondaryConstructor
secondaryConstructor =
  SecondaryConstructor
    <$> modifiers
    <*> (keywordConstructor *> valueParameters)
    <*> optionMaybe (char ':' *> constructorDelegationCall)
    <*> block

-- constructorDelegationCall (used by secondaryConstructor)
--   : "this" valueArguments
--   : "super" valueArguments
--   ;
constructorDelegationCall :: Parser ConstructorDelegationCall
constructorDelegationCall =
  (ConstructorDelegationCallThis <$> (keywordThis *> valueArguments))
    <|> (ConstructorDelegationCallSuper <$> (keywordSuper *> valueArguments))

-- === Enum classes

-- enumClassBody (used by class)
--   : "{" enumEntries (";" members)? "}"
--   ;
enumClassBody :: Parser EnumClassBody
enumClassBody =
  EnumClassBody
    <$> (operator "{" *> enumEntries)
    <*> (optionMaybe (operator ";" *> members) <* operator "}")

-- enumEntries (used by enumClassBody)
--   : (enumEntry{","} ","? ";"?)?
--   ;
enumEntries :: Parser EnumEntries
enumEntries =
  EnumEntries
    <$> (optionMaybe
          (enumEntry `sepBy1` comma <* optional comma <* optional semicolon)
        )

-- enumEntry (used by enumEntries)
--   : modifiers SimpleName valueArguments? classBody?
--   ;
enumEntry :: Parser EnumEntry
enumEntry =
  EnumEntry
    <$> modifiers
    <*> simpleName
    <*> optionMaybe valueArguments
    <*> optionMaybe classBody

-- == Types

-- type (used by namedInfix, simpleUserType, getter, atomicExpression, whenCondition, property, typeArguments, function, typeAlias, parameter, functionType, variableDeclarationEntry, lambdaParameter, typeConstraint)
--   : typeModifiers typeReference
--   ;
_type :: Parser Type
_type = Type <$> typeModifiers <*> typeReference

-- typeReference (used by typeReference, nullableType, type)
--   : "(" typeReference ")"
--   : functionType
--   : userType
--   : nullableType
--   : "dynamic"
--   ;
typeReference :: Parser TypeReference
typeReference =
  (TypeReferenceTypeReference <$> parens typeReference)
    <|> (TypeReferenceFunctionType <$> functionType)
    <|> (TypeReferenceUserType <$> userType)
    <|> (TypeReferenceNullableType <$> nullableType)
    <|> (keywordDynamic *> pure TypeReferenceDynamic)

-- nullableType (used by typeReference)
--   : typeReference "?"
--   ;
nullableType :: Parser NullableType
nullableType = NullableType <$> typeReference <* pad (char '?')

-- userType (used by typeParameter, catchBlock, callableReference, typeReference, delegationSpecifier, constructorInvocation, explicitDelegation)
--   : simpleUserType{"."}
--   ;
userType :: Parser UserType
userType = UserType <$> (simpleUserType `sepBy1` operatorAccessor)

-- simpleUserType (used by userType)
--   : SimpleName ("<" (projection? type | "*"){","} ">")?
--   ;
simpleUserType :: Parser SimpleUserType
simpleUserType = undefined

-- projection (used by simpleUserType)
--   : varianceAnnotation
--   ;
projection :: Parser Projection
projection = Projection <$> varianceAnnotation

-- functionType (used by typeReference)
--   : (type ".")? "(" parameter{","}? ")" "->" type
--   ;
functionType :: Parser FunctionType
functionType =
  FunctionType
    <$> optionMaybe (_type <* operatorAccessor)
    <*> parens (optionMaybe (parameter `sepBy1` comma))
    <*> (operatorArrow *> _type)

-- == Control structures

-- controlStructureBody (used by whenEntry, for, if, doWhile, while)
--   : block
--   : blockLevelExpression
--   ;
controlStructureBody :: Parser ControlStructureBody
controlStructureBody =
  (ControlStructureBodyBlock <$> block)
    <|> (ControlStructureBodyBlockLevelExpression <$> blockLevelExpression)

-- if (used by atomicExpression)
--   : "if" "(" expression ")" controlStructureBody SEMI? ("else" controlStructureBody)?
--   ;
_if :: Parser If
_if =
  If
    <$> parens expression
    <*> controlStructureBody
    <*> optionMaybe semi
    <*> optionMaybe (keywordElse *> controlStructureBody)

-- try (used by atomicExpression)
--   : "try" block catchBlock* finallyBlock?
--   ;
_try :: Parser Try
_try =
  Try
    <$> (keywordTry *> block)
    <*> (many catchBlock)
    <*> (optionMaybe finallyBlock)

-- catchBlock (used by try)
--   : "catch" "(" annotations SimpleName ":" userType ")" block
--   ;
catchBlock :: Parser CatchBlock
catchBlock =
  CatchBlock
    <$> (operator "(" *> annotations)
    <*> simpleName
    <*> (operatorTypeSeparator *> userType)
    <*> (operator ")" *> block)

-- finallyBlock (used by try)
--   : "finally" block
--   ;
finallyBlock :: Parser FinallyBlock
finallyBlock = FinallyBlock <$> (keywordFinally *> block)

-- loop (used by atomicExpression)
--   : for
--   : while
--   : doWhile
--   ;
loop :: Parser Loop
loop =
  (LoopFor <$> for) <|> (LoopWhile <$> while) <|> (LoopDoWhile <$> doWhile)

-- for (used by loop)
--   : "for" "(" annotations (multipleVariableDeclarations | variableDeclarationEntry) "in" expression ")" controlStructureBody
--   ;
for :: Parser For
for = undefined

-- while (used by loop)
--   : "while" "(" expression ")" controlStructureBody
--   ;
while :: Parser While
while = While <$> (keywordWhile *> parens expression) <*> controlStructureBody

-- doWhile (used by loop)
--   : "do" controlStructureBody "while" "(" expression ")"
--   ;
doWhile :: Parser DoWhile
doWhile =
  DoWhile
    <$> (keywordDo *> controlStructureBody <* keywordWhile)
    <*> (parens expression)

-- == Expressions

-- expression (used by for, atomicExpression, longTemplate, whenCondition, functionBody, doWhile, property, script, explicitDelegation, jump, while, arrayAccess, blockLevelExpression, if, when, valueArguments, functionParameter)
--   : disjunction (assignmentOperator disjunction)*
--   ;
expression :: Parser Expression
expression =
  Expression
    <$> disjunction
    <*> (many (toPair <$> assignmentOperator <*> disjunction))

-- disjunction (used by expression)
--   : conjunction ("||" conjunction)*
--   ;
disjunction :: Parser Disjunction
disjunction = Disjunction <$> conjunction <*> many (operatorOr *> conjunction)

-- conjunction (used by disjunction)
--   : equalityComparison ("&&" equalityComparison)*
--   ;
conjunction :: Parser Conjunction
conjunction = Conjunction <$> equalityComparison <*> many
  (operatorAnd *> equalityComparison)

-- equalityComparison (used by conjunction)
--   : comparison (equalityOperation comparison)*
--   ;
equalityComparison :: Parser EqualityComparison
equalityComparison =
  EqualityComparison
    <$> comparison
    <*> (many (toPair <$> equalityComparison <*> comparison))

-- comparison (used by equalityComparison)
--   : namedInfix (comparisonOperation namedInfix)*
--   ;
comparison :: Parser Comparison
comparison =
  Comparison
    <$> namedInfix
    <*> (many (toPair <$> comparisonOperation <*> namedInfix))

-- namedInfix (used by comparison)
--   : elvisExpression (inOperation elvisExpression)*
--   : elvisExpression (isOperation type)?
--   ;
namedInfix :: Parser NamedInfix
namedInfix =
  (   NamedInfixIn
    <$> elvisExpression
    <*> (many (toPair <$> inOperation <*> elvisExpression))
    )
    <|> (   NamedInfixIs
        <$> elvisExpression
        <*> (optionMaybe (toPair <$> isOperation <*> _type))
        )

-- elvisExpression (used by namedInfix)
--   : infixFunctionCall ("?:" infixFunctionCall)*
--   ;
elvisExpression :: Parser ElvisExpression
elvisExpression =
  ElvisExpression
    <$> infixFunctionCall
    <*> (many (operatorElvis *> infixFunctionCall))

-- infixFunctionCall (used by elvisExpression)
--   : rangeExpression (SimpleName rangeExpression)*
--   ;
infixFunctionCall :: Parser InfixFunctionCall
infixFunctionCall =
  InfixFunctionCall
    <$> rangeExpression
    <*> (many (toPair <$> simpleName <*> rangeExpression))

-- rangeExpression (used by infixFunctionCall)
--   : additiveExpression (".." additiveExpression)*
--   ;
rangeExpression :: Parser RangeExpression
rangeExpression =
  RangeExpression
    <$> additiveExpression
    <*> (many (operatorRange *> additiveExpression))

-- additiveExpression (used by rangeExpression)
--   : multiplicativeExpression (additiveOperation multiplicativeExpression)*
--   ;
additiveExpression :: Parser AdditiveExpression
additiveExpression =
  AdditiveExpression
    <$> multiplicativeExpression
    <*> (many (toPair <$> additiveOperation <*> multiplicativeExpression))

-- multiplicativeExpression (used by additiveExpression)
--   : typeRHS (multiplicativeOperation typeRHS)*
--   ;
multiplicativeExpression :: Parser MultiplicativeExpression
multiplicativeExpression =
  MultiplicativeExpression
    <$> typeRHS
    <*> (many (toPair <$> multiplicativeOperation <*> typeRHS))

-- typeRHS (used by multiplicativeExpression)
--   : prefixUnaryExpression (typeOperation prefixUnaryExpression)*
--   ;
typeRHS :: Parser TypeRHS
typeRHS =
  TypeRHS
    <$> prefixUnaryExpression
    <*> (many (toPair <$> typeOperation <*> prefixUnaryExpression))

-- prefixUnaryExpression (used by typeRHS)
--   : prefixUnaryOperation* postfixUnaryExpression
--   ;
prefixUnaryExpression :: Parser PrefixUnaryExpression
prefixUnaryExpression =
  PrefixUnaryExpression
    <$> many prefixUnaryExpression
    <*> postfixUnaryExpression

-- postfixUnaryExpression (used by prefixUnaryExpression, postfixUnaryOperation)
--   : atomicExpression postfixUnaryOperation*
--   : callableReference postfixUnaryOperation*
--   ;
postfixUnaryExpression :: Parser PostfixUnaryExpression
postfixUnaryExpression =
  (   PostfixUnaryExpressionAtomicExpression
    <$> atomicExpression
    <*> many postfixUnaryOperation
    )
    <|> (   PostfixUnaryExpressionCallableReference
        <$> callableReference
        <*> many postfixUnaryOperation
        )

-- callableReference (used by postfixUnaryExpression)
--   : (userType "?"*)? "::" SimpleName typeArguments?
--   ;
callableReference :: Parser CallableReference
callableReference =
  CallableReference
    <$> (optionMaybe userType <* many operatorNullable)
    <*> (operatorMemberReference *> simpleName)
    <*> optionMaybe typeArguments

-- atomicExpression (used by postfixUnaryExpression)
--   : "(" expression ")"
--   : literalConstant
--   : functionLiteral
--   : "this" labelReference?
--   : "super" ("<" type ">")? labelReference?
--   : if
--   : when
--   : try
--   : objectLiteral
--   : jump
--   : loop
--   : collectionLiteral
--   : SimpleName
--   ;
atomicExpression :: Parser AtomicExpression
atomicExpression = undefined

-- labelReference (used by atomicExpression, jump)
--   : "@" ++ LabelName
--   ;
labelReference :: Parser LabelReference
labelReference = LabelReference <$> (char '@' *> labelName)

-- labelDefinition (used by prefixUnaryOperation, annotatedLambda)
--   : LabelName ++ "@"
--   ;
labelDefinition :: Parser LabelDefinition
labelDefinition = LabelDefinition <$> labelName <* char '@'

-- literalConstant (used by atomicExpression)
--   : "true" | "false"
--   : stringTemplate
--   : NoEscapeString
--   : IntegerLiteral
--   : CharacterLiteral
--   : FloatLiteral
--   : "null"
--   ;
literalConstant :: Parser LiteralConstant
literalConstant =
  (LiteralConstantTrue <$ keywordTrue)
    <|> (LiteralConstantFalse <$ keywordFalse)
    <|> (LiteralConstantStringTemplate <$> try stringTemplate)
    <|> (LiteralConstantNoEscapeString <$> try noEscapeString)
    <|> (LiteralConstantIntegerLiteral <$> try integerLiteral)
    <|> (LiteralConstantCharacterLiteral <$> try characterLiteral)
    <|> (LiteralConstantFloatLiteral <$> try floatLiteral)
    <|> (LiteralConstantNull <$ keywordNull)

-- stringTemplate (used by literalConstant)
--   : "\"" stringTemplateElement* "\""
--   ;
stringTemplate :: Parser StringTemplate
stringTemplate =
  StringTemplate <$> (doubleQuote *> many stringTemplateElement <* doubleQuote)

-- stringTemplateElement (used by stringTemplate)
--   : RegularStringPart
--   : ShortTemplateEntryStart (SimpleName | "this")
--   : EscapeSequence
--   : longTemplate
--   ;
stringTemplateElement :: Parser StringTemplateElement
stringTemplateElement =
  (StringTemplateElementRegularStringPart <$> regularStringPart)
    <|> (   StringTemplateElementShortTemplateEntryStart
        <$> shortTemplateEntryStart
        )
    <|> (StringTemplateElementEscapeSequence <$> escapeSequence)
    <|> (StringTemplateElementLongTemplate <$> longTemplate)

-- longTemplate (used by stringTemplateElement)
--   : "${" expression "}"
--   ;
longTemplate :: Parser LongTemplate
longTemplate =
  LongTemplate <$> (operatorStringTemplateStart *> braces expression)

-- declaration (used by statement)
--   : function
--   : property
--   : class
--   : typeAlias
--   : object
--   ;
declaration :: Parser Declaration
declaration =
  (DeclarationFunction <$> function)
    <|> (DeclarationProperty <$> property)
    <|> (DeclarationClass <$> _class)
    <|> (DeclarationTypeAlias <$> typeAlias)
    <|> (DeclarationObject <$> object)

-- statement (used by statements)
--   : declaration
--   : blockLevelExpression
--   ;
statement :: Parser Statement
statement =
  (StatementDeclaration <$> declaration)
    <|> (StatementBlockLevelExpression <$> blockLevelExpression)

-- blockLevelExpression (used by statement, controlStructureBody)
--   : annotations ("\n")+ expression
--   ;
blockLevelExpression :: Parser BlockLevelExpression
blockLevelExpression =
  BlockLevelExpression <$> annotations <* many1 newline <*> expression

-- multiplicativeOperation (used by multiplicativeExpression)
--   : "*" : "/" : "%"
--   ;
multiplicativeOperation :: Parser MultiplicativeOperation
multiplicativeOperation =
  (MultiplicativeOperationMultiply <$ operatorMultiply)
    <|> (MultiplicativeOperationDivide <$ operatorDivide)
    <|> (MultiplicativeOperationModulus <$ operatorModulus)

-- additiveOperation (used by additiveExpression)
--   : "+" : "-"
--   ;
additiveOperation :: Parser AdditiveOperation
additiveOperation =
  (AdditiveOperationAdd <$ operatorAdd)
    <|> (AdditiveOperationSubtract <$ operatorSubtract)

-- inOperation (used by namedInfix)
--   : "in" : "!in"
--   ;
inOperation :: Parser InOperation
inOperation =
  (InOperationIn <$ keywordNotIn) <|> (InOperationNotIn <$ keywordNotIn)

-- typeOperation (used by typeRHS)
--   : "as" : "as?" : ":"
--   ;
typeOperation :: Parser TypeOperation
typeOperation =
  (TypeOperationAs <$ keywordAs)
    <|> (TypeOperationAsNullable <$ keywordAsNullable)
    <|> (TypeOperationColon <$ operatorTypeSeparator)

-- isOperation (used by namedInfix)
--   : "is" : "!is"
--   ;
isOperation :: Parser IsOperation
isOperation =
  (IsOperationIs <$ keywordIs) <|> (IsOperationNotIs <$ keywordNotIs)

-- comparisonOperation (used by comparison)
--   : "<" : ">" : ">=" : "<="
--   ;
comparisonOperation :: Parser ComparisonOperation
comparisonOperation =
  (ComparisonOperationLessThan <$ operatorLessThan)
    <|> (ComparisonOperationGreaterThan <$ operatorGreaterThan)
    <|> (ComparisonOperationLessThanOrEqualTo <$ operatorLessThanOrEqualTo)
    <|> (ComparisonOperationGreaterThanOrEqualTo <$ operatorGreaterThanOrEqualTo
        )

-- equalityOperation (used by equalityComparison)
--   : "!=" : "=="
--   ;
equalityOperation :: Parser EqualityOperation
equalityOperation =
  (EqualityOperationNotEqual <$ operatorNotEqual)
    <|> (EqualityOperationEqual <$ operatorEqual)

-- assignmentOperator (used by expression)
--   : "="
--   : "+=" : "-=" : "*=" : "/=" : "%="
--   ;
assignmentOperator :: Parser AssignmentOperator
assignmentOperator =
  (AssignmentOperatorSimple <$ operatorAssign)
    <|> (AssignmentOperatorAdd <$ operatorAssignAdd)
    <|> (AssignmentOperatorSubtract <$ operatorAssignSubtract)
    <|> (AssignmentOperatorMultiply <$ operatorAssignMultiply)
    <|> (AssignmentOperatorDivide <$ operatorAssignDivide)
    <|> (AssignmentOperatorModulus <$ operatorAssignModulus)

-- prefixUnaryOperation (used by prefixUnaryExpression)
--   : "-" : "+"
--   : "++" : "--"
--   : "!"
--   : annotations
--   : labelDefinition
--   ;
prefixUnaryOperation :: Parser PrefixUnaryOperation
prefixUnaryOperation =
  (PrefixUnaryOperationSubtract <$ operatorSubtract)
    <|> (PrefixUnaryOperationAdd <$ operatorAdd)
    <|> (PrefixUnaryOperationIncrement <$ operatorIncrement)
    <|> (PrefixUnaryOperationDecrement <$ operatorDecrement)
    <|> (PrefixUnaryOperationNegate <$ operatorNot)
    <|> (PrefixUnaryOperationAnnotations <$> annotations)
    <|> (PrefixUnaryOperationLabelDefinition <$> labelDefinition)

-- postfixUnaryOperation (used by postfixUnaryExpression)
--   : "++" : "--" : "!!"
--   : callSuffix
--   : arrayAccess
--   : memberAccessOperation postfixUnaryExpression 
--   ;
postfixUnaryOperation :: Parser PostfixUnaryOperation
postfixUnaryOperation =
  (PostfixUnaryOperationIncrement <$ try increment)
    <|> (PostfixUnaryOperationDecrement <$ try decrement)
    <|> (PostfixUnaryOperationNotNullAssertion <$ operatorNotNullAssertion)
    <|> (PostfixUnaryOperationCallSuffix <$> try callSuffix)
    <|> (PostfixUnaryOperationArrayAccess <$> try arrayAccess)
    <|> (   PostfixUnaryOperationMemberAccessOperation
        <$> try memberAccessOperation
        <*> try postfixUnaryExpression
        )

-- callSuffix (used by constructorInvocation, postfixUnaryOperation)
--   : typeArguments? valueArguments annotatedLambda
--   : typeArguments annotatedLambda
--   ;
callSuffix :: Parser CallSuffix
callSuffix =
  (   CallSuffixParameterized
    <$> optionMaybe typeArguments
    <*> valueArguments
    <*> annotatedLambda
    )
    <|> (CallSuffixUnparameterized <$> typeArguments <*> annotatedLambda)

-- annotatedLambda (used by callSuffix)
--   : ("@" unescapedAnnotation)* labelDefinition? functionLiteral
--   ;
annotatedLambda :: Parser AnnotatedLambda
annotatedLambda =
  AnnotatedLambda
    <$> many (operatorAt *> unescapedAnnotation)
    <*> optionMaybe labelDefinition
    <*> functionLiteral

-- memberAccessOperation (used by postfixUnaryOperation)
--   : "." : "?." : "?"
--   ;
memberAccessOperation :: Parser MemberAccessOperation
memberAccessOperation =
  (MemberAccessOperationSimple <$ try operatorAccessor)
    <|> (MemberAccessOperationSafe <$ try operatorSafeAccessor)
    <|> (MemberAccessOperationNullable <$ try operatorNullable)

-- typeArguments (used by callSuffix, callableReference, unescapedAnnotation)
--   : "<" type{","} ">"
--   ;
typeArguments :: Parser TypeArguments
typeArguments = TypeArguments <$> _type `sepBy1` comma


-- valueArguments (used by callSuffix, enumEntry, constructorDelegationCall, unescapedAnnotation)
--   : "(" ((SimpleName "=")? "*"? expression){","} ")"
--   ;
valueArguments :: Parser ValueArguments
valueArguments =
  ValueArguments
    <$> (        parens
            (   toPair
            <$> optionMaybe (simpleName <* operatorAssign)
            <*> expression
            )
        `sepBy1` comma
        )

-- jump (used by atomicExpression)
--   : "throw" expression
--   : "return" ++ labelReference? expression?
--   : "continue" ++ labelReference?
--   : "break" ++ labelReference?
--   ;
jump :: Parser Jump
jump =
  (JumpThrow <$> (keywordThrow *> spaces *> expression))
    <|> (   JumpReturn
        <$> (keywordReturn *> optionMaybe labelReference)
        <*> (optionMaybe expression)
        )
    <|> (JumpContinue <$> (keywordContinue *> optionMaybe labelReference))
    <|> (JumpBreak <$> (keywordBreak *> optionMaybe labelReference))

-- functionLiteral (used by atomicExpression, annotatedLambda)
--   : "{" statements "}"
--   : "{" lambdaParameter{","} "->" statements "}"
--   ;
functionLiteral :: Parser FunctionLiteral
functionLiteral = (FunctionLiteralUnparameterized <$> braces statements)
  -- <|> (FunctionLiteralParameterized <$> braces statements)

-- lambdaParameter (used by functionLiteral)
--   : variableDeclarationEntry
--   : multipleVariableDeclarations (":" type)?
--   ;
lambdaParameter :: Parser LambdaParameter
lambdaParameter =
  (LambdaParameterUntyped <$> variableDeclarationEntry)
    <|> (   LambdaParameterTyped
        <$> multipleVariableDeclarations
        <*> (optionMaybe (operatorTypeSeparator *> spaces *> _type))
        )

-- statements (used by block, functionLiteral)
--   : SEMI* statement{SEMI+} SEMI*
--   ;
statements :: Parser Statements
statements =
  Statements <$> (many semi *> statement `sepBy1` many1 semi <* many semi)

-- constructorInvocation (used by delegationSpecifier)
--   : userType callSuffix
--   ;
constructorInvocation :: Parser ConstructorInvocation
constructorInvocation = ConstructorInvocation <$> userType <*> callSuffix

-- arrayAccess (used by postfixUnaryOperation)
--   : "[" expression{","} "]"
--   ;
arrayAccess :: Parser ArrayAccess
arrayAccess = ArrayAccess <$> brackets (expression `sepBy1` comma)

-- objectLiteral (used by atomicExpression)
--   : "object" (":" delegationSpecifier{","})? classBody
--   ;
objectLiteral :: Parser ObjectLiteral
objectLiteral =
  ObjectLiteral
    <$> (keywordObject *> optionMaybe
          (operatorTypeSeparator *> delegationSpecifier `sepBy1` comma)
        )
    <*> classBody

-- collectionLiteral (used by atomicExpression)
--   : "[" element{","}? "]"
--   ;
collectionLiteral :: Parser CollectionLiteral
collectionLiteral =
  CollectionLiteral <$> brackets (optionMaybe (expression `sepBy1` comma))

-- ==== When-expression

-- when (used by atomicExpression)
--   : "when" ("(" expression ")")? "{"
--         whenEntry*
--     "}"
--   ;
when :: Parser When
when =
  When
    <$> (keywordWhen *> optionMaybe (parens expression))
    <*> (braces (many whenEntry))

-- whenEntry (used by when)
--   : whenCondition{","} "->" controlStructureBody SEMI
--   : "else" "->" controlStructureBody SEMI
--   ;
whenEntry :: Parser WhenEntry
whenEntry =
  (   WhenEntryConditional
    <$> (whenCondition `sepBy1` comma)
    <*> (operatorArrow *> controlStructureBody)
    <*> semi
    )
    <|> (   WhenEntryElse
        <$> (keywordElse *> operatorArrow *> controlStructureBody)
        <*> semi
        )

-- whenCondition (used by whenEntry)
--   : expression
--   : ("in" | "!in") expression
--   : ("is" | "!is") type
--   ;
whenCondition :: Parser WhenCondition
whenCondition =
  (WhenConditionExpression <$> expression)
    <|> (WhenConditionInExpression <$> isOperation <*> expression)
    <|> (WhenConditionIsType <$> inOperation <*> _type)

-- == Modifiers

-- modifiers (used by typeParameter, getter, packageHeader, class, property, object, function, typeAlias, secondaryConstructor, enumEntry, setter, companionObject, primaryConstructor, functionParameter)
--   : (modifier | annotations)*
--   ;
modifiers :: Parser Modifiers
modifiers = Modifiers <$> many modifier <*> many annotations

-- typeModifiers (used by type)
--   : (suspendModifier | annotations)*
--   ;
typeModifiers :: Parser TypeModifers
typeModifiers = undefined

-- modifier (used by modifiers)
--   : classModifier
--   : accessModifier
--   : varianceAnnotation
--   : memberModifier
--   : parameterModifier
--   : typeParameterModifier
--   : functionModifier
--   : propertyModifier
--   ;
modifier :: Parser Modifier
modifier =
  (ModifierClass <$> classModifier)
    <|> (ModifierAccess <$> accessModifier)
    <|> (ModifierVariance <$> varianceAnnotation)
    <|> (ModifierMember <$> memberModifier)
    <|> (ModifierParameter <$> parameterModifier)
    <|> (ModifierTypeParameter <$> typeParameterModifier)
    <|> (ModifierFunction <$> functionModifier)
    <|> (ModifierProperty <$> propertyModifier)

-- classModifier (used by modifier)
--   : "abstract"
--   : "final"
--   : "enum"
--   : "open"
--   : "annotation"
--   : "sealed"
--   : "data"
--   ;
classModifier :: Parser ClassModifier
classModifier =
  (ClassModifierAbstract <$ keywordAbstract)
    <|> (ClassModifierFinal <$ keywordFinal)
    <|> (ClassModifierEnum <$ keywordEnum)
    <|> (ClassModifierOpen <$ keywordOpen)
    <|> (ClassModifierAnnotation <$ keywordAnnotation)
    <|> (ClassModifierSealed <$ keywordSealed)
    <|> (ClassModifierData <$ keywordData)

-- memberModifier (used by modifier)
--   : "override"
--   : "open"
--   : "final"
--   : "abstract"
--   : "lateinit"
--   ;
memberModifier :: Parser MemberModifier
memberModifier =
  (MemberModifierOverride <$ keywordOverride)
    <|> (MemberModifierOpen <$ keywordOpen)
    <|> (MemberModifierFinal <$ keywordFinal)
    <|> (MemberModifierAbstract <$ keywordAbstract)
    <|> (MemberModifierLateInit <$ keywordLateinit)

-- accessModifier (used by modifier)
--   : "private"
--   : "protected"
--   : "public"
--   : "internal"
--   ;
accessModifier :: Parser AccessModifier
accessModifier =
  (AccessModifierPrivate <$ keywordPrivate)
    <|> (AccessModifierProtected <$ keywordProtected)
    <|> (AccessModifierPublic <$ keywordPublic)
    <|> (AccessModifierInternal <$ keywordInternal)

-- varianceAnnotation (used by modifier, projection)
--   : "in"
--   : "out"
--   ;
varianceAnnotation :: Parser VarianceAnnotation
varianceAnnotation =
  (VarianceAnnotationIn <$ keywordIn) <|> (VarianceAnnotationOut <$ keywordOut)

-- parameterModifier (used by modifier)
--   : "noinline"
--   : "crossinline"
--   : "vararg"
--   ;
parameterModifier :: Parser ParameterModifier
parameterModifier =
  (ParameterModifierNoInline <$ keywordNoinline)
    <|> (ParameterModifierCrossInline <$ keywordCrossinline)
    <|> (ParameterModifierVararg <$ keywordVararg)

-- typeParameterModifier (used by modifier)
--   : "reified"
--   ;
typeParameterModifier :: Parser TypeParameterModifier
typeParameterModifier = TypeParameterModifierReified <$ keywordReified

-- functionModifier (used by modifier)
--   : "tailrec"
--   : "operator"
--   : "infix"
--   : "inline"
--   : "external"
--   : suspendModifier
--   ;
functionModifier :: Parser FunctionModifier
functionModifier =
  (FunctionModifierTailRec <$ keywordTailrec)
    <|> (FunctionModifierOperator <$ keywordOperator)
    <|> (FunctionModifierInfix <$ keywordInfix)
    <|> (FunctionModifierInline <$ keywordInline)
    <|> (FunctionModifierInternal <$ keywordInternal)
    <|> (FunctionModifierSuspend <$> suspendModifier)

-- propertyModifier (used by modifier)
--   : "const"
--   ;
propertyModifier :: Parser PropertyModifier
propertyModifier = PropertyModifierConst <$ keywordConst

-- suspendModifier (used by typeModifiers, functionModifier)
--   : "suspend"
--   ;
suspendModifier :: Parser SuspendModifier
suspendModifier = SuspendModifierSuspend <$ keywordSuspend

-- multiPlatformModifier
--   : "expect"
--   : "actual"
--   ;
multiPlatformModifier :: Parser MultiPlatformModifier
multiPlatformModifier =
  (MultiPlatformModifierExpect <$ keywordExpect)
    <|> (MultiPlatformModifierActual <$ keywordActual)

-- == Annotations

-- annotations (used by catchBlock, prefixUnaryOperation, blockLevelExpression, for, typeModifiers, class, modifiers, typeConstraint)
--   : (annotation | annotationList)*
--   ;
annotations :: Parser Annotations
annotations = undefined

-- annotation (used by annotations)
--   : "@" (annotationUseSiteTarget ":")? unescapedAnnotation
--   ;
annotation :: Parser Annotation
annotation =
  Annotation
    <$> (  operatorAt
        *> optionMaybe (annotationUseSiteTarget <* operatorTypeSeparator)
        )
    <*> unescapedAnnotation

-- annotationList (used by annotations)
--   : "@" (annotationUseSiteTarget ":")? "[" unescapedAnnotation+ "]"
--   ;
annotationList :: Parser AnnotationList
annotationList =
  AnnotationList
    <$> (  operatorAt
        *> optionMaybe (annotationUseSiteTarget <* operatorTypeSeparator)
        )
    <*> (brackets (many unescapedAnnotation))

-- annotationUseSiteTarget (used by annotation, annotationList)
--   : "field"
--   : "file"
--   : "property"
--   : "get"
--   : "set"
--   : "receiver"
--   : "param"
--   : "setparam"
--   : "delegate"
--   ;
annotationUseSiteTarget :: Parser AnnotationUseSiteTarget
annotationUseSiteTarget =
  (AnnotationUseSiteTargetField <$ keywordField)
    <|> (AnnotationUseSiteTargetFile <$ keywordFile)
    <|> (AnnotationUseSiteTargetProperty <$ keywordProperty)
    <|> (AnnotationUseSiteTargetGet <$ keywordGet)
    <|> (AnnotationUseSiteTargetSet <$ keywordSet)
    <|> (AnnotationUseSiteTargetReceiver <$ keywordReceiver)
    <|> (AnnotationUseSiteTargetParam <$ keywordParam)
    <|> (AnnotationUseSiteTargetSetParam <$ keywordSetparam)
    <|> (AnnotationUseSiteTargetDelegate <$ keywordDelegate)

-- unescapedAnnotation (used by annotation, fileAnnotation, annotatedLambda, annotationList)
--   : SimpleName{"."} typeArguments? valueArguments?
--   ;
unescapedAnnotation :: Parser UnescapedAnnotation
unescapedAnnotation =
  UnescapedAnnotation
    <$> (simpleName `sepBy` operatorAccessor)
    <*> (optionMaybe typeArguments)
    <*> (optionMaybe valueArguments)

-- = Lexical structure

-- helper
-- LongSuffix (used by IntegerLiteral)
--   : "L"
--   ;
longSuffix :: Parser LongSuffix
longSuffix = LongSuffix <$ char 'L'

-- IntegerLiteral (used by literalConstant)
--   : DecimalLiteral LongSuffix?
--   : HexadecimalLiteral LongSuffix?
--   : BinaryLiteral LongSuffix?
--   ;
integerLiteral =
  (IntegerLiteralDecimalLiteral <$> decimalLiteral <*> optionMaybe longSuffix)
    <|> (   IntegerLiteralHexadecimalLiteral
        <$> hexadecimalLiteral
        <*> optionMaybe longSuffix
        )
    <|> (   IntegerLiteralBinaryLiteral
        <$> binaryLiteral
        <*> optionMaybe longSuffix
        )

-- DecimalLiteral (used by IntegerLiteral)
--   : Digit
--   : Digit (Digit | "_")* Digit
--   ;
decimalLiteral :: Parser DecimalLiteral
decimalLiteral =
  DecimalLiteral
    <$> (digit <:> option "" (many (digit <|> underscore) <+++> digit))

-- FloatLiteral (used by literalConstant)
--   : <Java double literal>
--   ;
floatLiteral :: Parser FloatLiteral
floatLiteral =
  FloatLiteral
    <$>  many1 digit
    <++> option "" (char '.' <:> many digit)
    <++> option "" (toList (char 'f' <|> char 'F'))

-- HexadecimalLiteral (used by IntegerLiteral)
--   : "0" ("x" | "X") HexDigit
--   : "0" ("x" | "X") HexDigit (HexDigit | "_")* HexDigit
--   ;
hexadecimalLiteral :: Parser HexadecimalLiteral
hexadecimalLiteral =
  HexadecimalLiteral
    <$> (   try
            (    hexadecimalPrefix
            <++> (between hexDigit hexDigit (many (hexDigit <|> underscore)))
            )
        <|> (hexadecimalPrefix <+++> hexDigit)
        )

hexadecimalPrefix :: Parser String
hexadecimalPrefix = sequence [char '0', char 'x' <|> char 'X']

-- helper
-- BinaryDigit (used by BinaryLiteral)
--   : ("0" | "1")
--   ;
binaryDigit :: Parser Char
binaryDigit = char '0' <|> char '1'

binaryPrefix :: Parser String
binaryPrefix = sequence [char '0', char 'b' <|> char 'B']

-- BinaryLiteral (used by IntegerLiteral)
--   : "0" ("b" | "B") BinaryDigit
--   : "0" ("b" | "B") BinaryDigit (BinaryDigit | "_")* BinaryDigit
binaryLiteral :: Parser BinaryLiteral
binaryLiteral =
  BinaryLiteral
    <$> (   try
            (    binaryPrefix
            <++> (between binaryDigit
                          binaryDigit
                          (many (binaryDigit <|> underscore))
                 )
            )
        <|> (binaryPrefix <+++> binaryDigit)
        )

-- CharacterLiteral (used by literalConstant)
--   : <character as in Java>
--   ;
characterLiteral :: Parser CharacterLiteral
characterLiteral =
  CharacterLiteral
    <$> (     singleQuote
        <:>   (   try unicodeEscapeSequence
              <|> try regularEscapeSequence
              <|> toList (noneOf "'")
              )
        <+++> singleQuote
        )

-- NoEscapeString (used by literalConstant)
--   : <"""-quoted string>
--   ;
noEscapeString :: Parser NoEscapeString
noEscapeString =
  NoEscapeString <$> doubleQuote <:> many (noneOf "\\\"$\n") <+++> doubleQuote

-- RegularStringPart (used by stringTemplateElement)
--   : <any character other than backslash, quote, $ or newline>
--   ;
regularStringPart :: Parser RegularStringPart
regularStringPart = RegularStringPart <$> noneOf "\\\"$\n"

-- ShortTemplateEntryStart (used by stringTemplateElement)
--   : "$"
--   ;
shortTemplateEntryStart :: Parser ShortTemplateEntryStart
shortTemplateEntryStart = ShortTemplateEntryStart <$ char '$'

-- EscapeSequence (used by stringTemplateElement)
--   : UnicodeEscapeSequence | RegularEscapeSequence
--   ;
escapeSequence :: Parser EscapeSequence
escapeSequence =
  (UnicodeEscapeSequence <$> unicodeEscapeSequence)
    <|> (RegularEscapeSequence <$> regularEscapeSequence)

-- UnicodeEscapeSequence (used by EscapeSequence)
--   : "\u" HexDigit{4}
--   ;
unicodeEscapeSequence :: Parser String
unicodeEscapeSequence = sequence [backslash, char 'u'] <++> count 4 hexDigit

-- RegularEscapeSequence (used by EscapeSequence)
--   : "\" <any character other than newline>
--   ;
regularEscapeSequence :: Parser String
regularEscapeSequence = sequence [backslash, noneOf "\n"]

-- SEMI (used by whenEntry, if, statements, packageHeader, property, import)
--   : <semicolon or newline>
--   ;
semi :: Parser SEMI
semi = NewLine <$ newline <|> Semicolon <$ char ';'

-- SimpleName (used by typeParameter, catchBlock, simpleUserType, atomicExpression, LabelName, packageHeader, class, object, infixFunctionCall, function, typeAlias, parameter, callableReference, variableDeclarationEntry, stringTemplateElement, enumEntry, setter, import, companionObject, valueArguments, unescapedAnnotation, typeConstraint)
--   : <java identifier>
--   : "`" <java identifier> "`"
--   ;
simpleName :: Parser SimpleName
simpleName = SimpleName <$> javaIdentifier

javaIdentifier :: Parser String
javaIdentifier = regularJavaIdentifier <|> escapedJavaIdentifier

regularJavaIdentifier :: Parser String
regularJavaIdentifier = javaIdentifierStart <:> many javaIdentifierPart

escapedJavaIdentifier :: Parser String
escapedJavaIdentifier = backtick <:> regularJavaIdentifier <+++> backtick

javaIdentifierStart :: Parser Char
javaIdentifierStart = letter <|> oneOf "$_"

javaIdentifierPart :: Parser Char
javaIdentifierPart = digit <|> javaIdentifierStart

-- LabelName (used by labelReference, labelDefinition)
--   : SimpleName
--   ;
labelName :: Parser LabelName
labelName = LabelName <$> simpleName
