module AST where

-- = Syntax

data KotlinFile
    = KotlinFile
        Preamble
        [TopLevelObject]
    deriving Show

data Script
    = Script
        Preamble
        [Expression]
    deriving Show

data Preamble
    = Preamble
        (Maybe FileAnnotations)
        (Maybe PackageHeader)
        [Import]
    deriving Show

data FileAnnotations
    = FileAnnotations [FileAnnotation]
    deriving Show

data FileAnnotation
    = FileAnnotation [UnescapedAnnotation]
    deriving Show

data PackageHeader
    = PackageHeader
        Modifiers
        [SimpleName]
        (Maybe SEMI)
    deriving Show

data Import
    = ImportSimple
        [SimpleName]
        (Maybe SEMI)
    | ImportWildcard
        [SimpleName]
        (Maybe SEMI)
    | ImportAlias
        [SimpleName]
        SimpleName
        (Maybe SEMI)
    deriving Show

data TopLevelObject
    = TopLevelObjectClass Class
    | TopLevelObjectObject Object
    | TopLevelObjectFunction Function
    | TopLevelObjectProperty Property
    | TopLevelObjectTypeAlias TypeAlias
    deriving Show

data TypeAlias
    = TypeAlias
        Modifiers
        SimpleName
        (Maybe TypeParameters)
        Type
    deriving Show

-- == Classes

data Class
    = Class
        Modifiers
        SimpleName
        (Maybe TypeParameters)
        (Maybe PrimaryConstructor)
    | Interface
        Modifiers
        SimpleName
        (Maybe TypeParameters)
        (Maybe PrimaryConstructor)
    deriving Show

data PrimaryConstructor
    = PrimaryConstructor
        (Maybe Modifiers)
        [FunctionParameter]
    deriving Show

data ClassBody
    = ClassBody (Maybe Members)
    deriving Show

data Members
    = Members [MemberDeclaration]
    deriving Show

data DelegationSpecifier
    = DelegationSpecifierConstructorInvocation ConstructorInvocation
    | DelegationSpecifierUserType UserType
    | DelegationSpecifierExplicitDelegation ExplicitDelegation
    deriving Show

data ExplicitDelegation
    = ExplicitDelegation
        UserType
        Expression
    deriving Show

data TypeParameters
    = TypeParameters [TypeParameter]
    deriving Show

data TypeParameter
    = TypeParameter
        Modifiers
        SimpleName
        (Maybe UserType)
    deriving Show

data TypeConstraints
    = TypeConstraints [TypeConstraint]
    deriving Show

data TypeConstraint
    = TypeConstraint
        Annotations
        SimpleName
        Type
    deriving Show

-- === Class members

data MemberDeclaration
    = MemberDeclarationCompanionObject CompanionObject
    | MemberDeclarationObject Object
    | MemberDeclarationFunction Function
    | MemberDeclarationProperty Property
    | MemberDeclarationClass Class
    | MemberDeclarationTypeAlias TypeAlias
    | MemberDeclarationAnonymousInitializer AnonymousInitializer
    | MemberDeclarationSecondaryConstructor SecondaryConstructor
    deriving Show

data AnonymousInitializer
    = AnonymousInitializer Block
    deriving Show

data CompanionObject
    = CompanionObject
        Modifiers
        (Maybe SimpleName)
        (Maybe [DelegationSpecifier])
        (Maybe ClassBody)
    deriving Show

data ValueParameters
    = ValueParameters (Maybe [FunctionParameter])
    deriving Show

data FunctionParameter
    = FunctionParameterSimple
        Modifiers
        Parameter
        (Maybe Expression)
    | FunctionParameterVal
        Modifiers
        Parameter
        (Maybe Expression)
    | FunctionParameterVar
        Modifiers
        Parameter
        (Maybe Expression)
    deriving Show

data Block
    = Block Statements
    deriving Show

data Function
    = Function
        Modifiers
        (Maybe TypeParameters)
        (Maybe Type)
        SimpleName
        (Maybe TypeParameters)
        ValueParameters
        (Maybe Type)
        TypeConstraints
        (Maybe FunctionBody)
    deriving Show

data FunctionBody
    = FunctionBodyBlock Block
    | FunctionBodyExpression Expression
    deriving Show

data VariableDeclarationEntry
    = VariableDeclarationEntry
        SimpleName
        (Maybe Type)
    deriving Show

data MultipleVariableDeclarations
    = MultipleVariableDeclarations [VariableDeclarationEntry]
    deriving Show

data Property
    = Property
    deriving Show

data Getter
    = GetterSimple Modifiers
    | GetterFunctionBody
        Modifiers
        (Maybe Type)
        FunctionBody
    deriving Show

data Setter
    = SetterSimple Modifiers
    | SetterFunctionBody
        Modifiers
        (Either SimpleName Parameter)
        Modifiers
        FunctionBody
    deriving Show

data Parameter
    = Parameter
        SimpleName
        Type
    deriving Show

data Object
    = Object Modifiers SimpleName (Maybe PrimaryConstructor) (Maybe [DelegationSpecifier]) (Maybe ClassBody)
    deriving Show

data SecondaryConstructor
    = SecondaryConstructor
        Modifiers
        ValueParameters
        (Maybe ConstructorDelegationCall)
        Block
    deriving Show

data ConstructorDelegationCall
    = ConstructorDelegationCallThis ValueArguments
    | ConstructorDelegationCallSuper ValueArguments
    deriving Show

-- === Enum classes

data EnumClassBody
    = EnumClassBody EnumEntries (Maybe Members)
     deriving Show

data EnumEntries
    = EnumEntries (Maybe [EnumEntry])
    deriving Show

data EnumEntry
    = EnumEntry
        Modifiers
        SimpleName
        (Maybe ValueArguments)
        (Maybe ClassBody)
    deriving Show

-- == Types

data Type
    = Type
        TypeModifers
        TypeReference
    deriving Show

data TypeReference
    = TypeReferenceTypeReference TypeReference
    | TypeReferenceFunctionType FunctionType
    | TypeReferenceUserType UserType
    | TypeReferenceNullableType NullableType
    | TypeReferenceDynamic
    deriving Show

data NullableType
    = NullableType TypeReference
    deriving Show

data UserType
    = UserType [SimpleUserType]
    deriving Show

data SimpleUserType
    = SimpleUserType
    deriving Show

data Projection
    = Projection VarianceAnnotation
    deriving Show

data FunctionType
    = FunctionType (Maybe Type) (Maybe [Parameter]) Type
    deriving Show

-- == Control structures

data ControlStructureBody
    = ControlStructureBodyBlock Block
    | ControlStructureBodyBlockLevelExpression BlockLevelExpression
    deriving Show

data If
    = If
        Expression
        ControlStructureBody
        (Maybe SEMI)
        (Maybe ControlStructureBody)
    deriving Show

data Try
    = Try
        Block
        [CatchBlock]
        (Maybe FinallyBlock)
    deriving Show

data CatchBlock
    = CatchBlock
        Annotations
        SimpleName
        UserType
        Block
    deriving Show

data FinallyBlock
    = FinallyBlock Block
    deriving Show

data Loop
    = LoopFor For
    | LoopWhile While
    | LoopDoWhile DoWhile
    deriving Show

data For
    = For
    deriving Show

data While
    = While
        Expression
        ControlStructureBody
    deriving Show

data DoWhile
    = DoWhile
        ControlStructureBody
        Expression
    deriving Show

-- == Expressions

data Expression
    = Expression
        Disjunction
        [(AssignmentOperator, Disjunction)]
    deriving Show

data Disjunction
    = Disjunction
        Conjunction
        [Conjunction]
    deriving Show

data Conjunction
    = Conjunction
        EqualityComparison
        [EqualityComparison]
    deriving Show

data EqualityComparison
    = EqualityComparison
        Comparison
        [(EqualityComparison, Comparison)]
    deriving Show

data Comparison
    = Comparison
        NamedInfix
        [(ComparisonOperation, NamedInfix)]
    deriving Show

data NamedInfix
    = NamedInfixIn ElvisExpression [(InOperation, ElvisExpression)]
    | NamedInfixIs ElvisExpression (Maybe (IsOperation, Type))
    deriving Show

data ElvisExpression
    = ElvisExpression
        InfixFunctionCall
        [InfixFunctionCall]
    deriving Show

data InfixFunctionCall
    = InfixFunctionCall
        RangeExpression
        [(SimpleName, RangeExpression)]
    deriving Show

data RangeExpression
    = RangeExpression
        AdditiveExpression
        [AdditiveExpression]
    deriving Show

data AdditiveExpression
    = AdditiveExpression
        MultiplicativeExpression
        [(AdditiveOperation , MultiplicativeExpression)]
    deriving Show

data MultiplicativeExpression
    = MultiplicativeExpression
        TypeRHS
        [(MultiplicativeOperation, TypeRHS)]
    deriving Show

data TypeRHS
    = TypeRHS
        PrefixUnaryExpression
        [(TypeOperation, PrefixUnaryExpression)]
    deriving Show

data PrefixUnaryExpression
    = PrefixUnaryExpression
        [PrefixUnaryExpression]
        PostfixUnaryExpression
    deriving Show

data PostfixUnaryExpression
    = PostfixUnaryExpressionAtomicExpression
        AtomicExpression
        [PostfixUnaryOperation]
    | PostfixUnaryExpressionCallableReference
        CallableReference
        [PostfixUnaryOperation]
    deriving Show

data CallableReference
    = CallableReference
        (Maybe UserType)
        SimpleName
        (Maybe TypeArguments)
    deriving Show

data AtomicExpression
    = AtomicExpression
    deriving Show

data LabelReference
    = LabelReference LabelName
    deriving Show

data LabelDefinition
    = LabelDefinition LabelName
    deriving Show

data LiteralConstant
    = LiteralConstantTrue
    | LiteralConstantFalse
    | LiteralConstantStringTemplate StringTemplate
    | LiteralConstantNoEscapeString NoEscapeString
    | LiteralConstantIntegerLiteral IntegerLiteral
    | LiteralConstantCharacterLiteral CharacterLiteral
    | LiteralConstantFloatLiteral FloatLiteral
    | LiteralConstantNull
    deriving Show

data StringTemplate
    = StringTemplate [StringTemplateElement]
    deriving Show

data StringTemplateElement
    = StringTemplateElementRegularStringPart RegularStringPart
    | StringTemplateElementShortTemplateEntryStart ShortTemplateEntryStart
    | StringTemplateElementEscapeSequence EscapeSequence
    | StringTemplateElementLongTemplate LongTemplate
    deriving Show

data LongTemplate
    = LongTemplate Expression
    deriving Show

data Declaration
    = DeclarationFunction Function
    | DeclarationProperty Property
    | DeclarationClass Class
    | DeclarationTypeAlias TypeAlias
    | DeclarationObject Object
    deriving Show

data Statement
    = StatementDeclaration Declaration
    | StatementBlockLevelExpression BlockLevelExpression
    deriving Show

data BlockLevelExpression
    = BlockLevelExpression Annotations Expression
    deriving Show

data MultiplicativeOperation
    = MultiplicativeOperationMultiply
    | MultiplicativeOperationDivide
    | MultiplicativeOperationModulus
    deriving Show

data AdditiveOperation
    = AdditiveOperationAdd
    | AdditiveOperationSubtract
    deriving Show

data InOperation
    = InOperationIn
    | InOperationNotIn
    deriving Show

data TypeOperation
    = TypeOperationAs
    | TypeOperationAsNullable
    | TypeOperationColon
    deriving Show

data IsOperation
    = IsOperationIs
    | IsOperationNotIs
    deriving Show

data ComparisonOperation
    = ComparisonOperationLessThan
    | ComparisonOperationGreaterThan
    | ComparisonOperationLessThanOrEqualTo
    | ComparisonOperationGreaterThanOrEqualTo
    deriving Show

data EqualityOperation
    = EqualityOperationNotEqual
    | EqualityOperationEqual
    deriving Show

data AssignmentOperator
    = AssignmentOperatorSimple
    | AssignmentOperatorAdd
    | AssignmentOperatorSubtract
    | AssignmentOperatorMultiply
    | AssignmentOperatorDivide
    | AssignmentOperatorModulus
    deriving Show

data PrefixUnaryOperation
    = PrefixUnaryOperationSubtract
    | PrefixUnaryOperationAdd
    | PrefixUnaryOperationIncrement
    | PrefixUnaryOperationDecrement
    | PrefixUnaryOperationNegate
    | PrefixUnaryOperationAnnotations Annotations
    | PrefixUnaryOperationLabelDefinition LabelDefinition
    deriving Show

data PostfixUnaryOperation
    = PostfixUnaryOperationIncrement
    | PostfixUnaryOperationDecrement
    | PostfixUnaryOperationNotNullAssertion
    | PostfixUnaryOperationCallSuffix CallSuffix
    | PostfixUnaryOperationArrayAccess ArrayAccess
    | PostfixUnaryOperationMemberAccessOperation
        MemberAccessOperation
        PostfixUnaryExpression
    deriving Show

data CallSuffix
    = CallSuffixParameterized (Maybe TypeArguments) ValueArguments AnnotatedLambda
    | CallSuffixUnparameterized TypeArguments AnnotatedLambda
    deriving Show

data AnnotatedLambda
    = AnnotatedLambda [UnescapedAnnotation] (Maybe LabelDefinition) FunctionLiteral
    deriving Show

data MemberAccessOperation
    = MemberAccessOperationSimple
    | MemberAccessOperationSafe
    | MemberAccessOperationNullable
    deriving Show

data TypeArguments
    = TypeArguments [Type]
    deriving Show

data ValueArguments
    = ValueArguments [(Maybe SimpleName, Expression)]
    deriving Show

data Jump
    = JumpThrow Expression
    | JumpReturn (Maybe LabelReference) (Maybe Expression)
    | JumpContinue (Maybe LabelReference)
    | JumpBreak (Maybe LabelReference)
    deriving Show

data FunctionLiteral
    = FunctionLiteralUnparameterized Statements
    | FunctionLiteralParameterized [LambdaParameter] Statements
    deriving Show

data LambdaParameter
    = LambdaParameterUntyped VariableDeclarationEntry
    | LambdaParameterTyped MultipleVariableDeclarations (Maybe Type)
    deriving Show

data Statements
    = Statements [Statement]
    deriving Show

data ConstructorInvocation
    = ConstructorInvocation UserType CallSuffix
    deriving Show

data ArrayAccess
    = ArrayAccess [Expression]
    deriving Show

data ObjectLiteral
    = ObjectLiteral (Maybe [DelegationSpecifier]) ClassBody
    deriving Show

data CollectionLiteral
    = CollectionLiteral (Maybe [Expression])
    deriving Show

-- ==== When-expression

data When
    = When (Maybe Expression) [WhenEntry]
    deriving Show

data WhenEntry
    = WhenEntryConditional [WhenCondition] ControlStructureBody SEMI
    | WhenEntryElse ControlStructureBody SEMI
    deriving Show

data WhenCondition
    = WhenConditionExpression Expression
    | WhenConditionInExpression IsOperation Expression
    | WhenConditionIsType InOperation Type
    deriving Show

-- == Modifiers

data Modifiers
    = Modifiers
        [Modifier]
        [Annotations]
    deriving Show

data TypeModifers
    = TypeModifiers
        [SuspendModifier]
        [Annotations]
    deriving Show

data Modifier
    = ModifierClass ClassModifier
    | ModifierAccess AccessModifier
    | ModifierVariance VarianceAnnotation
    | ModifierMember MemberModifier
    | ModifierParameter ParameterModifier
    | ModifierTypeParameter TypeParameterModifier
    | ModifierFunction FunctionModifier
    | ModifierProperty PropertyModifier
    deriving Show

data ClassModifier
    = ClassModifierAbstract
    | ClassModifierFinal
    | ClassModifierEnum
    | ClassModifierOpen
    | ClassModifierAnnotation
    | ClassModifierSealed
    | ClassModifierData
    deriving Show

data MemberModifier
    = MemberModifierOverride
    | MemberModifierOpen
    | MemberModifierFinal
    | MemberModifierAbstract
    | MemberModifierLateInit
    deriving Show

data AccessModifier
    = AccessModifierPrivate
    | AccessModifierProtected
    | AccessModifierPublic
    | AccessModifierInternal
    deriving Show

data VarianceAnnotation
    = VarianceAnnotation
    | VarianceAnnotationIn
    | VarianceAnnotationOut
    deriving Show

data ParameterModifier
    = ParameterModifierNoInline
    | ParameterModifierCrossInline
    | ParameterModifierVararg
    deriving Show

data TypeParameterModifier
    = TypeParameterModifierReified
    deriving Show

data FunctionModifier
    = FunctionModifierTailRec
    | FunctionModifierOperator
    | FunctionModifierInfix
    | FunctionModifierInline
    | FunctionModifierInternal
    | FunctionModifierSuspend SuspendModifier
    deriving Show

data PropertyModifier
    = PropertyModifierConst
    deriving Show

data SuspendModifier
    = SuspendModifierSuspend
    deriving Show

data MultiPlatformModifier
    = MultiPlatformModifierExpect
    | MultiPlatformModifierActual
    deriving Show

-- == Annotations

data Annotations
    = Annotations
        [Annotation]
        [AnnotationList]
    deriving Show

data Annotation
    = Annotation
        (Maybe AnnotationUseSiteTarget)
        UnescapedAnnotation
    deriving Show

data AnnotationList
    = AnnotationList
        (Maybe AnnotationUseSiteTarget)
        [UnescapedAnnotation]
    deriving Show

data AnnotationUseSiteTarget
    = AnnotationUseSiteTargetField
    | AnnotationUseSiteTargetFile
    | AnnotationUseSiteTargetProperty
    | AnnotationUseSiteTargetGet
    | AnnotationUseSiteTargetSet
    | AnnotationUseSiteTargetReceiver
    | AnnotationUseSiteTargetParam
    | AnnotationUseSiteTargetSetParam
    | AnnotationUseSiteTargetDelegate
    deriving Show

data UnescapedAnnotation
    = UnescapedAnnotation
        [SimpleName]
        (Maybe TypeArguments)
        (Maybe ValueArguments)
    deriving Show

-- ||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
-- = Lexical structure

data LongSuffix
    = LongSuffix
    deriving Show

data IntegerLiteral
    = IntegerLiteralDecimalLiteral
        DecimalLiteral
        (Maybe LongSuffix)
    | IntegerLiteralHexadecimalLiteral
        HexadecimalLiteral
        (Maybe LongSuffix)
    | IntegerLiteralBinaryLiteral
        BinaryLiteral
        (Maybe LongSuffix)
    deriving Show

data DecimalLiteral
    = DecimalLiteral String
    deriving Show

data FloatLiteral
    = FloatLiteral String
    deriving Show

data HexadecimalLiteral
    = HexadecimalLiteral String
    deriving Show

data BinaryLiteral
    = BinaryLiteral String
    deriving Show

data CharacterLiteral
    = CharacterLiteral String
    deriving Show

data NoEscapeString
    = NoEscapeString String
    deriving Show

data RegularStringPart
    = RegularStringPart Char
    deriving Show

data ShortTemplateEntryStart
    = ShortTemplateEntryStart
    deriving Show

data EscapeSequence
    = UnicodeEscapeSequence String
    | RegularEscapeSequence String
    deriving Show

data SEMI
    = NewLine
    | Semicolon
    deriving (Show, Eq)

data SimpleName
    = SimpleName String
    deriving Show

data LabelName
    = LabelName SimpleName
    deriving Show
