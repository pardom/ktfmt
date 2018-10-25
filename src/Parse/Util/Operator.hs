module Parse.Util.Operator where

import           Parse.Util
import           Text.Parsec.String

-- Mathematical

operatorAdd :: Parser String
operatorAdd = operator "+"

operatorSubtract :: Parser String
operatorSubtract = operator "-"

operatorMultiply :: Parser String
operatorMultiply = operator "*"

operatorDivide :: Parser String
operatorDivide = operator "/"

operatorModulus :: Parser String
operatorModulus = operator "%"

operatorDereference :: Parser String
operatorDereference = operatorMultiply

-- Assignment

operatorAssign :: Parser String
operatorAssign = operator "="

-- Augmented assignment

operatorAssignAdd :: Parser String
operatorAssignAdd = operator "+="

operatorAssignSubtract :: Parser String
operatorAssignSubtract = operator "-="

operatorAssignMultiply :: Parser String
operatorAssignMultiply = operator "*="

operatorAssignDivide :: Parser String
operatorAssignDivide = operator "/="

operatorAssignModulus :: Parser String
operatorAssignModulus = operator "%="

-- Increment and decrement

operatorIncrement :: Parser String
operatorIncrement = operator "++"

operatorDecrement :: Parser String
operatorDecrement = operator "--"

-- Logical

operatorAnd :: Parser String
operatorAnd = operator "&&"

operatorOr :: Parser String
operatorOr = operator "||"

operatorNot :: Parser String
operatorNot = operator "!"

-- Equality

operatorEqual :: Parser String
operatorEqual = operator "=="

operatorNotEqual :: Parser String
operatorNotEqual = operator "!="

-- Referential equality

operatorReferentialEqual :: Parser String
operatorReferentialEqual = operator "==="

operatorReferentialNotEqual :: Parser String
operatorReferentialNotEqual = operator "!=="

-- Comparison

operatorLessThan :: Parser String
operatorLessThan = operator "<"

operatorLessThanOrEqualTo :: Parser String
operatorLessThanOrEqualTo = operator "<="

operatorGreaterThan :: Parser String
operatorGreaterThan = operator ">"

operatorGreaterThanOrEqualTo :: Parser String
operatorGreaterThanOrEqualTo = operator ">="

-- Indexed access

operatorLeftBracket :: Parser String
operatorLeftBracket = operator "["

operatorRightBracket :: Parser String
operatorRightBracket = operator "]"

-- Assertion

operatorNotNullAssertion :: Parser String
operatorNotNullAssertion = operator "!!"

-- Accessor

operatorAccessor :: Parser String
operatorAccessor = operator "."

-- Safe call

operatorSafeAccessor :: Parser String
operatorSafeAccessor = operator "?."

-- Elvis

operatorElvis :: Parser String
operatorElvis = operator "?:"

-- Member reference

operatorMemberReference :: Parser String
operatorMemberReference = operator "::"

-- Range

operatorRange :: Parser String
operatorRange = operator ".."

-- Type separator

operatorTypeSeparator :: Parser String
operatorTypeSeparator = operator ":"

-- Nullable

operatorNullable :: Parser String
operatorNullable = operator "?"

-- Parameter / when-condition separator

operatorArrow :: Parser String
operatorArrow = operator "->"

-- Reference / annotation

operatorAt :: Parser String
operatorAt = operator "@"

operatorAnnotationStart :: Parser String
operatorAnnotationStart = operatorAt

operatorLabel :: Parser String
operatorLabel = operatorAt

-- Statement terminator

operatorSemi :: Parser String
operatorSemi = operator ";"

operatorStatementTerminator :: Parser String
operatorStatementTerminator = operatorSemi

-- String template

operatorStringTemplateStart :: Parser String
operatorStringTemplateStart = operator "$"

-- Unused parameter

operatorUnusedParameter :: Parser String
operatorUnusedParameter = operator "_"
