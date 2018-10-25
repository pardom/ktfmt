module Parse.Util.Keyword where

import           Parse.Util
import           Text.Parsec.String


-- Hard

keywordAs :: Parser String
keywordAs = operator "as"

keywordAsNullable :: Parser String
keywordAsNullable = operator "as?"

keywordBreak :: Parser String
keywordBreak = operator "break"

keywordClass :: Parser String
keywordClass = operator "class"

keywordContinue :: Parser String
keywordContinue = keyword "continue"

keywordDo :: Parser String
keywordDo = keyword "do"

keywordElse :: Parser String
keywordElse = keyword "else"

keywordFalse :: Parser String
keywordFalse = keyword "false"

keywordFor :: Parser String
keywordFor = keyword "for"

keywordFun :: Parser String
keywordFun = keyword "fun"

keywordIf :: Parser String
keywordIf = keyword "if"

keywordIn :: Parser String
keywordIn = keyword "in"

keywordNotIn :: Parser String
keywordNotIn = keyword "!in"

keywordInterface :: Parser String
keywordInterface = keyword "interface"

keywordIs :: Parser String
keywordIs = keyword "is"

keywordNotIs :: Parser String
keywordNotIs = keyword "!is"

keywordNull :: Parser String
keywordNull = keyword "null"

keywordObject :: Parser String
keywordObject = keyword "object"

keywordPackage :: Parser String
keywordPackage = keyword "package"

keywordReturn :: Parser String
keywordReturn = keyword "return"

keywordSuper :: Parser String
keywordSuper = keyword "super"

keywordThis :: Parser String
keywordThis = keyword "this"

keywordThrow :: Parser String
keywordThrow = keyword "throw"

keywordTrue :: Parser String
keywordTrue = keyword "true"

keywordTry :: Parser String
keywordTry = keyword "try"

keywordTypealias :: Parser String
keywordTypealias = keyword "typealias"

keywordVal :: Parser String
keywordVal = keyword "val"

keywordVar :: Parser String
keywordVar = keyword "var"

keywordWhen :: Parser String
keywordWhen = keyword "when"

keywordWhile :: Parser String
keywordWhile = keyword "while"

-- Soft

keywordBy :: Parser String
keywordBy = keyword "by"

keywordCatch :: Parser String
keywordCatch = keyword "catch"

keywordConstructor :: Parser String
keywordConstructor = keyword "constructor"

keywordDelegate :: Parser String
keywordDelegate = keyword "delegate"

keywordDynamic :: Parser String
keywordDynamic = keyword "dynamic"

keywordField :: Parser String
keywordField = keyword "field"

keywordFile :: Parser String
keywordFile = keyword "file"

keywordFinally :: Parser String
keywordFinally = keyword "finally"

keywordGet :: Parser String
keywordGet = keyword "get"

keywordImport :: Parser String
keywordImport = keyword "import"

keywordInit :: Parser String
keywordInit = keyword "init"

keywordParam :: Parser String
keywordParam = keyword "param"

keywordProperty :: Parser String
keywordProperty = keyword "property"

keywordReceiver :: Parser String
keywordReceiver = keyword "receiver"

keywordSet :: Parser String
keywordSet = keyword "set"

keywordSetparam :: Parser String
keywordSetparam = keyword "setparam"

keywordWhere :: Parser String
keywordWhere = keyword "where"

-- Modifier

keywordActual :: Parser String
keywordActual = keyword "actual"

keywordAbstract :: Parser String
keywordAbstract = keyword "abstract"

keywordAnnotation :: Parser String
keywordAnnotation = keyword "annotation"

keywordCompanion :: Parser String
keywordCompanion = keyword "companion"

keywordConst :: Parser String
keywordConst = keyword "const"

keywordCrossinline :: Parser String
keywordCrossinline = keyword "crossinline"

keywordData :: Parser String
keywordData = keyword "data"

keywordEnum :: Parser String
keywordEnum = keyword "enum"

keywordExpect :: Parser String
keywordExpect = keyword "expect"

keywordExternal :: Parser String
keywordExternal = keyword "external"

keywordFinal :: Parser String
keywordFinal = keyword "final"

keywordInfix :: Parser String
keywordInfix = keyword "infix"

keywordInline :: Parser String
keywordInline = keyword "inline"

keywordInner :: Parser String
keywordInner = keyword "inner"

keywordInternal :: Parser String
keywordInternal = keyword "internal"

keywordLateinit :: Parser String
keywordLateinit = keyword "lateinit"

keywordNoinline :: Parser String
keywordNoinline = keyword "noinline"

keywordOpen :: Parser String
keywordOpen = keyword "open"

keywordOperator :: Parser String
keywordOperator = keyword "operator"

keywordOut :: Parser String
keywordOut = keyword "out"

keywordOverride :: Parser String
keywordOverride = keyword "override"

keywordPrivate :: Parser String
keywordPrivate = keyword "private"

keywordProtected :: Parser String
keywordProtected = keyword "protected"

keywordPublic :: Parser String
keywordPublic = keyword "public"

keywordReified :: Parser String
keywordReified = keyword "reified"

keywordSealed :: Parser String
keywordSealed = keyword "sealed"

keywordSuspend :: Parser String
keywordSuspend = keyword "suspend"

keywordTailrec :: Parser String
keywordTailrec = keyword "tailrec"

keywordVararg :: Parser String
keywordVararg = keyword "vararg"

-- Special identifier

keywordIt :: Parser String
keywordIt = keyword "it"
