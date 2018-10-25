module ParseSpec where

import           AST
import           Control.Monad.IO.Class
import           Parse
import           Parse.Util
import           Test.Hspec
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.String
import           Util

spec :: Spec
spec = do

    describe "Parse._import" $ do
        shouldParse
            _import
            "simple import"
            "import foo.bar.baz"
            (ImportSimple
                [SimpleName "foo", SimpleName "bar", SimpleName "baz"]
                Nothing
            )
        shouldParse
            _import
            "wildcard import"
            "import foo.bar.*"
            (ImportWildcard [SimpleName "foo", SimpleName "bar"] Nothing)
        shouldParse
            _import
            "alias import"
            "import foo.bar as baz"
            (ImportAlias [SimpleName "foo", SimpleName "bar"]
                         (SimpleName "baz")
                         Nothing
            )
        shouldParse _import
                    "import with newline"
                    "import foo"
                    (ImportSimple [SimpleName "foo"] (Just NewLine))
        shouldParse _import
                    "import with semicolon"
                    "import foo"
                    (ImportSimple [SimpleName "foo"] (Just Semicolon))

    describe "Parser.longSuffix" $ do
        shouldParse longSuffix "long suffix" "L" "L"
        shouldNotParse longSuffix "lower long suffix" "l"

    {-
    describe "Parser.integerLiteral" $ do
        shouldParse integerLiteral "decimal literal"     "0"    "0"
        shouldParse integerLiteral "decimal literal"     "0_0"  "0_0"

        shouldParse integerLiteral "hexadecimal literal" "0xAA" "0xAA"
        shouldParse integerLiteral
                    "hexadecimal literal with underscore"
                    "0xA_A"
                    "0xA_A"
        shouldNotParse integerLiteral "non-hexadecimal literal" "0xZZ"

        shouldParse integerLiteral
                    "lower binary literal"
                    "0b0001"
                    (BinaryLiteral "0b0001")
        shouldParse integerLiteral
                    "upper binary literal"
                    "0B0001"
                    (BinaryLiteral "0B0001")
        shouldParse binaryLiteral
                    "binary literal with underscore"
                    "0B0000_0001"
                    (BinaryLiteral "0B000_0001")
        shouldNotParse integerLiteral "non-binary binary literal"      "0b0002"
        shouldNotParse integerLiteral "non-binary integer literal"     "0"
        shouldNotParse integerLiteral "non-binary decimal literal"     "0_0"
        shouldNotParse integerLiteral "non-binary hexadecimal literal" "0x0"

    describe "Parser.decimalLiteral" $ do
        shouldParse decimalLiteral "decimal literal" "0"   "0"
        shouldParse decimalLiteral "decimal literal" "0_0" "0_0"

    describe "Parser.floatLiteral" $ do
        shouldParse floatLiteral "float literal" "0F" "0F"
        shouldParse floatLiteral "float literal" "0F" "0F"

    describe "Parser.hexadecimalLiteral" $ do
        shouldParse hexadecimalLiteral "hexadecimal literal" "0xAA" "0xAA"
        shouldParse hexadecimalLiteral
                    "hexadecimal literal with underscore"
                    "0xA_A"
                    "0xA_A"
        shouldNotParse hexadecimalLiteral "non-hexadecimal literal" "0xZZ"
    -}

    describe "Parser.hexadecimalPrefix" $ do
        shouldParse hexadecimalPrefix "lower hex prefix" "0xAA" "0x"
        shouldParse hexadecimalPrefix "upper hex prefix" "0XAA" "0X"

    describe "Parser.binaryDigit" $ do
        shouldParse binaryDigit "binary digit 0" "0" '0'
        shouldParse binaryDigit "binary digit 1" "1" '1'
        shouldNotParse binaryDigit "non-binary digit" "a"

    describe "Parser.bindaryPrefix" $ do
        shouldParse binaryPrefix "lower binary prefix" "0b0000" "0b"
        shouldParse binaryPrefix "upper binary prefix" "0B0000" "0B"

    {-
    describe "Parser.binaryLiteral" $ do
        shouldParse binaryLiteral
                    "lower binary literal"
                    "0b0001"
                    (BinaryLiteral "0b0001")
        shouldParse binaryLiteral
                    "upper binary literal"
                    "0B0001"
                    (BinaryLiteral "0B0001")
        shouldParse binaryLiteral
                    "binary literal with underscore"
                    "0B0000_0001"
                    (BinaryLiteral "0B000_0001")
        shouldNotParse binaryLiteral "non-binary binary literal"      "0b0002"
        shouldNotParse binaryLiteral "non-binary integer literal"     "0"
        shouldNotParse binaryLiteral "non-binary decimal literal"     "0_0"
        shouldNotParse binaryLiteral "non-binary hexadecimal literal" "0x0"
    -}

    describe "Parser.characterLiteral" $ do
        shouldParse characterLiteral "character literal" "'a'" "'a'"
        shouldParse characterLiteral
                    "regular escaped character literal"
                    "'\\t'"
                    "'\\t'"
        shouldParse characterLiteral
                    "unicode escaped character literal"
                    "'\\u0000'"
                    "'\\u0000'"
        shouldNotParse characterLiteral "empty character literal"           "''"
        shouldNotParse characterLiteral "regular escaped character literal" "''"
        shouldNotParse characterLiteral "unicode escaped character literal" "''"

    describe "Parser.noEscapeString" $ do
        shouldParse noEscapeString
                    "no escape string"
                    "\"foo\""
                    (NoEscapeString "\"foo\"")
        shouldNotParse noEscapeString "escaped string"     "\"\\tfoo\""
        shouldNotParse noEscapeString "unclosed string"    "\"foo"
        shouldNotParse noEscapeString "line broken string" "\"foo\n\""

    describe "Parser.regularStringPart" $ do
        shouldParse regularStringPart "regular string part" "foo" "foo"
        shouldNotParse regularStringPart "escape sequence" "\\foo"
        shouldNotParse regularStringPart "short template"  "$foo"
        shouldNotParse regularStringPart "long entry"      "${foo.bar}"
        shouldNotParse regularStringPart "double quote"    "\""
        shouldNotParse regularStringPart "new line"        "\n"

    describe "Parser.shortTemplateEntryStart" $ do
        shouldParse shortTemplateEntryStart
                    "short template"
                    "$foo"
                    ShortTemplateEntryStart
        shouldNotParse shortTemplateEntryStart "simple name" "foo"

    describe "Parser.escapeSequence" $ do
        shouldParse regularEscapeSequence "regular escape sequence" "\\u" "\\u"
        shouldParse unicodeEscapeSequence
                    "unicode escape sequence"
                    "\\u0000"
                    "\\u0000"
        shouldNotParse regularEscapeSequence "unescaped string" "foo"

    describe "Parser.unicodeEscapeSequence" $ do
        shouldParse unicodeEscapeSequence
                    "unicode escape sequence"
                    "\\u0000"
                    "\\u0000"
        shouldNotParse unicodeEscapeSequence "regular escape sequence" "\\u"

    describe "Parser.regularEscapeSequence" $ do
        shouldParse regularEscapeSequence "regular escape sequence" "\\u" "\\u"
        shouldNotParse regularEscapeSequence "unescaped string" "foo"

    describe "Parser.semi" $ do
        shouldParse semi "newline"   "\n" NewLine
        shouldParse semi "semicolon" ";"  Semicolon

    describe "Parser.simpleName" $ do
        shouldParse simpleName "valid identifier"   "foo"   "foo"
        shouldParse simpleName "escaped identifier" "`foo`" "`foo`"
        shouldNotParse simpleName "invalid identifier" "-foo"
        shouldNotParse simpleName "invalid identifier" "`-foo`"

    describe "Parser.regularJavaIdentifier" $ do
        shouldParse regularJavaIdentifier "valid identifier" "foo" "foo"
        shouldNotParse regularJavaIdentifier "invalid identifier" "-foo"

    describe "Parser.escapedJavaIdentifier" $ do
        shouldParse escapedJavaIdentifier "escaped identifier" "`foo`" "`foo`"
        shouldNotParse escapedJavaIdentifier "regular identifier" "foo"
        shouldNotParse escapedJavaIdentifier "invalid identifier" "`-foo`"

    describe "Parser.javaIdentifierStart" $ do
        shouldParse javaIdentifierStart "dollar sign" "$abc" '$'
        shouldParse javaIdentifierStart "underscore"  "_abc" '_'
        shouldParse javaIdentifierStart "letter"      "abc"  'a'
        shouldNotParse javaIdentifierStart "digit"  "0abc"
        shouldNotParse javaIdentifierStart "hyphen" "-abc"
        shouldNotParse javaIdentifierStart "slash"  "/abc"

    describe "Parser.javaIdentifierPart" $ do
        shouldParse javaIdentifierPart "dollar sign" "$abc" '$'
        shouldParse javaIdentifierPart "underscore"  "_abc" '_'
        shouldParse javaIdentifierPart "letter"      "abc"  'a'
        shouldParse javaIdentifierPart "digit"       "0abc" "0"
        shouldNotParse javaIdentifierPart "hyphen" "-abc"
        shouldNotParse javaIdentifierPart "slash"  "/abc"

    describe "Parser.surround" $ do
        shouldParse (parens (string "foo"))       "parens" "(foo)"   "foo"
        shouldParse (braces (string "foo"))       "parens" "{foo}"   "foo"
        shouldParse (brackets (string "foo"))     "parens" "[foo]"   "foo"
        shouldParse (singleQuotes (string "foo")) "parens" "'foo'"   "foo"
        shouldParse (doubleQuotes (string "foo")) "parens" "\"foo\"" "foo"
