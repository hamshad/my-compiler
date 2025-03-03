-- Step 2: Lexer converts source code into tokens.
-- parsec, text, containers dependencies added

module Lexer where


import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
-- emptyDef is used to create a default language definition
import qualified Text.Parsec.Token as Token
-- qualified imports are used to avoid name clashes


-- Lexer definition
languageDef = emptyDef {
  Token.commentStart = "/*",
  Token.commentEnd = "*/",
  Token.commentLine = "//",
  Token.indentStart = letter,
  Token.indentLetter = alphaNum <|> char '_',
  Token.reservedNames = ["let", "in", "def", "if", "then", "else"],
  Token.reservedOpNames = ["+", "-", "*", "/", "=", "(", ")", ","]
}

lexer = Token.makeTokenParser languageDef

-- Lexer components
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer
