-- Step 3: Parser converts tokens into an Abstract Syntax Tree (AST)

module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Token

import Lexer
import Syntax

-- Parser for expressions
parseExpr :: Parser Expr
parseExpr = Ex.buildExpressionParser operators term

-- Operator table with precedence
operators = [
  [Ex.Infix (reservedOp "*" >> return (\x y -> Mul x y)) Ex.AssocLeft,
  Ex.Infix (reservedOp "/" >> return (\x y -> Div x y)) Ex.AssocLeft],
  [Ex.Infix (reservedOp "+" >> return (\x y -> Add x y)) Ex.AssocLeft,
  Ex.Infix (reservedOp "-" >> return (\x y -> Sub x y)) Ex.AssocLeft],
]

-- Term parser - basic expressions
term = parens parseExpr
  <|> fmap IntLit (fromInteger <$> integer)
  <|> fmap Var identifier
  <|> parseLetBinding
  <|> parseFunctionDef
  <|> parseFunctionCall

-- Let binding: let x = expr in expr
parseLetBinding :: Parser Expr
parseLetBinding = do
  reserved "let"
  name <- identifier
  reservedOp "="
  value <- parseExpr
  reserved "in"
  body <- parseExpr
  return $ Let name value body

-- Function definition: def name(args) = expr
parseFunctionDef :: Parser Expr
parseFunctionDef = do
  reserved "def"
  name <- identifier
  args <- parens $ identifier `sepBy` comma
  reservedOp "="
  body <- parseExpr
  return $ FuncDef name args body

-- Function call: name(args)
parseFunctionCall :: Parser Expr
parseFunctionCall = do
  name <- identifier
  args <- parens $ parseExpr `sepBy` comma
  return $ FuncCall name args

-- Parse a whole program
parseProgramm :: Parser Expr
parseProgramm = whiteSpace *> parseExpr <* eof

-- Parse from string
parseFromString :: String -> Either ParseError Expr
parseFromString = parse parseProgramm ""

