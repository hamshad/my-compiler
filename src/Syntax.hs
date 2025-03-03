-- step 1: defining the syntax and semantics of the language.
-- 1. Integer literals
-- 2. Variables
-- 3. Basic arthmetic operations
-- 4. Function definitions and calls

module Syntax where


-- Abstract Syntax Tree (AST)
data Expr
  = LitInt Int         -- Integer literals
  | Var String         -- Variables
  | Add Expr Expr      -- Addition
  | Sub Expr Expr      -- Subtraction
  | Mul Expr Expr      -- Multiplication
  | Div Expr Expr      -- Division
  | Let String Expr Expr -- Let binding (name, value, body)
  | FuncDef String [String] Expr -- Function definition (name, args, body)
  | FuncCall String [Expr] -- Function call (name, args
  deriving (Show, Eq)
