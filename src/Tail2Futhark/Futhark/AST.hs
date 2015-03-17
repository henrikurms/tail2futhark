module Tail2Futhark.Futhark.AST where

type Program = [FunDecl]

type FunDecl = (Type, Ident, [Arg], Exp)

data Type = IntT
          | RealT
          | BoolT
          | CharT
         -- | ToupleT [Type]
          | ArrayT Type
         -- | UArrayT Type
  deriving (Show, Eq)

type Ident = String

type Arg = (Type, Ident)

data Pattern = Ident Ident
             | TouplePat [Pattern]
  deriving (Show, Eq)

data Constant = Int Integer
              | Float Float
              | Char Char
              | Bool Bool
              | ArrayConstant [Constant]
  deriving (Show, Eq)

data Kernel = Fn Type [Arg] Exp
              | Fun Ident [Exp]
              | Op Operator
  deriving (Show, Eq)

data Operator = Plus | Mult 
  deriving (Show, Eq)

data Exp = Var Ident
         | Let Pattern Exp Exp
         | Constant Constant
         | Index Exp [Exp]
         | Neg Exp
         | Array [Exp]
         | BinApp Operator Exp Exp
         | FunCall Ident [Exp]
         | Map Kernel Exp
         | Filter Kernel Exp
         | Scan Kernel Exp Exp
         | Reduce Kernel Exp Exp
  deriving (Show, Eq)
