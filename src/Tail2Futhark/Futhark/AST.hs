module Tail2Futhark.Futhark.AST where

type Program = [FunDecl]

type FunDecl = (Type, Ident, [Arg], Exp)

data Type = IntT
          | RealT
          | BoolT
          | CharT
          | ToupleT [Type]
          | ArrayT Type
          | UArrayT Type
  deriving (Show, Eq)

type Ident = String

type Arg = (Ident, Type)

data Pattern = Ident Ident
             | TouplePat [Pattern]
  deriving (Show, Eq)

data Exp = Var Ident
         | Let Pattern Exp Exp
  deriving (Show, Eq)
