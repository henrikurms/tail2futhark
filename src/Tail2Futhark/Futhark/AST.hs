module Tail2Futhark.Futhark.AST where

newtype Program = Program [FunDecl]

data FunDecl = FunDecl Type Ident [Arg] Exp

data Type = IntT
          | Int8T
          | F32T
          | F64T
          | BoolT
          | TupleT [Type]
          | ArrayT Type
         -- | UArrayT Type
  deriving (Show, Eq)

rank :: Num a => Type -> a
rank (ArrayT tp) = 1 + rank tp
rank _ = 0

baseType :: Type -> Type
baseType (ArrayT tp) = baseType tp
baseType tp = tp

type Ident = String

type Arg = (Type, Ident)

data Pattern = Ident Ident
             | TouplePat [Pattern]
  deriving (Show, Eq)

data Constant = Int Integer
              | F32 Float
              | F64 Double
              | Char Char
              | Bool Bool
              | ArrayConstant [Constant]
  deriving (Show, Eq)

data Kernel = Fn Type [Arg] Exp
              | Fun Ident [Exp]
              | Op Operator
  deriving (Show, Eq)

data Operator = Plus | Mult | LessEq | GreaterEq | Less | Greater | Minus | Div | Eq | Mod |
                LogicAnd | LogicOr | Pow | And | Or | Xor | Shl | Shr
  deriving (Show, Eq)

data Exp = Var Ident
         | Let Pattern Exp Exp
         | IfThenElse Exp Exp Exp
         | Constant Constant
         | Index Exp [Exp]
         | Neg Exp
         | Array [Exp]
         | Tuple [Exp]
         | BinApp Operator Exp Exp
         | FunCall Ident [Exp]
         | FunCall2 Ident [Exp] Exp -- special case for FunCalls with paranthese list of args
         -- | Reshape [Exp] Exp -- "old" version of impl
         | Empty Type
         | Map Kernel Exp
         | Power Kernel Exp Exp
         | Filter Kernel Exp
         | Scan Kernel Exp Exp
         | Reduce Kernel Exp Exp
  deriving (Show, Eq)

