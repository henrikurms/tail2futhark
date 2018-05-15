module Tail2Futhark.Futhark.AST where

newtype Program = Program [FunDecl]

-- | Boolean is true if entry point.
data FunDecl = FunDecl Bool Type Ident [TypeSizeParam] [Arg] Exp

data DimDecl = AnyDim | NamedDim Ident | ConstDim Int
             deriving (Show, Eq, Ord)

data Type = IntT
          | Int8T
          | F32T
          | F64T
          | BoolT
          | TupleT [Type]
          | ArrayT Type DimDecl
          | VarT Ident
  deriving (Show, Eq, Ord)

rank :: Num a => Type -> a
rank (ArrayT tp _) = 1 + rank tp
rank _             = 0

baseType :: Type -> Type
baseType (ArrayT tp _) = baseType tp
baseType tp            = tp

type Ident = String

data TypeSizeParam = TypeParam Ident | SizeParam Ident

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

data Operator = Plus | Mult | LessEq | GreaterEq | Less | Greater | Minus | Div | Eq | Mod |
                LogicAnd | LogicOr | Pow | And | Or | Xor | Shl | Shr
  deriving (Show, Eq)

data Exp = Var Ident
         | Ascript Exp Type
         | Let Pattern Exp Exp
         | IfThenElse Exp Exp Exp
         | ForLoop Ident Exp Ident Exp Exp
         | Constant Constant
         | Index Exp [Exp]
         | Neg Exp
         | Array [Exp]
         | Tuple [Exp]
         | Project String Exp
         | BinApp Operator Exp Exp
         | FunCall Exp [Exp]
         | Rearrange [Integer] Exp
         | Unsafe Exp
         | Empty Type
         | Map Exp [Exp]
         | Filter Exp Exp
         | Scan Exp Exp Exp
         | Reduce Exp Exp Exp

         | Fn Type [Arg] Exp
         | Fun Ident [Exp]
         | Op Operator
  deriving (Show, Eq)

