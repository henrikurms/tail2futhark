module APLAcc.TAIL.AST where

type Ident = String

data Rank
  = R Integer
  -- | Rv String          Unsupported
  -- | Radd Rank Rank     Unsupported
  deriving (Show)

data BType = IntT 
           | DoubleT
           | BoolT
           | CharT
           | Btyv Ident
  deriving (Show, Eq)

data Type
  = ArrT BType Rank
  | VecT BType Rank
  | S BType Rank
  | SV BType Rank
--  | FunT Type Type
--  | ShT Rank
--  | SiT Rank
--  | ViT Rank
  deriving (Show)

scalar :: BType -> Type
scalar b = ArrT b (R 0)

int = scalar IntT
double = scalar DoubleT


data Shape = Sh [Integer]
  deriving (Show)

type InstDecl = ([BType], [Integer])

data Exp
  = Var Ident
  | I Integer
  | D Double
  | C Char
  | Inf
  | Neg Exp
  | Let Ident Type Exp Exp
  | Op Ident (Maybe InstDecl) [Exp]
  | Fn Ident Type Exp
  | Vc [Exp]
  deriving (Show)

type Program = Exp
