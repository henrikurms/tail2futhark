module APLAcc.SimpleAcc.AST (
  module APLAcc.SimpleAcc.AST,
  T.BType(..)  -- reexport
) where

import qualified APLAcc.TAIL.AST as T

data Type
  = Exp T.BType         -- Exp t
  | Acc Integer T.BType -- Acc (Array n t)
  | Plain T.BType
  deriving (Eq)

baseType :: Type -> T.BType
baseType (Exp t) = t
baseType (Acc _ t) = t
baseType (Plain t) = t

data Name
  = Ident T.Ident
  | Symbol String

data QName
  = UnQual Name
  | Prelude Name
  | Accelerate Name
  | Primitive Name

data Exp
  = Var QName
  | I Integer
  | D Double
  | Shape [Integer]
  | Neg Exp
  | TypSig Exp Type
  | List [Exp]
  | InfixApp QName [Exp]       -- x1 `op` x2 `op` …
  | App QName [Exp]            -- op x1 x2 …
  | Let T.Ident Type Exp Exp    -- let x = e1 :: t in e2
  | Fn T.Ident Type Exp         -- \x -> e

type Program = Exp

the x  = App (Accelerate $ Ident "the") [x]
unit x = App (Accelerate $ Ident "unit") [x]
constant x = App (Accelerate $ Ident "constant") [x]
lift x = App (Accelerate $ Ident "lift") [x]
unlift x = App (Accelerate $ Ident "unlift") [x]
i2d x  = App (Primitive  $ Ident "i2d") [x]
fromList n x = App (Accelerate $ Ident "fromList") [Shape [fromIntegral n], x]
use x  = App (Accelerate $ Ident "use") [x]
fromInt x = App (Prelude $ Ident "fromIntegral") [x]
unitvec x = App (Primitive $ Ident "unitvec") [x]
first x = App (Primitive $ Ident "first") [x]
