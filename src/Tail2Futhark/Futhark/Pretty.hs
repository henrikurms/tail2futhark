{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tail2Futhark.Futhark.Pretty (pretty)  where

import Text.PrettyPrint.Mainland hiding (pretty)
import qualified Text.PrettyPrint.Mainland as PP
import Tail2Futhark.Futhark.AST

-- | Prettyprint a value, wrapped to 80 characters.
pretty :: Pretty a => a -> String
pretty = PP.pretty 80 . ppr

instance Pretty Program where
  ppr (Program fundecs) =
    stack . map ppr $ fundecs

instance Pretty FunDecl where
  ppr (FunDecl tp ident args body) =
    text "fun"
    <+> ppr tp
    <+> text ident <> (commaList . map ppArg) args
    <+> equals </>
    indent 2 (ppr body)

commaList :: [Doc] -> Doc
commaList = parens . commasep
ppKernel :: Kernel -> Doc
commaExps :: [Exp] -> Doc
commaExps = commaList . map ppr
brackList :: [Doc] -> Doc
brackList = brackets . commasep
brackExps :: [Exp] -> Doc
brackExps = brackList . map ppr

instance Pretty Type where
  ppr IntT = text "int"
  ppr Int8T = text "i8"
  ppr F32T = text "f32"
  ppr F64T = text "f64"
  ppr BoolT = text "bool"
  ppr (ArrayT at) = brackets (ppr at)
  ppr (TupleT ts) = braces $ commasep $ map ppr ts

instance Pretty Exp where
  ppr (Var ident) = text ident
  ppr (Let Indent pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppr exp1 <+> text "in" </> ppr exp2
  ppr (Let Inline pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppr exp1 <+> text "in" <+> ppr exp2
  ppr (IfThenElse Indent e1 e2 e3) = text "if" <+> ppr e1 </> text "then" <+> ppr e2 </> text "else" <+> ppr e3
  ppr (IfThenElse Inline e1 e2 e3) = text "if" <+> ppr e1 <+> text "then" <+> ppr e2 <+> text "else" <+> ppr e3
  ppr (Constant c) = ppr c
  ppr (Neg e)    = parens $ text "-" <> ppr e
  ppr (Index e exps) = ppr e <> brackExps exps
  ppr (Array exps) = brackExps exps
  ppr (Tuple exps) = braces $ commasep $ map ppr exps
  ppr (BinApp op e1 e2) = parens $ ppr e1 <+> ppOp op <+> ppr e2
  ppr (FunCall ident exps) = text ident <> commaExps exps
  ppr (FunCall2 ident exps e) = text ident <> parens (commaExps exps <> comma <> ppr e)
  --ppr (Reshape exps exp) = text "reshape" <> parens (commaExps exps <> comma <> ppr exp)
  ppr (Empty tp) = text "empty" <> parens (ppr tp)
  ppr (Map k e) = ppSOAC1 "map" k e
  ppr (Power k e1 e2) = ppPower k e1 e2
  ppr (Filter k e) = ppSOAC1 "filter" k e
  ppr (Scan k e1 e2) = ppSOAC2 "scan" k e1 e2
  ppr (Reduce k e1 e2) = ppSOAC2 "reduce" k e1 e2

ppSOAC1 :: String -> Kernel -> Exp -> Doc
ppSOAC1 v k e = text v <> parens (ppKernel k <> comma <> ppr e)

ppSOAC2 :: String -> Kernel -> Exp -> Exp -> Doc
ppSOAC2 v k e1 e2 = text v <> parens (ppKernel k <> comma <> ppr e1 <> comma <> ppr e2)

ppPower :: Kernel -> Exp -> Exp -> Doc
ppPower (Fn _ [(_,var)] body) e1 e2 =
  text "loop" <+> parens (text var <+> text"=" <+> ppr e2) <+> text "=" </>
  text "for" <+> text "i" <+> text "<" <+> (ppr e1) <+> text "do" </>
  ppr body <+> text "in" <+> text var
ppPower (Fn _ _ _) _ _ = error "expecting exactly one argument in function to power-function"
ppPower _ _ _ = error "expecting anonymous function as argument to power-function"

ppKernel (Fn tp args body) =
  text "fn" <+> ppr tp <+> (commaList . map ppArg $ args) <+> text "=>" </>
  ppr body
ppKernel (Fun ident []) = text ident
ppKernel (Fun ident exps) = text ident <+> (commaList . map ppr $ exps)
ppKernel (Op op) = ppOp op

ppOp :: Operator -> Doc
ppOp op = text $ case op of
  Plus -> "+"
  Minus -> "-"
  LessEq -> "<="
  Mult -> "*"
  Div -> "/"
  Eq -> "=="
  Mod -> "%"
  Greater -> ">"
  Less -> "<"
  GreaterEq -> ">="
  LogicAnd -> "&&"
  LogicOr -> "||"
  Pow -> "**"
  Or -> "|"
  Xor -> "^"
  And -> "&"
  Shl -> ">>"
  Shr -> "<<"
  --XOr -> "^"

instance Pretty Constant where
  ppr (Int x) = integer x
  ppr (F32 f) = text (show f) <> text "f32"
  ppr (F64 f) = text (show f) <> text "f64"
  ppr (Char c) = text $ show c
  ppr (Bool b) = text (if b then "True" else "False")
  ppr (ArrayConstant arr) = braces . commasep . map ppr $ arr

-- Arguments --
ppArg :: (Type, String) -> Doc
ppArg (tp,ident) = ppr tp <+> text ident

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = braces . commasep . map ppPat $ pat 
