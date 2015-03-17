module Tail2Futhark.Futhark.Pretty (prettyPrint)  where

import Text.PrettyPrint
import Tail2Futhark.Futhark.AST

prettyPrint :: Program -> String
prettyPrint = render . vcat . map ppFun 

ppFun :: FunDecl -> Doc
ppFun (tp, ident, args, exp) = 
  text "fun" 
  <+> ppType tp 
  <+> text ident 
  <> (commaList . map ppArg) args 
  <+> equals $+$ nest 2 (ppExp exp)

commaList = parens . hcat . punctuate comma
brackList = brackets . hcat . punctuate comma

ppType :: Type -> Doc
ppType IntT = text "int"
ppType RealT = text "real"
ppType BoolT = text "bool"
ppType CharT = text "char"
ppType (ArrayT at) = brackets (ppType at)

ppExp (Var ident) = text ident
ppExp (Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" $+$ ppExp exp2
ppExp (Constant c) = ppConstant c
ppExp (Neg exp)    = text "-" <> ppExp exp
ppExp (Index exp exps) = ppExp exp <> brackList (map ppExp exps)
ppExp (Array exps) = brackets . hcat . punctuate comma . map ppExp $ exps
ppExp (BinApp op e1 e2) = ppExp e1 <+> ppOp op <+> ppExp e2
ppExp (FunCall ident exps) = text ident <> (commaList . map ppExp $ exps)
ppExp e = case e of
  Map k e         -> pp1 "map" k e
  Filter k e      -> pp1 "filter" k e
  Scan k e1 e2    -> pp2 "scan" k e1 e2
  Reduce k e1 e2  -> pp2 "reduce" k e1 e2
  where pp1 id k e     = text id <> parens ((ppKernel k) <> comma <> ppExp e)
        pp2 id k e1 e2 = text id <> parens ((ppKernel k) <> comma <> ppExp e1 <> comma <> ppExp e2)

ppKernel (Fn tp args exp) = text "fn" <+> ppType tp <+> (commaList . map ppArg $ args) <+> text "=>" <+> ppExp exp
ppKernel (Fun ident []) = text ident
ppKernel (Fun ident exps) = text ident <+> (commaList . map ppExp $ exps)
ppKernel (Op op) = text "op" <+> ppOp op

ppOp op = text $ case op of 
  Plus -> "+"

ppConstant (Int int) = integer int
ppConstant (Float f) = float f
ppConstant (Char c) = quotes $ char c
ppConstant (Bool b) = text (if b then "true" else "false")
ppConstant (ArrayConstant arr) = braces . hcat . punctuate comma . map ppConstant $ arr

-- Arguments --
ppArg (tp,ident) = ppType tp <+> text ident

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = braces . hcat . punctuate comma . map ppPat $ pat 
