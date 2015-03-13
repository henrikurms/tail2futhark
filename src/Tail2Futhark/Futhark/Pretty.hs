module Tail2Futhark.Futhark.Pretty (prettyPrint)  where

import Text.PrettyPrint
import Tail2Futhark.Futhark.AST

prettyPrint :: Program -> String
prettyPrint = render . vcat . map ppFun 

ppFun :: FunDecl -> Doc
ppFun (tp, ident, args, exp) = text "fun" 
                               <+> ppType tp 
                               <+> text ident 
                               <> (parens . hcat . punctuate comma . map ppArg) args 
                               <+> equals $+$ nest 2 (ppExp exp)

ppType :: Type -> Doc
ppType IntT = text "int"
ppType RealT = text "real"
ppType BoolT = text "bool"
ppType CharT = text "char"
ppType tp = error $ "type " ++ show tp ++ "not supported in pretty"

ppExp (Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" $+$ ppExp exp2
ppExp (Array exps) = brackets . hcat . punctuate comma . map ppExp $ exps
ppExp (Constant c) = ppConstant c
ppExp (Var ident) = text ident
ppExp (Reduce k e1 e2) = text "reduce" <> parens ((ppKernel k) <> comma <> ppExp e1 <> comma <> ppExp e2)
ppExp (Map k e) = text "map" <> parens ((ppKernel k) <> comma <> ppExp e)
ppExp rest = text . show $ rest

ppKernel (Fn tp args exp) = text "fn" <+> ppType tp <+> (parens . hcat . punctuate comma . map ppArg $ args) <+> text "=>" <+> ppExp exp
ppKernel (Fun ident []) = text ident
ppKernel (Fun ident exps) = text ident <+> (parens . hcat . punctuate comma . map ppExp $ exps)
ppKernel (Op op) = text "op" <+> ppOp op

ppOp Plus = text "+"

ppConstant (Int int) = integer int
ppConstant (Float f) = float f
ppConstant (Char c) = quotes $ char c
ppConstant (ArrayConstant arr) = braces . hcat . punctuate comma . map ppConstant $ arr
--ppConstant rest = text . show $ rest

-- Arguments --
ppArg (ident,tp) = text ident <+> ppType tp

-- Pattern --
ppPat :: Pattern -> Doc
ppPat (Ident ident) = text ident
ppPat (TouplePat pat) = braces . hcat . punctuate comma . map ppPat $ pat 
