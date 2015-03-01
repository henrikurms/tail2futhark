module Main where

import System.IO
import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import APLAcc.TAIL.AST as T -- the TAIL AST
import Futhark.AST as F -- the futhark AST
import Text.PrettyPrint

main :: IO ()
main = do
  args <- getArgs
  program <- case args of [] -> parseFile stdin "stdin"
                          (f : _) -> withFile f ReadMode $ flip parseFile f
  -- putStrLn $ show program
  putStrLn . render . vcat . map ppFun . compile $ program


compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp e))]

compileExp :: T.Exp -> F.Exp
compileExp (T.Let id t e1 e2) = F.Let (Ident id) (compileExp e1) (compileExp e2)
compileExp rest = F.Var $ show rest -- catch all

pretty :: F.Program -> String
pretty p = render $ ppProgram p

ppProgram :: F.Program -> Doc
ppProgram p = text . show $ p

ppFun :: FunDecl -> Doc
ppFun (tp, ident, args, exp) = text "fun" 
                               <+> ppType tp 
                               <+> text ident 
                               <> (parens . hcat . punctuate comma . map ppArg) args 
                               <+> equals $+$ nest 2 (ppExp exp)

ppType tp = text . show $ tp

ppExp (F.Let pat exp1 exp2) = text "let" <+> ppPat pat <+> equals <+> ppExp exp1 <+> text "in" $+$ ppExp exp2
ppExp rest = text . show $ rest

ppArg arg = text . show $ arg

ppPat p = text . show $ p
