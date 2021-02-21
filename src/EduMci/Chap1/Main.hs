module EduMci.Chap1.Main (maxargs) where

import EduMci.Chap1.Prog1_5
import EduMci.Import.External

maxargs :: Stm -> Int
maxargs = \case
  CompoundStm stm0 stm1 -> max (maxargs stm0) (maxargs stm1)
  AssignStm _ expr -> maxargsE expr
  PrintStm [] -> 0
  PrintStm exprs ->
    max (length exprs) (maximum $ maxargsE <$> exprs)

maxargsE :: Exp -> Int
maxargsE = \case
  IdExp _ -> 0
  NumExp _ -> 0
  OpExp exp0 _ exp1 -> max (maxargsE exp0) (maxargsE exp1)
  EseqExp stm expr -> max (maxargs stm) (maxargsE expr)
