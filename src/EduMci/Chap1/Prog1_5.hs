module EduMci.Chap1.Prog1_5
  ( Id (..),
    BinOp (..),
    Stm (..),
    Exp (..),
    prog,
  )
where

import EduMci.Import.External

newtype Id = Id String
  deriving (Eq, Ord, Show, IsString)

data BinOp = Plus | Minus | Times

data Stm
  = CompoundStm Stm Stm
  | AssignStm Id Exp
  | PrintStm [Exp]

data Exp
  = IdExp Id
  | NumExp Int
  | OpExp Exp BinOp Exp
  | EseqExp Stm Exp

--
-- Example prog:
--
--   a := 5 + 3; b := (print (a, a-1), 10 * a); print (b)
--
prog :: Stm
prog =
  CompoundStm
    (AssignStm "a" $ OpExp (NumExp 5) Plus (NumExp 3))
    ( CompoundStm
        ( AssignStm "b" $
            EseqExp
              ( PrintStm
                  [ IdExp "a",
                    OpExp (IdExp "a") Minus (NumExp 1)
                  ]
              )
              (OpExp (NumExp 10) Times (IdExp "a"))
        )
        (PrintStm [IdExp "b"])
    )
