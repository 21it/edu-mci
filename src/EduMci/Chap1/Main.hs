module EduMci.Chap1.Main
  ( maxargs,
    interp,
    St (..),
    Stdout (..),
    Line (..),
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import EduMci.Chap1.Prog1_5
import EduMci.Import.External

newtype Line
  = Line [Int]
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid
    )

newtype Stdout
  = Stdout [Line]
  deriving newtype
    ( Eq,
      Ord,
      Show,
      Semigroup,
      Monoid
    )

data St
  = St
      { stTable :: Map Id Int,
        stStdout :: Stdout
      }
  deriving (Eq, Ord, Show)

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
  OpExp l _ r -> max (maxargsE l) (maxargsE r)
  EseqExp stm expr -> max (maxargs stm) (maxargsE expr)

interp :: Stm -> St
interp = interpStm $ St mempty mempty

interpStm :: St -> Stm -> St
interpStm st0 = \case
  CompoundStm stm0 stm1 ->
    interpStm (interpStm st0 stm0) stm1
  AssignStm k expr ->
    case Map.lookup k table of
      Nothing ->
        let (v, st1) = interpExp st0 expr
         in st1 {stTable = Map.insert k v table}
      Just v ->
        error $
          (show k :: Text)
            <> " is already set to "
            <> show v
            <> ", can not set to "
            <> show expr
  PrintStm exprs ->
    let (line, st1) = foldl printExpr (mempty, st0) exprs
     in st1 {stStdout = stStdout st1 <> Stdout [line]}
  where
    table = stTable st0

interpExp :: St -> Exp -> (Int, St)
interpExp st0 = \case
  IdExp k ->
    case Map.lookup k table of
      Nothing -> error $ (show k :: Text) <> " is not defined"
      Just v -> (v, st0)
  NumExp x -> (x, st0)
  OpExp l op r -> interpOp st0 l op r
  EseqExp stm expr -> interpExp (interpStm st0 stm) expr
  where
    table = stTable st0

interpOp :: St -> Exp -> BinOp -> Exp -> (Int, St)
interpOp st0 l op r =
  (f lv rv, st2)
  where
    (lv, st1) = interpExp st0 l
    (rv, st2) = interpExp st1 r
    f = case op of
      Plus -> (+)
      Minus -> (-)
      Times -> (*)
      Div -> div

printExpr :: (Line, St) -> Exp -> (Line, St)
printExpr (line, st0) expr =
  (line <> Line [v], st1)
  where
    (v, st1) = interpExp st0 expr
