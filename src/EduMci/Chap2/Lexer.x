{

{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module EduMci.Chap2.Lexer (
  Lexeme (..),
  AlexUserState (..),
  Token (..),
  Alex,
  runAlex,
  alexMonadScan,
  alexGetUserState,
  alexSetUserState,
  alexError
) where
import EduMci.Import.External hiding (EQ, GT, LT)

}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
@int = $digit+
@id = $alpha($alpha|$digit|_)*

tokens :-

<0> $white+          ;
<0> "type"           {newLex TYPE}
<0> "var"            {newLex VAR}
<0> "function"       {newLex FUNCTION}
<0> "break"          {newLex BREAK}
<0> "of"             {newLex OF}
<0> "end"            {newLex END}
<0> "in"             {newLex IN}
<0> "nil"            {newLex NIL}
<0> "let"            {newLex LET}
<0> "do"             {newLex DO}
<0> "to"             {newLex TO}
<0> "for"            {newLex FOR}
<0> "while"          {newLex WHILE}
<0> "else"           {newLex ELSE}
<0> "then"           {newLex THEN}
<0> "if"             {newLex IF}
<0> "array"          {newLex ARRAY}
<0> "assign"         {newLex ASSIGN}
<0> "|"              {newLex OR}
<0> "&"              {newLex AND}
<0> ">="             {newLex GE}
<0> ">"              {newLex GT}
<0> "<="             {newLex LE}
<0> "<"              {newLex LT}
<0> "<>"             {newLex NEQ}
<0> "="              {newLex EQ}
<0> "/"              {newLex DIVIDE}
<0> "*"              {newLex TIMES}
<0> "-"              {newLex MINUS}
<0> "+"              {newLex PLUS}
<0> "."              {newLex DOT}
<0> "}"              {newLex RBRACE}
<0> "{"              {newLex LBRACE}
<0> "]"              {newLex RBRACK}
<0> "["              {newLex LBRACK}
<0> ")"              {newLex RPAREN}
<0> "("              {newLex LPAREN}
<0> ";"              {newLex SEMICOLON}
<0> ":"              {newLex COLON}
<0> ","              {newLex COMMA}
<0> @int             {newInt}
<0> "/*"             {pushComment `andBegin` state_comment}
<state_comment> "/*" {pushComment}
<state_comment> "*/" {popComment}
<state_comment> .    ;
<state_comment> \n   ;

--<0> "\""       {newLex STRING String}
--<0> @id        {newLex ID String}

{

data AlexUserState =
    AlexUserStateVoid
  | AlexUserStateString String
  | AlexUserStateComment Int
  deriving (Eq, Show)

data Lexeme =
  Lexeme {
    lexPosn :: AlexPosn,
    lexToken :: Token,
    lexRaw :: Maybe String
  } deriving (Eq, Show)

data Token =
    TYPE
  | VAR
  | FUNCTION
  | BREAK
  | OF
  | END
  | IN
  | NIL
  | LET
  | DO
  | TO
  | FOR
  | WHILE
  | ELSE
  | THEN
  | IF
  | ARRAY
  | ASSIGN
  | OR
  | AND
  | GE
  | GT
  | LE
  | LT
  | NEQ
  | EQ
  | DIVIDE
  | TIMES
  | MINUS
  | PLUS
  | DOT
  | RBRACE
  | LBRACE
  | RBRACK
  | LBRACK
  | RPAREN
  | LPAREN
  | SEMICOLON
  | COLON
  | COMMA
  | STRING String
  | INT Int
  | ID String
  | EOF
    deriving (Eq, Show)

state_initial :: Int
state_initial = 0

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserStateVoid

alexEOF :: Alex Lexeme
alexEOF =
  pure $ Lexeme {
    lexPosn = undefined,
    lexToken = EOF,
    lexRaw = Nothing
  }

newLex :: Token -> AlexInput -> Int -> Alex Lexeme
newLex tok (pos, _, _, input) len =
  pure $ Lexeme {
    lexPosn = pos,
    lexToken = tok,
    lexRaw = Just (take len input)
  }

newInt :: AlexInput -> Int -> Alex Lexeme
newInt (pos, _, _, input) len =
  case readMaybe raw of
    Just x ->
      pure $ Lexeme {
        lexPosn = pos,
        lexToken = INT x,
        lexRaw = Just raw
      }
    Nothing ->
      alexError $
        "Can not parse INT from "
          <> raw
          <> " on "
          <> show pos
  where
    raw = take len input

pushComment :: AlexInput -> Int -> Alex Lexeme
pushComment input len = do
  st <- alexGetUserState
  case st of
    AlexUserStateVoid ->
      alexSetUserState $ AlexUserStateComment 1
    AlexUserStateString _ ->
      failure st
    AlexUserStateComment x ->
      alexSetUserState $ AlexUserStateComment $ x + 1
  skip input len
  where
    failure st =
      alexError $
        "Unexpected pushComment input "
          <> show input
          <> " not compatible with "
          <> show st

popComment :: AlexInput -> Int -> Alex Lexeme
popComment input len = do
  st <- alexGetUserState
  case st of
    AlexUserStateVoid -> failure st
    AlexUserStateString _ -> failure st
    AlexUserStateComment x0 ->
      case x0 - 1 of
        x | x < 0 ->
          failure st
        0 -> do
          alexSetStartCode state_initial
          alexSetUserState AlexUserStateVoid
        x ->
          alexSetUserState $ AlexUserStateComment x
  skip input len
  where
    failure st =
      alexError $
        "Unexpected popComment input "
          <> show input
          <> " not compatible with "
          <> show st

}
