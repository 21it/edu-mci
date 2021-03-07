module EduMci.Chap2.Driver
  ( scanner,
  )
where

import EduMci.Chap2.Lexer
  ( Alex,
    AlexUserState (..),
    Lexeme (..),
    Token (..),
    alexError,
    alexGetUserState,
    alexMonadScan,
    runAlex,
  )
import EduMci.Import.External

scanner :: String -> Either String [Lexeme]
scanner x = runAlex x scannerLoop

scannerLoop :: Alex [Lexeme]
scannerLoop = do
  lex <- alexMonadScan
  if lexToken lex == EOF
    then do
      s0 <- alexGetUserState
      case s0 of
        AlexUserStateVoid -> pure [lex]
        AlexUserStateString {} -> alexError "EOF: string not closed"
        AlexUserStateComment {} -> alexError "EOF: comment not closed"
    else do
      lexs <- scannerLoop
      pure $ lex : lexs
