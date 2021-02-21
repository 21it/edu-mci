module EduMci.Data.Type
  ( LogFormat (..),
    Message (..),
    ChatEvent (..),
  )
where

import EduMci.Import.External

data LogFormat
  = Bracket
  | JSON
  deriving (Read)

newtype Message = Message Text

data ChatEvent
  = ChatSend Message
  | ChatKeyDown Text
  | ChatRefresh Message
  | ChatTyping Message

