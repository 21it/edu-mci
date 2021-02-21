module EduMci.Class
  ( EnvM (..),
  )
where

import EduMci.Data.Env (Env (..))
import EduMci.Import.External

class MonadIO m => EnvM m where
  getEnv :: m Env
