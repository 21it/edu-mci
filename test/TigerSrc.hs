{-# LANGUAGE TemplateHaskell #-}

module TigerSrc
  ( sources,
  )
where

import EduMci.Import.External

sources :: [(FilePath, ByteString)]
sources = $(embedDir "tiger/testcases")
