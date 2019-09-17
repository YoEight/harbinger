{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.Version
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.Version where

--------------------------------------------------------------------------------
import           Development.GitRev (gitHash)
import           Distribution.PackageDescription.TH (PackageIdentifier(..), PackageDescription(..), packageVariable)
import           Data.Text (Text)
import qualified Data.Text.IO as Text

--------------------------------------------------------------------------------
run :: IO ()
run = Text.putStrLn gitVersionString

--------------------------------------------------------------------------------
gitVersionString :: Text
gitVersionString =
  mconcat [ "Version "
          , packageVersion
          , ", Git revision "
          , $(gitHash)
          ]

--------------------------------------------------------------------------------
packageVersion :: Text
packageVersion = $(packageVariable (pkgVersion . package))
