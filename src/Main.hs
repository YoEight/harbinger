--------------------------------------------------------------------------------
-- |
-- Module    :  Main
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Main where

--------------------------------------------------------------------------------
import Prelude hiding (getArgs)

--------------------------------------------------------------------------------
import           Harbinger.Command
import qualified Harbinger.Command.CheckConnection as CheckConnection
import qualified Harbinger.Command.ListStreams as ListStreams

--------------------------------------------------------------------------------
main :: IO ()
main = getArgs >>= runCommand

--------------------------------------------------------------------------------
runCommand :: Args -> IO ()
runCommand args =
  case argsCommand args of
    CheckConnection -> CheckConnection.run setts
    List cmd ->
      case cmd of
        ListStreams args -> ListStreams.run setts args
  where
    setts = argsSetts args
