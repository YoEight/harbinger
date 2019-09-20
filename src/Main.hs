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
import qualified Harbinger.Command.CreateSub as CreateSub
import qualified Harbinger.Command.ListEvents as ListEvents
import qualified Harbinger.Command.ListStreams as ListStreams
import qualified Harbinger.Command.ListSub as ListSub
import qualified Harbinger.Command.ListSubs as ListSubs
import qualified Harbinger.Command.Version as Version

--------------------------------------------------------------------------------
main :: IO ()
main = getArgs >>= runCommand

--------------------------------------------------------------------------------
runCommand :: Args -> IO ()
runCommand args =
  case argsCommand args of
    CheckConnection -> CheckConnection.run setts
    Version -> Version.run
    List cmd ->
      case cmd of
        ListEvents args -> ListEvents.run setts args
        ListStreams args -> ListStreams.run setts args
        ListSub args -> ListSub.run setts args
        ListSubs -> ListSubs.run setts
    Create cmd ->
        case cmd of
          CreateSub args -> CreateSub.run setts args
  where
    setts = argsSetts args
