--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.CreateSub
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.CreateSub where

--------------------------------------------------------------------------------
import           Control.Exception.Safe (try)
import qualified Control.Concurrent.Async as Async
import qualified Database.EventStore as ES
import           Data.List.NonEmpty (NonEmpty(..), toList)
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text.IO as Text
import           System.Exit (exitFailure)

--------------------------------------------------------------------------------
import Harbinger.Command
import Harbinger.Common

--------------------------------------------------------------------------------
run :: Setts -> SubCreateArgs -> IO ()
run setts args = do
  conn <- createConnection setts
  outcome <- Async.wait =<< ES.createPersistentSubscription conn
    (subCreateArgsGroup args)
    (ES.StreamName $ subCreateArgsStreamName args)
    (subCreateArgsSettings args)
    Nothing

  case outcome of
    Just ex -> do
      case ex of
        ES.PersistActionFail ->
          Text.putStrLn "Failed to create persistent subscription, Unfortunately, we don't have much information to give. Might want to checkout database logs."

        ES.PersistActionAlreadyExist ->
          Text.putStrLn [i|A persistent subscription on stream [#{subCreateArgsStreamName args}] with [#{subCreateArgsGroup args}] as a group exists already.|]

        ES.PersistActionAccessDenied ->
          Text.putStrLn "Your current user is not allowed to create a persistent subscription."

        ES.PersistActionAborted ->
          Text.putStrLn "create persistent subscription operation has been aborted. This is unexpected error case. Please create an issue on GitHub."

        _ ->
          -- Impossible situation.
          pure ()

      exitFailure

    Nothing -> pure ()
