--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.CheckConnection
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.CheckConnection (run) where

--------------------------------------------------------------------------------
import           Control.Exception.Safe (try)
import qualified Control.Concurrent.Async as Async
import qualified Database.EventStore as ES
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

--------------------------------------------------------------------------------
import Harbinger.Command

--------------------------------------------------------------------------------
run :: Setts -> IO ()
run setts = do
  let port = settsTcpPort setts
      host = settsHost setts

  conn <- createConnection setts
  outcome <- try . Async.wait =<<
    ES.readEventsForward conn ES.All ES.positionStart 1 ES.NoResolveLink Nothing

  if validCheckOutcome outcome
    then
      Text.putStrLn
        [i|Successfully connected to node #{host}:#{port} through its public TCP port.|]
    else
      Text.putStrLn
        [i|Failed to connect node #{host}:#{port} through its public TCP port.|]

--------------------------------------------------------------------------------
validCheckOutcome :: Either ES.OperationError a -> Bool
validCheckOutcome = \case
  Left e ->
    case e of
      ES.AccessDenied{} -> True
      _                 -> False
  Right _ -> True

--------------------------------------------------------------------------------
createConnection :: Setts -> IO ES.Connection
createConnection setts = ES.connect settings tpe
  where
    tpe = ES.Static (settsHost setts) (settsTcpPort setts)

    settings =
      ES.defaultSettings
      { ES.s_retry = ES.atMost 0 }
