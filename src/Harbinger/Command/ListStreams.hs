--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.ListStreams
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.ListStreams where

--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
import           Data.Foldable (foldl')
import           Data.Maybe (fromMaybe)
import qualified Database.EventStore as ES
import           Database.EventStore.Internal.Test (Credentials(..))
import qualified Database.EventStore.Streaming as ESStream
import           Data.String.Interpolate.IsString (i)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Streaming
import qualified Streaming.Prelude as Streaming
import           System.Exit (exitFailure)

--------------------------------------------------------------------------------
import Harbinger.Command

--------------------------------------------------------------------------------
run :: Setts -> ListStreamsArgs -> IO ()
run setts args = do
  conn <- createConnection setts
  let stream =
        handleError
          $ Streaming.mapMaybe getStreamName
          $ ESStream.readThrough conn customReadResultHandler ES.Forward (ES.StreamName "$streams") ES.ResolveLink ES.streamStart (Just 4_096) Nothing

  Streaming.print stream

--------------------------------------------------------------------------------
getStreamName :: ES.ResolvedEvent -> Maybe Text
getStreamName resolved = do
  evt <- ES.resolvedEventRecord resolved
  let name = ES.recordedEventStreamId evt

  when (Text.isPrefixOf "$" name)
    Nothing

  pure name

--------------------------------------------------------------------------------
customReadResultHandler :: ESStream.ReadResultHandler
customReadResultHandler = ESStream.onRegularStream go
  where
    go ES.ReadNoStream          = ESStream.FetchError ESStream.NoStream
    go ES.ReadNotModified       = ESStream.Fetch ES.emptySlice
    go (ES.ReadStreamDeleted n) = ESStream.FetchError (ESStream.StreamDeleted n)
    go (ES.ReadError e)         = ESStream.FetchError (ESStream.ReadError e)
    go (ES.ReadAccessDenied n)  = ESStream.FetchError (ESStream.AccessDenied n)
    go (ES.ReadSuccess s)       = ESStream.Fetch s

--------------------------------------------------------------------------------
unwantedEvents :: ES.ResolvedEvent -> Bool
unwantedEvents resolved = not (Text.isPrefixOf "$" (ES.recordedEventType event))
  where
    event = ES.resolvedEventOriginal resolved

--------------------------------------------------------------------------------
createConnection :: Setts -> IO ES.Connection
createConnection setts = ES.connect settings tpe
  where
    tpe = ES.Static (settsHost setts) (settsTcpPort setts)

    settings =
      ES.defaultSettings
      { ES.s_defaultUserCredentials = makeCreds setts
      , ES.s_heartbeatInterval = ES.msDiffTime 3_000
      , ES.s_heartbeatTimeout = ES.msDiffTime 6_000
      }

--------------------------------------------------------------------------------
handleError :: Show t => Stream (Of a) (ExceptT (ESStream.ReadError t) IO) ()
            -> Stream (Of a) IO ()
handleError = hoist go
  where
    go action = do
      runExceptT action >>= \case
        Left e -> do
          case e of
            ESStream.AccessDenied{} ->
              Text.putStrLn
                "Access denied: You can't list streams with your current user credentials."

            ESStream.ReadError causeMay ->
              Text.putStrLn
                [i|Error occured: #{getReason causeMay}.|]

            ESStream.NoStream ->
              Text.putStrLn
                "$streams projection doesn't exist. Enable and start system projections to be able to list streams."

          exitFailure

        Right a -> pure a

    getReason Nothing  = "Unknown"
    getReason (Just e) = e

--------------------------------------------------------------------------------
makeCreds :: Setts -> Maybe ES.Credentials
makeCreds setts = foldl' go Nothing [0 :: Int,1]
  where
    go acc tpe =
      let creds = fromMaybe (ES.credentials "" "") acc in
      case tpe of
        0 ->
          case settsLogin setts of
            Nothing    -> acc
            Just login -> Just creds { credLogin = login }
        _ ->
          case settsPassword setts of
            Nothing  -> acc
            Just pwd -> Just creds { credPassword = pwd }
