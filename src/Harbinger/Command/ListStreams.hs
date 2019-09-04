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
import Harbinger.Common

--------------------------------------------------------------------------------
run :: Setts -> ListStreamsArgs -> IO ()
run setts args = do
  conn <- createConnection setts
  let stream =
        handleError
          $ Streaming.mapMaybe (getStreamName args)
          $ source conn

  Streaming.mapM_ Text.putStrLn stream
  where
    source conn =
      if listStreamArgsRecent args
        then
          Streaming.take 50 $
          ESStream.readThrough conn customReadResultHandler ES.Backward (ES.StreamName "$streams") ES.ResolveLink ES.streamEnd (Just 50) Nothing
        else
          ESStream.readThrough conn customReadResultHandler ES.Forward (ES.StreamName "$streams") ES.ResolveLink ES.streamStart (Just 4_096) Nothing

--------------------------------------------------------------------------------
getStreamName :: ListStreamsArgs -> ES.ResolvedEvent -> Maybe Text
getStreamName args resolved = do
  evt <- ES.resolvedEventRecord resolved
  let name = ES.recordedEventStreamId evt
      predicate =
        case listStreamArgsTarget args of
          UserStreams -> not . Text.isPrefixOf "$"
          SystemStreams -> Text.isPrefixOf "$"
          AllStreams -> const True

  unless (predicate name)
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
