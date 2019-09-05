{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.ListEvents
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.ListEvents where

--------------------------------------------------------------------------------
import qualified Control.Concurrent.Async as Async
import           Control.Monad.Except
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import           Data.Int (Int32)
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
toBatch :: EventListingArgs -> Batch ES.EventNumber
toBatch args =
  Batch
  { batchStream = streamName
  , batchDirection = direction
  , batchStart = start
  , batchCount = count
  }

  where
    streamName = ES.StreamName (eventListingArgsStream args)

    start =
      if recent
        then ES.streamEnd
        else ES.streamStart

    count =
      if recent
        then Just 50
        else Nothing

    direction =
      if recent
        then ES.Backward
        else ES.Forward

    recent = eventListingArgsRecent args

--------------------------------------------------------------------------------
run :: Setts -> EventListingArgs -> IO ()
run setts args = do
  conn <- createConnection setts
  let batch = toBatch args
      stream =
        handleError batch
          $ Streaming.mapMaybe ES.resolvedEventRecord
          $ buildSource conn (toBatch args)

  Streaming.mapM_ formatEvent stream

--------------------------------------------------------------------------------
formatEvent :: ES.RecordedEvent -> IO ()
formatEvent e = do
  let Just (value :: Aeson.Value) = Aeson.decodeStrict (ES.recordedEventData e)

  Text.putStrLn "--------------------------------------------"
  Text.putStrLn [i|EventNumber: #{ES.recordedEventNumber e} |]
  Text.putStrLn [i|EventId: #{ES.recordedEventId e} |]
  Text.putStrLn [i|Type: #{ES.recordedEventType e} |]
  Text.putStrLn [i|Stream: #{ES.recordedEventStreamId e} |]
  Text.putStrLn [i|Created: #{ES.recordedEventCreated e} |]
  Text.putStrLn "Payload:"
  LazyChar8.putStrLn (Pretty.encodePretty value)
  Text.putStrLn ""

--------------------------------------------------------------------------------
buildSource :: ES.Connection
            -> Batch t
            -> Streaming.Stream (Streaming.Of ES.ResolvedEvent) (ExceptT (ESStream.ReadError t) IO) ()
buildSource conn b = transform src
  where
    transform =
      case batchCount b of
        -- TODO - Might not be the right thing to do in a the future
        -- depending if users wanted to set a specific max count or a different
        -- buffer batch size.
        Just c  -> Streaming.take (fromIntegral c)
        Nothing -> id
    src =
      ESStream.readThrough
        conn
        customReadResultHandler
        (batchDirection b)
        (batchStream b)
        ES.ResolveLink
        (batchStart b)
        (batchCount b)
        Nothing

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
handleError :: forall t a. Show t
            => Batch t
            -> Stream (Of a) (ExceptT (ESStream.ReadError t) IO) ()
            -> Stream (Of a) IO ()
handleError b = hoist go
  where
    go :: ExceptT (ESStream.ReadError t) IO x -> IO x
    go action = do
      runExceptT action >>= \case
        Left e -> do
          case e of
            ESStream.AccessDenied{} ->
              Text.putStrLn
                [i|Access denied: You can't list #{batchStream b} stream with your current user credentials.|]

            ESStream.ReadError causeMay ->
              Text.putStrLn
                [i|Error occured: #{getReason causeMay}.|]

            ESStream.NoStream ->
              Text.putStrLn
                [i|#{batchStream b} stream doesn't exist. Either there is no stream matching that category or you need to enable and start system-projections to be able to list system projection streams.|]

          exitFailure

        Right a -> pure a

    getReason Nothing  = "Unknown"
    getReason (Just e) = e
