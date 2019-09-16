{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.ListSub
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.ListSub where

--------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types (Parser, parseEither)
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import           Data.Int (Int64)
import           Data.String.Interpolate.IsString (i)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           System.Exit (exitFailure)

--------------------------------------------------------------------------------
import Harbinger.Command
import Harbinger.Http

----------------------------------------------------------------------------------
data AbridgedInfo =
  AbridgedInfo
  { abridgedInfoGroupName :: Text
  , abridgedInfoStreamId :: Text
  , abridgedInfoStatus :: Text
  , abridgedInfoLastKnownEventNumber :: Int64
  , abridgedInfoLastProcessedEventNumber :: Int64
  , abridgedInfoAverageItemsPerSecond :: Double
  , abridgedInfoConnectionCount :: Int
  } deriving Show

--------------------------------------------------------------------------------
parseAbridgedInfo :: Value -> Parser AbridgedInfo
parseAbridgedInfo = withObject "AbridgedInfo" $ \ o ->
  AbridgedInfo
    <$> o .: "groupName"
    <*> o .: "eventStreamId"
    <*> o .: "status"
    <*> o .: "lastKnownEventNumber"
    <*> o .: "lastProcessedEventNumber"
    <*> o .: "averageItemsPerSecond"
    <*> (parseConnectionCount =<< (o .: "connections"))
  where
    parseConnectionCount = withArray "List of connection info" $ \xs ->
      pure $ length xs

--------------------------------------------------------------------------------
run :: Setts -> SubListingArgs -> IO ()
run setts args = do
  env <- makeClient setts

  let stream = subListingArgsStream args
      groupId = subListingArgsGroup args

  getSubscriptionInfo env stream groupId >>= \case
    Left _ -> do
      Text.putStrLn [i|Your current user is not allowed to read subscription information.|]
      exitFailure

    Right opt ->
      case opt of
        Nothing -> do
          Text.putStrLn
            [i|There is no persistent subscription for the stream [#{stream}]
            and group [#{groupId}.]|]

          exitFailure

        Just dat ->
          if subListingArgsDetailed args
            then LazyChar8.putStrLn (Pretty.encodePretty dat)
            else printAbridgedInfo dat

--------------------------------------------------------------------------------
printAbridgedInfo :: Value -> IO ()
printAbridgedInfo json =
  case parseEither parseAbridgedInfo json of
    Left msg ->
      putStrLn
        [i|ERROR when producing abridged version output of persistent
        subscription listing: #{msg}. Please submit an issue and append
        --detailed in the meantime|]

    Right info -> do
      let processDiff = abridgedInfoLastKnownEventNumber info - abridgedInfoLastProcessedEventNumber info
      Text.putStrLn "--------------------------------------------"
      Text.putStrLn [i|Stream: #{abridgedInfoStreamId info}|]
      Text.putStrLn [i|Group: #{abridgedInfoGroupName info}|]
      Text.putStrLn [i|Status: #{abridgedInfoStatus info}|]
      Text.putStrLn [i|Connections: #{abridgedInfoConnectionCount info}|]
      Text.putStrLn [i|Processed / Known: #{abridgedInfoLastProcessedEventNumber info} / #{abridgedInfoLastKnownEventNumber info} (#{processDiff})|]
      Text.putStrLn [i|Processing speed: #{abridgedInfoAverageItemsPerSecond info} msgs/sec|]
