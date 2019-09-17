--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Common
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Common where

--------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import           Data.Char (toLower, isUpper)
import           Data.Foldable (foldl')
import           Data.Int (Int32)
import           Data.List (stripPrefix)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import qualified Database.EventStore as ES
import           Database.EventStore.Internal.Test (Credentials(..))

--------------------------------------------------------------------------------
import Harbinger.Command

--------------------------------------------------------------------------------
data User =
  User
  { userLogin :: ByteString
  , userPassword :: ByteString
  }

--------------------------------------------------------------------------------
data Batch t =
  Batch
  { batchStream :: ES.StreamId t
  , batchDirection :: ES.ReadDirection
  , batchStart :: t
  , batchCount :: Maybe Int32
  }

---------------------------------------------------------------------------------
fromSetts :: Setts -> ES.Settings
fromSetts setts = go seed
  where
    seed =
      ES.defaultSettings
      { ES.s_heartbeatInterval = settsHeartbeatInterval setts
      , ES.s_heartbeatTimeout = settsHeartbeatTimeout setts
      , ES.s_defaultUserCredentials = makeCreds setts
      }

    go settings =
      if settsVerbose setts
        then settings { ES.s_loggerType = ES.LogStdout 0
                      , ES.s_loggerFilter = ES.LoggerLevel ES.LevelDebug
                      }
        else settings

--------------------------------------------------------------------------------
makeUser :: Setts -> Maybe User
makeUser = fmap go . makeCreds
  where
    go c =
      User (credLogin c) (credPassword c)

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

--------------------------------------------------------------------------------
createConnection :: Setts -> IO ES.Connection
createConnection = createConnectionWith id

--------------------------------------------------------------------------------
createConnectionWith :: (ES.Settings -> ES.Settings)
                     -> Setts
                     -> IO ES.Connection
createConnectionWith k setts = ES.connect (k $ fromSetts setts) tpe
  where
    hosts@(host :| rest) = settsHosts setts
    ports@(port :| _) = settsHttpPorts setts

    hostLen = length hosts
    portLen = length ports

    tpe =
      if hostLen == 1 && portLen == 1
        then
          ES.Static host (settsTcpPort setts)
        else
          ES.Cluster $ ES.gossipSeedClusterSettings makeGossipSeeds

    makeGossipSeeds
      | hostLen == portLen = NonEmpty.zipWith ES.gossipSeed hosts ports
      | hostLen == 1 = fmap (ES.gossipSeed host) ports
      | portLen == 1 = fmap (flip ES.gossipSeed port) hosts
      -- FIXME - Don't use non total function!
      | otherwise = error "Unsupported host/port cluster configuration."

--------------------------------------------------------------------------------
-- | Strips the given prefix from a field name and if the remaining string
--   starts with an upper letter, it will map it to its lower counterpart.
--
--   @stripFieldWith "_foo" "_fooField" == "field"@
--   @stripFieldWith "_foo" "Field" == "Field"@
stripFieldWith :: String -> String -> String
stripFieldWith prefix = go
  where
    go field =
        case stripPrefix prefix field of
            Just rest ->
                case rest of
                    (x:xs)
                        | isUpper x -> toLower x : xs
                        | otherwise -> rest
                    _ -> rest
            _ -> field

--------------------------------------------------------------------------------
-- | Makes the first character lower-case
toLowercase :: String -> String
toLowercase (x : xs) = toLower x : xs
toLowercase xs = xs
