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
import           Data.Foldable (foldl')
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (fromMaybe)
import qualified Database.EventStore as ES
import           Database.EventStore.Internal.Test (Credentials(..))

--------------------------------------------------------------------------------
import Harbinger.Command

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
      case rest of
        [] ->
          ES.Static host (settsTcpPort setts)
        _  ->
          ES.Cluster $ ES.gossipSeedClusterSettings makeGossipSeeds

    makeGossipSeeds
      | hostLen == portLen = NonEmpty.zipWith ES.gossipSeed hosts ports
      | hostLen == 1 = fmap (ES.gossipSeed host) ports
      | portLen == 1 = fmap (flip ES.gossipSeed port) hosts
      -- FIXME - Don't use non total function!
      | otherwise = error "Unsupported host/port cluster configuration."

