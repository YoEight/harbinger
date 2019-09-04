{-# LANGUAGE QuasiQuotes #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command where

--------------------------------------------------------------------------------
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.String (fromString)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.EventStore (msDiffTime)
import Options.Applicative
import Safe (readMay)

--------------------------------------------------------------------------------
data Setts =
  Setts
  { settsHosts :: NonEmpty String
  , settsTcpPort :: Int
  , settsHttpPorts :: NonEmpty Int
  , settsLogin :: Maybe ByteString
  , settsPassword :: Maybe ByteString
  , settsHeartbeatInterval :: NominalDiffTime
  , settsHeartbeatTimeout :: NominalDiffTime
  , settsVerbose :: Bool
  } deriving Show

-------------------------------------------------------------------------------
data Args =
  Args
  { argsSetts   :: Setts
  , argsCommand :: Command
  } deriving Show

--------------------------------------------------------------------------------
data Command
  = CheckConnection
  | List ListCommand
  deriving Show

--------------------------------------------------------------------------------
data ListCommand
  = ListStreams ListStreamsArgs
  deriving Show

--------------------------------------------------------------------------------
data TargettedStreams
  = UserStreams
  | SystemStreams
  | AllStreams
  deriving (Enum, Bounded)

--------------------------------------------------------------------------------
instance Show TargettedStreams where
  show UserStreams = "user"
  show SystemStreams = "system"
  show AllStreams = "all"

--------------------------------------------------------------------------------
data ListStreamsArgs =
  ListStreamsArgs
  { listStreamArgsTarget :: TargettedStreams
  , listStreamArgsRecent :: Bool
  }
  deriving Show

--------------------------------------------------------------------------------
getArgs :: IO Args
getArgs = execParser go
  where
    go = info (helper <*> parseArgs) description

    description =
      fullDesc <> header "Harbinger - An EventStore administration tool."

--------------------------------------------------------------------------------
parseArgs :: Parser Args
parseArgs =
  Args <$> parseSetts
       <*> parseCommand

--------------------------------------------------------------------------------
parseSetts :: Parser Setts
parseSetts =
  Setts <$> fmap makeHostList (many parseHost)
        <*> parseTcpPort
        <*> fmap makePortList (many parseHttpPort)
        <*> parseLogin
        <*> parsePassword
        <*> parseHeartbeatInterval
        <*> parseHeartbeatTimeout
        <*> parseVerbose
  where
    makeHostList (h:hs) = h :| hs
    makeHostList _ = "localhost" :| []

    makePortList (p:ps) = p :| ps
    makePortList _ = 2113 :| []

--------------------------------------------------------------------------------
parseHost :: Parser String
parseHost = strOption go
  where
    go = mconcat [ long "host"
                 , metavar "HOST"
                 , help "Database host (default: localhost)."
                 ]

--------------------------------------------------------------------------------
parseTcpPort :: Parser Int
parseTcpPort = option (eitherReader check) go
  where
    go = mconcat [ long "tcp-port"
                 , metavar "PORT"
                 , help "Database TCP port."
                 , value 1113
                 , showDefault
                 ]


    check input =
      case readMay input of
        Nothing -> Left "Invalid TCP port number"
        Just port
          | port >= 1 && port <= 65535 -> Right port
          | otherwise -> Left $ "Port should be between [1,65535]"

--------------------------------------------------------------------------------
parseHttpPort :: Parser Int
parseHttpPort = option (eitherReader check) go
  where
    go = mconcat [ long "http-port"
                 , metavar "PORT"
                 , help "Database HTTP port (default: 2113)."
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid HTTP port number"
        Just port
          | port >= 1 && port <= 65535 -> Right port
          | otherwise -> Left $ "Port should be between [1,65535]"

--------------------------------------------------------------------------------
parseLogin :: Parser (Maybe ByteString)
parseLogin = option (maybeReader check) go
  where
    go = mconcat [ long "login"
                 , metavar "LOGIN"
                 , help "Database user login."
                 , value Nothing
                 ]

    check str
      | null str  = Just Nothing
      | otherwise = Just $ Just $ fromString str

--------------------------------------------------------------------------------
parsePassword :: Parser (Maybe ByteString)
parsePassword = option (maybeReader check) go
  where
    go = mconcat [ long "password"
                 , metavar "PASSWORD"
                 , help "Database user password."
                 , value Nothing
                 ]

    check str
      | null str  = Just Nothing
      | otherwise = Just $ Just $ fromString str

--------------------------------------------------------------------------------
parseHeartbeatInterval :: Parser NominalDiffTime
parseHeartbeatInterval = msDiffTime . fromIntegral <$> option (eitherReader check) go
  where
    go = mconcat [ long "heartbeat-interval"
                 , metavar "INTERVAL"
                 , help "Connection heartbeat interval in milliseconds."
                 , value 750
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid heartbeat interval value, should be a float number."
        Just value
          | value >= 1 -> Right value
          | otherwise -> Left $ "Heartbeat interval should be greater than 0"

--------------------------------------------------------------------------------
parseHeartbeatTimeout :: Parser NominalDiffTime
parseHeartbeatTimeout = msDiffTime . fromIntegral <$> option (eitherReader check) go
  where
    go = mconcat [ long "heartbeat-timeout"
                 , metavar "INTERVAL"
                 , help "Connection heartbeat timeout in milliseconds."
                 , value 1_500
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid heartbeat timeout value, should be a float number."
        Just value
          | value >= 1 -> Right value
          | otherwise -> Left $ "Heartbeat timeout should be greater than 0"

--------------------------------------------------------------------------------
parseVerbose :: Parser Bool
parseVerbose = flag False True go
  where
    go = mconcat [ long "verbose"
                 , help "Enable verbose mode. Harbinger will display a lot of logging."
                 ]

--------------------------------------------------------------------------------
parseCommand :: Parser Command
parseCommand =
  subparser $
    mconcat
      [ command "check" $
          go "Checks if a database node is reachable."
            parseCheckConnectionCommand

      , command "list" $
          go "List specific entities in the database."
            $ fmap List parseListCommand
      ]
  where
    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

--------------------------------------------------------------------------------
parseCheckConnectionCommand :: Parser Command
parseCheckConnectionCommand = pure CheckConnection

--------------------------------------------------------------------------------
parseListCommand :: Parser ListCommand
parseListCommand =
  subparser $
    mconcat
      [ command "streams" $
          go "List streams"
            $ fmap ListStreams parseListStreamsArgs
      ]
  where
    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

--------------------------------------------------------------------------------
parseListStreamsArgs :: Parser ListStreamsArgs
parseListStreamsArgs =
  ListStreamsArgs
    <$> parseListStreamsTarget
    <*> parseListStreamsRecent

--------------------------------------------------------------------------------
parseListStreamsTarget :: Parser TargettedStreams
parseListStreamsTarget = option (eitherReader check) go
  where
    check "user" = Right UserStreams
    check "system" = Right SystemStreams
    check "all" = Right AllStreams
    check wrong =
      Left [i|Wrong stream target value, supported values are: #{supported}|]

    supported :: [TargettedStreams]
    supported = [minBound..maxBound]

    go = mconcat [ long "target"
                 , metavar "TARGET"
                 , help [i|Type of streams your are looking for, supported values are: #{supported}.|]
                 , value UserStreams
                 , showDefault
                 ]

--------------------------------------------------------------------------------
parseListStreamsRecent :: Parser Bool
parseListStreamsRecent = flag False True go
  where
    go = mconcat [ long "recent"
                 , help "Only display recently created streams (last 50)."
                 ]
