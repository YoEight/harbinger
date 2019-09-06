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
  = ListStreams StreamListing
  | ListEvents EventListingArgs
  deriving Show

--------------------------------------------------------------------------------
data StreamListing
  = UserStreams UserStreamsArgs
  | ByCategory ByCategoryArgs
  deriving Show

--------------------------------------------------------------------------------
data EventListingArgs =
  EventListingArgs
  { eventListingArgsStream :: Text
  , eventListingArgsTop :: Maybe Int
  } deriving Show

--------------------------------------------------------------------------------
data ParkedMessagesStreamArgs =
  ParkedMessagesStreamArgs
  { parkedMessagesStreamsArgsStream :: Text
  , parkedMessagesStreamsArgsGroupId :: Text
  , parkedMessagesStreamsArgsTop :: Maybe Int
  }

--------------------------------------------------------------------------------
data CheckpointStreamArgs =
  CheckpointStreamArgs
  { checkpointStreamsArgsStream :: Text
  , checkpointStreamsArgsGroupId :: Text
  , checkpointStreamsArgsTop :: Maybe Int
  }

--------------------------------------------------------------------------------
data UserStreamsArgs =
  UserStreamsArgs
  { userStreamsArgsTop :: Maybe Int
  } deriving Show

--------------------------------------------------------------------------------
data ByCategoryArgs =
  ByCategoryArgs
  { byCategoryArgsName :: Text
  , byCategoryArgsTop :: Maybe Int
  } deriving Show

--------------------------------------------------------------------------------
data ByTypeArgs =
  ByTypeArgs
  { byTypeArgsName :: Text
  , byTypeArgsTop :: Maybe Int
  } deriving Show

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
          go "List streams" parseStreamListingCommand
      , command "events" $
          go "List events" parseEventListingArgs
      ]
  where
    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

--------------------------------------------------------------------------------
parseStreamListingCommand :: Parser ListCommand
parseStreamListingCommand = fmap ListStreams (withCommand <|> noCommand)
  where
    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

    withCommand =
      subparser $
        mconcat
          [ command "by-category" $
            go [i|List streams by a category. For example, if you pass "user",
                 Harbinger will display every stream conforming to this pattern
                 "user-*".
               |] parseByCategoryStreamListing

          , command "of-users" $
            go [i|List all streams created by users. Commonly those starting by '$'.
            |] parseUserStreamsArgs
          ]

    noCommand = parseUserStreamsArgs

--------------------------------------------------------------------------------
parseUserStreamsArgs :: Parser StreamListing
parseUserStreamsArgs = fmap UserStreams
  UserStreamsArgs
    <$> parseTop

--------------------------------------------------------------------------------
parseByCategoryStreamListing :: Parser StreamListing
parseByCategoryStreamListing = fmap ByCategory $
  ByCategoryArgs
    <$> parseCategoryName
    <*> parseTop

--------------------------------------------------------------------------------
parseByTypeArgs :: Parser ByTypeArgs
parseByTypeArgs =
  ByTypeArgs
    <$> parseTypeName
    <*> parseTop

--------------------------------------------------------------------------------
parseTop :: Parser (Maybe Int)
parseTop = option (eitherReader check) go
  where
    go = mconcat [ long "top"
                 , help "Only displays recently created elements."
                 , value Nothing
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Top value must be a strictly positive value."
        Just value
          | value >= 1 -> Right (Just value)
          | otherwise  -> Left "Top value must be a strictly positive value."

--------------------------------------------------------------------------------
parseCategoryName :: Parser Text
parseCategoryName = strOption go
  where
    go = mconcat [ long "category"
                 , metavar "NAME"
                 , help "Name of the category."
                 ]

--------------------------------------------------------------------------------
parseTypeName :: Parser Text
parseTypeName = strOption go
  where
    go = mconcat [ long "type"
                 , metavar "TYPE"
                 , help "Type of events."
                 ]

--------------------------------------------------------------------------------
parseEventListingArgs :: Parser ListCommand
parseEventListingArgs = fmap ListEvents (withCommand <|> parseStreamEvents)
  where
    parseStreamEvents =
      EventListingArgs
        <$> parseStreamName
        <*> parseTop

    withCommand =
      subparser $
        mconcat
          [ command "parked" $
            go "List parked events of a persistent subscription." $
              fmap parkedMessagesToEventListingArgs parseParkedMessagesStreamArgs

          , command "checkpoint" $
            go "List checkpoint of a persistent subscription." $
              fmap checkpointToEventListingArgs parseCheckpointStreamArgs

          , command "stream" $
            go "List events of a stream" parseStreamEvents

          , command "by-type" $
            go [i|List all events by a specfic type. For example, if you pass
                 "foo-type", Harbinger will display every event having "foo-type"
                 as a type, regardless of their original stream.
               |] $ fmap typeToEventListingArgs parseByTypeArgs
          ]

    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

--------------------------------------------------------------------------------
parseParkedMessagesStreamArgs :: Parser ParkedMessagesStreamArgs
parseParkedMessagesStreamArgs =
  ParkedMessagesStreamArgs
    <$> parseStreamId
    <*> parseGroupId
    <*> parseTop

--------------------------------------------------------------------------------
parseCheckpointStreamArgs :: Parser CheckpointStreamArgs
parseCheckpointStreamArgs =
  CheckpointStreamArgs
    <$> parseStreamId
    <*> parseGroupId
    <*> parseTop

--------------------------------------------------------------------------------
parkedMessagesToEventListingArgs :: ParkedMessagesStreamArgs -> EventListingArgs
parkedMessagesToEventListingArgs args =
  EventListingArgs
  { eventListingArgsStream = [i|$persistentsubscription-#{parkedMessagesStreamsArgsStream args}::#{parkedMessagesStreamsArgsGroupId args}-parked|]
  , eventListingArgsTop = parkedMessagesStreamsArgsTop args
  }

--------------------------------------------------------------------------------
checkpointToEventListingArgs :: CheckpointStreamArgs -> EventListingArgs
checkpointToEventListingArgs args =
  EventListingArgs
  { eventListingArgsStream = [i|$persistentsubscription-#{checkpointStreamsArgsStream args}::#{checkpointStreamsArgsGroupId args}-checkpoint|]
  , eventListingArgsTop = checkpointStreamsArgsTop args
  }

--------------------------------------------------------------------------------
typeToEventListingArgs :: ByTypeArgs -> EventListingArgs
typeToEventListingArgs args =
  EventListingArgs
  { eventListingArgsStream = [i|$et-#{byTypeArgsName args}|]
  , eventListingArgsTop = byTypeArgsTop args
  }

--------------------------------------------------------------------------------
parseStreamName :: Parser Text
parseStreamName = strOption go
  where
    go = mconcat [ long "name"
                 , metavar "NAME"
                 , help "Name of the stream."
                 ]

--------------------------------------------------------------------------------
parseStreamId :: Parser Text
parseStreamId = strOption go
  where
    go = mconcat [ long "stream"
                 , metavar "NAME"
                 , help "Name of the stream."
                 ]

--------------------------------------------------------------------------------
parseGroupId :: Parser Text
parseGroupId = strOption go
  where
    go = mconcat [ long "group-id"
                 , metavar "NAME"
                 , help "Name of the persistent subscription group."
                 ]
