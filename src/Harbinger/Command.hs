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
import Data.Int (Int64, Int32)
import Data.List (foldl1)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Data.String (fromString)
import Data.String.Interpolate.IsString (i)
import Data.Text (Text)
import Data.Time (NominalDiffTime)
import Database.EventStore (SystemConsumerStrategy(..), PersistentSubscriptionSettings(..), msDiffTime)
import Data.DotNet.TimeSpan (TimeSpan, fromSeconds)
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
  | Create CreateCommand
  | Version
  deriving Show

--------------------------------------------------------------------------------
data ListCommand
  = ListStreams StreamListing
  | ListEvents EventListingArgs
  | ListSub SubListingArgs
  | ListSubs
  deriving Show

--------------------------------------------------------------------------------
data CreateCommand
  = CreateSub SubCreateArgs
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
data SubListingArgs =
  SubListingArgs
  { subListingArgsStream :: Text
  , subListingArgsGroup :: Text
  , subListingArgsDetailed :: Bool
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
data SubCreateArgs =
  SubCreateArgs
  { subCreateArgsStreamName :: Text
  , subCreateArgsGroup :: Text
  , subCreateArgsSettings :: PersistentSubscriptionSettings
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
parseCommand = withCommand <|> parseVersion
  where
    withCommand =
      subparser $
        mconcat
          [ command "check" $
              go "Checks if a database node is reachable."
                parseCheckConnectionCommand

          , command "list" $
              go "List specific entities in the database."
                $ fmap List parseListCommand

          , command "create" $
              go "Create database entities."
                $ fmap Create parseCreateCommand
          ]

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
      , command "subscription" $
          go "Show a persistent subscription information" parseSubListingArgs
      , command "subscriptions" $
          go "List persistent subscriptions" (pure ListSubs)
      ]
  where
    go desc parser =
      info (helper <*> parser)
           (progDesc desc)

--------------------------------------------------------------------------------
parseCreateCommand :: Parser CreateCommand
parseCreateCommand =
  subparser $
    mconcat
      [ command "subscription" $
          go "Create a persistent subscription" $
            fmap CreateSub parseSubCreateArgs
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

--------------------------------------------------------------------------------
parseSubListingArgs :: Parser ListCommand
parseSubListingArgs = fmap ListSub $
  SubListingArgs
    <$> parseStreamId
    <*> parseGroupId
    <*> parseDetailed

--------------------------------------------------------------------------------
parseDetailed :: Parser Bool
parseDetailed = flag False True go
  where
    go = mconcat [ long "detailed"
                 , help "Make the command display as much information as possible"
                 ]
--------------------------------------------------------------------------------
parseVersion :: Parser Command
parseVersion = flag' Version go
  where
    go = mconcat [ long "version"
                 , short 'v'
                 , help "Program version."
                 ]

--------------------------------------------------------------------------------
parseSubCreateArgs :: Parser SubCreateArgs
parseSubCreateArgs =
  SubCreateArgs
    <$> parseStreamId
    <*> parseGroupId
    <*> parsePersistentSubscriptionSettings

--------------------------------------------------------------------------------
parsePersistentSubscriptionSettings :: Parser PersistentSubscriptionSettings
parsePersistentSubscriptionSettings =
  PersistentSubscriptionSettings
    <$> parseResolveLinkTos
    <*> parseStartFrom
    <*> parseExtraStats
    <*> parseTimeout
    <*> parseMaxRetryCount
    <*> parseLiveBufSize
    <*> parseReadBatchSize
    <*> parseHistoryBufSize
    <*> parseCheckPointAfter
    <*> parseMinCheckPointCount
    <*> parseMaxCheckPointCount
    <*> parseMaxSubsCount
    <*> parseNamedConsumerStrategy

--------------------------------------------------------------------------------
parseResolveLinkTos :: Parser Bool
parseResolveLinkTos = flag False True go
  where
    go = mconcat [ long "resolve-link"
                 , help "Whether or not the persistent subscription should resolve linkTo events to their linked events."
                 ]

--------------------------------------------------------------------------------
parseStartFrom :: Parser Int64
parseStartFrom = option (eitherReader check) go
  where
    go = mconcat [ long "start-from"
                 , metavar "EVENT_NUMBER"
                 , help "Where the subscription should start from (think event number)."
                 , value (-1)
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid start from value, should be an integer."
        Just value -> Right value

--------------------------------------------------------------------------------
parseExtraStats :: Parser Bool
parseExtraStats = flag False True go
  where
    go = mconcat [ long "extra-stats"
                 , help "Whether or not in depth latency statistics should be tracked on this subscription."
                 ]

--------------------------------------------------------------------------------
parseTimeout :: Parser TimeSpan
parseTimeout = fmap fromSeconds $ option (eitherReader check) go
  where
    go = mconcat [ long "timeout"
                 , metavar "SECONDS"
                 , help "The amount of time after which a message should be considered to be timeout and retried."
                 , value 30.0
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid timeout value, should be a float number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Timeout value should be a positive float number."

--------------------------------------------------------------------------------
parseMaxRetryCount :: Parser Int32
parseMaxRetryCount = option (eitherReader check) go
  where
    go = mconcat [ long "max-retry"
                 , metavar "INTEGER"
                 , help "The maximum number of retries (due to timeout) before a message get considered to be parked."
                 , value 10
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid max retry value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Max retry value should be a positive number."

--------------------------------------------------------------------------------
parseLiveBufSize :: Parser Int32
parseLiveBufSize = option (eitherReader check) go
  where
    go = mconcat [ long "live-buffer-size"
                 , metavar "INTEGER"
                 , help "The size of the buffer listening to live messages as they happen."
                 , value 500
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid Live buffer size value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Live buffer size value should be a positive number."

--------------------------------------------------------------------------------
parseReadBatchSize :: Parser Int32
parseReadBatchSize = option (eitherReader check) go
  where
    go = mconcat [ long "read-batch-size"
                 , metavar "INTEGER"
                 , help "The number of events read at a time when paging in history."
                 , value 500
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid Read batch size value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Read batch size value should be a positive number."

--------------------------------------------------------------------------------
parseHistoryBufSize :: Parser Int32
parseHistoryBufSize = option (eitherReader check) go
  where
    go = mconcat [ long "history-buffer-size"
                 , metavar "INTEGER"
                 , help "The number of events to cache when paging through history."
                 , value 20
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid history buffer size value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Read history buffer size value should be a positive number."

--------------------------------------------------------------------------------
parseCheckPointAfter :: Parser TimeSpan
parseCheckPointAfter = fmap fromSeconds $ option (eitherReader check) go
  where
    go = mconcat [ long "checkpoint-after"
                 , metavar "SECONDS"
                 , help "The amount of time to try checkpoint after."
                 , value 2.0
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid checkpoint after value, should be a float number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Checkpoint after value should be a positive float number."

--------------------------------------------------------------------------------
parseMinCheckPointCount :: Parser Int32
parseMinCheckPointCount = option (eitherReader check) go
  where
    go = mconcat [ long "min-checkpoint-count"
                 , metavar "INTEGER"
                 , help "The minimum number of messages to checkpoint."
                 , value 10
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid minimum checkpoint count value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Minimum checkpoint count value should be a positive number."

--------------------------------------------------------------------------------
parseMaxCheckPointCount :: Parser Int32
parseMaxCheckPointCount = option (eitherReader check) go
  where
    go = mconcat [ long "max-checkpoint-count"
                 , metavar "INTEGER"
                 , help "The maximum number of messages to checkpoint. If this number is reached, a checkpoint will be forced."
                 , value 1_000
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid maximum checkpoint count value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Maximum checkpoint count value should be a positive number."

--------------------------------------------------------------------------------
parseMaxSubsCount :: Parser Int32
parseMaxSubsCount = option (eitherReader check) go
  where
    go = mconcat [ long "max-subscriber-count"
                 , metavar "INTEGER"
                 , help "The maximum number of subscribers allowed."
                 , value 1_000
                 , showDefault
                 ]

    check input =
      case readMay input of
        Nothing -> Left "Invalid maximum subscriber count value, should be a number."
        Just value
          | value >= 0 -> Right value
          | otherwise -> Left "Maximum subscriber count value should be a positive number."

--------------------------------------------------------------------------------
parseNamedConsumerStrategy :: Parser SystemConsumerStrategy
parseNamedConsumerStrategy = option (eitherReader check) go
  where
    go = mconcat [ long "strategy"
                 , metavar "STRATEGY_NAME"
                 , help [i|The strategy to use for distributing events to client consumers. Possibilities: #{display choices}|]
                 , value RoundRobin
                 , showDefaultWith showing
                 ]

    check "dispatch-to-single" = Right DispatchToSingle
    check "round-robin" = Right RoundRobin
    check "pinned" = Left "Pinned is Unsupported for the moment."
    check other = Left [i|Unsupported [#{other} strategy.]|]

    showing RoundRobin = "round-robin"
    showing DispatchToSingle = "dispatch-to-single"

    choices = [DispatchToSingle, RoundRobin]

    display = foldl1 (\a b -> a <> ", " <> b) . fmap showing
