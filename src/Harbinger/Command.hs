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
  deriving Show

--------------------------------------------------------------------------------
data StreamListing
  = UserStreams UserStreamsArgs
  | ByCategory ByCategoryArgs
  | ByType ByTypeArgs
  deriving Show

--------------------------------------------------------------------------------
data UserStreamsArgs =
  UserStreamsArgs
  { userStreamsArgsRecent :: Bool
  } deriving Show

--------------------------------------------------------------------------------
data ByCategoryArgs =
  ByCategoryArgs
  { byCategoryArgsName :: Text
  , byCategoryArgsRecent :: Bool
  } deriving Show

--------------------------------------------------------------------------------
data ByTypeArgs =
  ByTypeArgs
  { byTypeArgsName :: Text
  , byTypeArgsRecent :: Bool
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

          -- , command "by-type" $
          --   go [i|List all events by a specfic type. For example, if you pass
          --        "foo-type", Harbinger will display every event having "foo-type"
          --        as a type, regardless of their original stream.
          --      |] parseByTypeStreamListing

          , command "of-users" $
            go [i|List all streams created by users. Commonly those starting by '$'.
            |] parseUserStreamsArgs
          ]

    noCommand = parseUserStreamsArgs

--------------------------------------------------------------------------------
parseUserStreamsArgs :: Parser StreamListing
parseUserStreamsArgs = fmap UserStreams
  UserStreamsArgs
    <$> parseListStreamsRecent

--------------------------------------------------------------------------------
parseByCategoryStreamListing :: Parser StreamListing
parseByCategoryStreamListing = fmap ByCategory $
  ByCategoryArgs
    <$> parseCategoryName
    <*> parseListStreamsRecent

--------------------------------------------------------------------------------
parseByTypeStreamListing :: Parser StreamListing
parseByTypeStreamListing = fmap ByType $
  ByTypeArgs
    <$> parseTypeName
    <*> parseListStreamsRecent

--------------------------------------------------------------------------------
parseListStreamsRecent :: Parser Bool
parseListStreamsRecent = flag False True go
  where
    go = mconcat [ long "recent"
                 , help "Only displays recently created streams (last 50)."
                 ]
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
