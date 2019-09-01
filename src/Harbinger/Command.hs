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
import Data.String (fromString)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Options.Applicative
import Safe (readMay)

--------------------------------------------------------------------------------
data Setts =
  Setts
  { settsHost     :: String
  , settsTcpPort  :: Int
  , settsLogin    :: Maybe ByteString
  , settsPassword :: Maybe ByteString
  } deriving Show

--------------------------------------------------------------------------------
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
data ListStreamsArgs =
  ListStreamsArgs
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
  Setts <$> parseHost
        <*> parseTcpPort
        <*> parseLogin
        <*> parsePassword

--------------------------------------------------------------------------------
parseHost :: Parser String
parseHost = strOption go
  where
    go = mconcat [ long "tcp-host"
                 , metavar "HOST"
                 , help "Database host."
                 , value "localhost"
                 , showDefault
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
parseListStreamsArgs = pure ListStreamsArgs
