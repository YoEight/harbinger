{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Http
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Http where

--------------------------------------------------------------------------------
import Control.Exception (throwIO)
import Control.Monad.Error (MonadError(..))
import Data.Aeson (Value)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status (notFound404, unauthorized401)
import Servant.API
import Servant.Client hiding (Client)

--------------------------------------------------------------------------------
import Harbinger.Command
import Harbinger.Common

--------------------------------------------------------------------------------
data Client =
  Client
  { _clientEnv :: ClientEnv
  , _clientUser :: Maybe User
  }

--------------------------------------------------------------------------------
basicAuthData :: Client -> BasicAuthData
basicAuthData = maybe defaultData go . _clientUser
  where
    defaultData = BasicAuthData "" ""

    go u = BasicAuthData (userLogin u) (userPassword u)

--------------------------------------------------------------------------------
data AccessDenied = AccessDenied deriving Show

--------------------------------------------------------------------------------
type API = BasicAuth "EventStore Node" User :> "subscriptions" :> Capture "stream" Text :> Capture "group" Text :> "info" :> Get '[JSON] Value

--------------------------------------------------------------------------------
proxyAPI :: Proxy API
proxyAPI = Proxy

--------------------------------------------------------------------------------
subscriptionInfo = client proxyAPI

--------------------------------------------------------------------------------
runQuery :: Client -> ClientM a -> IO a
runQuery cl query =
  runClientM query env >>= \case
    Left e -> throwIO e
    Right a -> pure a
  where
   env = _clientEnv cl

--------------------------------------------------------------------------------
optional :: ClientM a -> ClientM (Maybe a)
optional action = catchError (Just <$> action) go
  where
    go self@(FailureResponse _ resp) =
      if responseStatusCode resp == notFound404
        then pure Nothing
        else throwError self

    go self = throwError self

--------------------------------------------------------------------------------
restricted :: ClientM a -> ClientM (Either AccessDenied a)
restricted action = catchError (Right <$> action) go
  where
    go self@(FailureResponse _ resp) =
      if responseStatusCode resp == unauthorized401
        then pure (Left AccessDenied)
        else throwError self

    go self = throwError self

--------------------------------------------------------------------------------
makeClient :: Setts -> IO Client
makeClient setts = do
  mgr <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http host port ""
      env = mkClientEnv mgr baseUrl

  pure (Client env (makeUser setts))

  where
    -- TODO - we might want to pick a different node in case of a cluster
    --  configuration.
    host = NonEmpty.head (settsHosts setts)
    port = NonEmpty.head (settsHttpPorts setts)

--------------------------------------------------------------------------------
getSubscriptionInfo :: Client -> Text -> Text -> IO (Either AccessDenied (Maybe Value))
getSubscriptionInfo env stream groupId =
  runQuery env (restricted $ optional $ subscriptionInfo (basicAuthData env) stream groupId)
