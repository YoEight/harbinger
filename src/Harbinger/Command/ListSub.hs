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
import qualified Data.Aeson.Encode.Pretty as Pretty
import qualified Data.ByteString.Lazy.Char8 as LazyChar8
import           Data.String.Interpolate.IsString (i)
import qualified Data.Text.IO as Text
import           System.Exit (exitFailure)

--------------------------------------------------------------------------------
import Harbinger.Command
import Harbinger.Http

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
          LazyChar8.putStrLn (Pretty.encodePretty dat)
