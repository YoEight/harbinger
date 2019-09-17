--------------------------------------------------------------------------------
-- |
-- Module    :  Harbinger.Command.ListSubs
-- Copyright :  (C) 2019 Yorick Laupa
-- License   :  (see the file LICENSE)
-- Maintainer:  Yorick Laupa <yo.eight@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------------------
module Harbinger.Command.ListSubs where

--------------------------------------------------------------------------------
import           Data.Foldable (traverse_)
import           Data.String.Interpolate.IsString (i)
import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           System.Exit (exitFailure)

--------------------------------------------------------------------------------
import Harbinger.Command
import Harbinger.Http

--------------------------------------------------------------------------------
run :: Setts -> IO ()
run setts = do
  env <- makeClient setts

  getSubscriptionList env >>= \case
    Left _ -> do
      Text.putStrLn [i|Your current user is not allowed to list subscriptions .|]
      exitFailure

    Right xs -> printList xs

--------------------------------------------------------------------------------
printList :: [SubscriptionSummary] -> IO ()
printList = traverse_ go
  where
    go info = do
      let processDiff = subscriptionSummaryLastKnownEventNumber info - subscriptionSummaryLastProcessedEventNumber info
      Text.putStrLn "--------------------------------------------"
      Text.putStrLn [i|Stream: #{subscriptionSummaryEventStreamId info}|]
      Text.putStrLn [i|Group: #{subscriptionSummaryGroupName info}|]
      Text.putStrLn [i|Status: #{subscriptionSummaryStatus info}|]
      Text.putStrLn [i|Connections: #{subscriptionSummaryConnectionCount info}|]
      Text.putStrLn [i|Processed / Known: #{subscriptionSummaryLastProcessedEventNumber info} / #{subscriptionSummaryLastKnownEventNumber info} (#{processDiff})|]
      Text.putStrLn [i|Processing speed: #{subscriptionSummaryAverageItemsPerSecond info} msgs/sec|]
