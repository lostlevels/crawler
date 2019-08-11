module Lib where

import Control.Monad.Reader
import Control.Concurrent

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString)

import RedisJobQueue

import qualified Web as W
import qualified Queue as Q
import qualified Repo as R

type State = (Q.State, R.State)
type Main = ReaderT State IO

queueName :: String
queueName = "pending-sites"
maxPagesPerSecond = 1

run :: String -> String -> IO ()
run dbStr seed = do
  runAction dbStr queueName (action seed)
  withJobQueue (fromString queueName) $ \jq -> do
    forever $ do
      res <- popJson jq
      case res of
        Right (Just url) -> void . forkIO $ runAction dbStr queueName (action url)
        Right Nothing -> return ()
        Left r -> print r
      threadDelay . floor $ 1000000 / maxPagesPerSecond

action :: String -> Main ()
action url = do
  let url' = fromString url
  handled <- R.hasUrl url'
  unless handled $ do
    res <- liftIO $ W.downloadHtml url
    case res of
      Just contents -> do
        let title = toStrict . W.extractTitle $ contents
            desc = toStrict . W.extractDescription $ contents
        R.saveSite url' title desc
        liftIO (print ("Handling URL: " ++ url))
        mapM_ Q.enqueue (W.extractLinks url' contents)
      _ -> return ()
    where toStrict = B.concat . BL.toChunks

runAction :: String -> String -> Main () -> IO ()
runAction dbStr qname x =
  R.withRepo (fromString dbStr) $ \db -> do
    Q.withQueue (fromString qname) $ \queue -> do
      runReaderT x (queue, db)
