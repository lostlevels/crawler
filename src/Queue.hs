module Queue where

import Control.Exception
import Control.Monad.Reader

import Data.ByteString
import Data.Has

import RedisJobQueue

type State = JobQueue
type Queue r m = (Has State r, MonadReader r m, MonadIO m)

withQueue :: ByteString -> (State -> IO ()) -> IO ()
withQueue queueName f =
  withJobQueue queueName f
  
enqueue :: Queue r m => String -> m ()
enqueue msg = do
  jq <- asks getter
  void . liftIO $ pushJson jq 1 msg
