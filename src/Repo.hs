module Repo where

import Control.Exception
import Control.Monad.Reader

import Data.ByteString
import Data.Has
import Database.PostgreSQL.Simple

import GHC.Int

type State = Connection
type Repo r m = (Has State r, MonadReader r m, MonadIO m)

withRepo :: ByteString -> (State -> IO a) -> IO a
withRepo connStr f =
  bracket acquire close f where
    acquire = connectPostgreSQL connStr
  
saveSite :: Repo r m => ByteString -> ByteString -> ByteString -> m ()
saveSite url title desc = do
  conn <- asks getter
  res <- liftIO (try (execute conn "INSERT INTO sites VALUES (?, ?, ?)" (url, title, desc)) :: IO (Either SomeException GHC.Int.Int64))
  case res of
    Left e -> liftIO (print e)
    Right _ -> return ()

hasUrl :: Repo r m => ByteString -> m Bool
hasUrl url = do
  conn <- asks getter
  let q = "SELECT EXISTS (SELECT * FROM sites WHERE url = ?)"
  res <- liftIO (try $ (query conn q (Only url)) :: IO (Either SqlError [Only Bool]))
  case res of
    Left err -> return False
    Right [Only val] -> return val
