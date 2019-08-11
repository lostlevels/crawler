module Web where

import Control.Applicative (liftA2)
import Control.Exception
import Control.Lens

import Network.URI
import Network.Wreq

import Text.HTML.TagSoup

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

opts = defaults & header "User-Agent" .~ ["WebCrawlerBot/1.0"]

downloadHtml :: String -> IO (Maybe BL.ByteString)
downloadHtml url = do
  eres <- try (getWith opts url) :: IO (Either SomeException (Response BL.ByteString))
  case eres of
    Left err -> print err >> return Nothing
    Right r -> do
      let header = r ^. responseHeader "Content-Type"
      if "text/html" `B.isPrefixOf` header then
        return $ Just (r ^. responseBody)
      else
        return Nothing

extractLinks :: B.ByteString -> BL.ByteString -> [String]
extractLinks origin contents =
  fmap (combine origin' . filter notquotes . show) . filter nonEmptyLink . hrefs . filter isTagOpen $ tags
  where
    tags = [tag | tag@(TagOpen name _) <- parseTags contents, name == "a"]
    hrefs = fmap (fromAttrib "href")
    notquotes x = x /= '"' && x /= '\'' && x /= '\\'
    origin' = filter notquotes (show origin)
    nonEmptyLink x = x /= "#" && x /= ""

extractTitle :: BL.ByteString -> BL.ByteString
extractTitle contents =
  title $ parseTags contents
  where
    selector = "<title>" :: String
    title = innerText . take 2 . dropWhile (~/= selector)

extractDescription :: BL.ByteString -> BL.ByteString
extractDescription contents =
  case title (parseTags contents) of
    [] -> ""
    (x:_) -> fromAttrib "content" x
  where
    selector = "<meta property=og:description>" :: String
    title = dropWhile (~/= selector)

combine :: String -> String -> String
combine origin url =
  case liftA2 relativeTo (parseURIReference url) (parseURI origin) of
    Nothing -> url
    Just nurl -> dropFragment $ show nurl
  where dropFragment = takeWhile (/= '#') 
