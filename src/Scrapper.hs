module Scrapper where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HTML.TagSoup (Tag (TagOpen, TagClose), parseTags, sections, partitions, (~==), innerText, isTagClose, renderTags, fromAttrib)
import Text.StringLike (StringLike)
import Control.Monad (liftM)
import qualified Data.Text as T
import Data.Text (Text)

data Post = Post { title :: Text
                 , date :: Text
                 , link :: Text
                 , content :: Text }
         deriving (Show)

postTag = TagOpen "div" [("class", "post")]
postTitleTag = TagOpen "h2" []
postFooterTag = TagOpen "div" [("class", "post-footer")]
postDateTag = TagOpen "span" [("class", "post-date")]
postCommentsTag = TagOpen "span" [("class", "post-comments")]
postContentTag = TagOpen "div" [("class", "post-content")]
linkTag = TagOpen "a" []

mapLinksToHost :: String -> [Tag String] -> [Tag String]
mapLinksToHost host = map f
  where
    f (TagOpen "a" attrs) = TagOpen "a" $ rep "href" (host++) attrs
    f (TagOpen "img" attrs) = TagOpen "img" $ rep "src" (host++) attrs
    f tag = tag
    rep key tr [] = []
    rep key tr ((key',val):xs) | key == key' = (key, tr val):xs
    rep key tr (x:xs) = x:(rep key tr xs)

takeUntilTagCloseAny = takeWhile $ not . isTagClose
takeUntilTagClose tag = takeWhile $ not . (~== (TagClose tag))

posts :: StringLike str => [Tag str] -> [[Tag str]]
posts = partitions (~== postTag)

findTitle :: StringLike str => [Tag str] -> str
findTitle = innerText . takeUntilTagCloseAny . head . sections (~== postTitleTag)

findDate :: StringLike str => [Tag str] -> str
findDate = innerText . takeUntilTagCloseAny . head . sections (~== postDateTag)

findContent :: String -> [Tag String] -> String
findContent host = renderTags . mapLinksToHost host . takeUntilTagClose "div" . head . sections (~== postContentTag)

findLink :: String -> [Tag String] -> String
findLink host = (host++) . fromAttrib "href" . head . head . sections (~== linkTag) . head . sections (~== postTitleTag)

page :: String -> IO String
page host = simpleHTTP (getRequest host) >>= getResponseBody

mkPost :: String -> [Tag String] -> Post
mkPost host post = Post { title = T.strip $ T.pack $ findTitle post
                   , date = T.strip $ T.pack $ findDate post
                   , link = T.pack $ findLink host post
                   , content = T.strip $ T.pack $ findContent host post }

parseAll :: String -> String -> [Post]
parseAll host page = map (mkPost host) $ posts $ parseTags page