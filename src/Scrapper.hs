module Scrapper where

import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HTML.TagSoup (Tag (TagOpen, TagClose), TagRep, parseTags, sections, partitions, (~==), (~/=), innerText, isTagClose, renderTags, fromAttrib)
import Text.StringLike (StringLike)
import qualified Text.StringLike as SL
import Control.Monad (liftM)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as E
import Data.Time.Clock (UTCTime)
import Data.Time.Format (readTime)
import System.Locale (defaultTimeLocale)
import Codec.Binary.UTF8.String (decodeString)

data Post = Post { title :: Text
                 , date :: UTCTime
                 , link :: Text
                 , content :: Text }
         deriving (Show)

s :: (StringLike str) => String -> str
s = SL.fromString

postTag = TagOpen "div" [("class", "post")]
postTitleTag = TagOpen "h2" []
postFooterTag = TagOpen "div" [("class", "post-footer")]
postDateTag = TagOpen "span" [("class", "post-date")]
postCommentsTag = TagOpen "span" [("class", "post-comments")]
postContentTag = TagOpen "div" [("class", "post-content")]
linkTag = TagOpen "a" []

mapLinksToHost :: StringLike str => str -> [Tag str] -> [Tag str]
mapLinksToHost host = map f
  where
    f (TagOpen a attrs)   | a == s"a"     = TagOpen (s"a") $ rep (s"href") (SL.append host) attrs
    f (TagOpen img attrs) | img == s"img" = TagOpen (s"img") $ rep (s"src") (SL.append host) attrs
    f tag = tag
    rep key tr [] = []
    rep key tr ((key',val):xs) | key == key' = (key, tr val):xs
    rep key tr (x:xs) = x:(rep key tr xs)

takeUntilTagCloseAny :: StringLike str => [Tag str] -> [Tag str]
takeUntilTagCloseAny = takeWhile $ not . isTagClose

takeUntilTagClose :: StringLike str => str -> [Tag str] -> [Tag str]
takeUntilTagClose tag = takeWhile $ (~/= (TagClose tag))

posts :: StringLike str => [Tag str] -> [[Tag str]]
posts = partitions (~== postTag)

findTitle :: StringLike str => [Tag str] -> str
findTitle = innerText . takeUntilTagCloseAny . head . sections (~== postTitleTag)

findDate :: StringLike str => [Tag str] -> str
findDate = innerText . takeUntilTagCloseAny . head . sections (~== postDateTag)

findContent :: StringLike str => str -> [Tag str] -> str
findContent host = renderTags . mapLinksToHost host . takeUntilTagClose (s"div") . head . sections (~== postContentTag)

findLink :: (StringLike str, Show str) => str -> [Tag str] -> str
findLink host = (SL.append host) . fromAttrib (SL.fromString "href") . head . head . sections (~== linkTag) . head . sections (~== postTitleTag)

fetchPage :: Text -> IO Text
fetchPage host = simpleHTTP (getRequest $ T.unpack host) >>= getResponseBody >>= return . T.pack .decodeString

mkPost :: Text -> [Tag Text] -> Post
mkPost host post = Post { title = T.strip $ findTitle post
                   , date = parseDate $ findDate post
                   , link = findLink host post
                   , content = T.strip $ findContent host post }

pageToPosts :: Text -> Text -> [Post]
pageToPosts host page = map (mkPost host) $ posts $ parseTags page

parseDate :: Text -> UTCTime
parseDate = readTime locale fmt . T.unpack
  where
    locale = defaultTimeLocale
    fmt = "%d.%m.%Y"