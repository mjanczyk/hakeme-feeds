{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Yesod
import Yesod.AtomFeed (atomFeed)
import Yesod.RssFeed (rssFeed)
import Yesod.Feed
import Text.Blaze (toMarkup, preEscapedToMarkup)
import Data.Time.Clock (getCurrentTime)

import Scrapper (fetchPage, pageToPosts, Post (..), )

data HakemeFeeds = HakemeFeeds

data HLang = Pl | En deriving (Show, Eq, Read)
data Format = Atom | RSS deriving (Show, Eq, Read)

instance PathPiece HLang where
  fromPathPiece path =
    case T.unpack path of
      "pl" -> Just Pl
      "en" -> Just En
      _ -> Nothing
  toPathPiece Pl = T.pack "pl"
  toPathPiece En = T.pack "en"
  
instance PathPiece Format where
  fromPathPiece path =
    case T.unpack path of
      "atom" -> Just Atom
      "rss" -> Just RSS
      _ -> Nothing
  toPathPiece Atom = T.pack "atom"
  toPathPiece RSS  = T.pack "rss"

mkYesod "HakemeFeeds" [parseRoutes|
  / HomeR GET
  /feed/atom/#HLang/ AtomFeedR GET
  /feed/rss/#HLang/ RssFeedR GET
  /external/#T.Text ExternalR GET
|]

instance Yesod HakemeFeeds where
  urlRenderOverride y (ExternalR url) = Just $ joinPath y url [] []
  urlRenderOverride _ _ = Nothing
  
getHomeR = defaultLayout [whamlet|
  <title>HakemeFeeds
  <h1>HakemeFeeds
  <ul>
    <li><a href=@{AtomFeedR Pl}>Hakeme.pl Atom
    <li><a href=@{RssFeedR Pl}>Hakeme.pl RSS
    <li><a href=@{AtomFeedR En}>Hakeme.com Atom
    <li><a href=@{RssFeedR En}>Hakeme.com RSS
  |]

getExternalR :: T.Text -> Handler Html
getExternalR url = undefined

urlByLang Pl = T.pack "http://hakeme.pl"
urlByLang En = T.pack "http://hakeme.com"

getAtomFeedR lang = atomFeed =<< liftIO (feed lang Atom)
getRssFeedR lang = rssFeed =<< liftIO (feed lang RSS)

feed lang format = do
  let url = urlByLang lang
  page <- fetchPage url
  now <- getCurrentTime
  let posts = pageToPosts url page
  return $ feedFromPosts lang format url now posts

feedFromPosts lang format url time posts = do
  let entries = map postToEntry posts
  
  Feed { feedTitle = T.append url (T.pack " newsfeed")
       , feedLinkSelf = case format of Atom -> AtomFeedR lang ; RSS -> RssFeedR lang
       , feedLinkHome = ExternalR url
       , feedAuthor = T.pack "HakemeFeeds"
       , feedDescription = toMarkup $ T.concat [T.pack "Unofficial Hakeme (", url, T.pack ") newsfeed"]
       , feedLanguage = T.toLower . T.pack $ show lang
       , feedUpdated = time
       , feedEntries = entries
       }

postToEntry (Post { title = title, date = date, link = link, content = content }) =
  FeedEntry { feedEntryLink = ExternalR link
            , feedEntryUpdated = date
            , feedEntryTitle = title
            , feedEntryContent = preEscapedToMarkup content }

main = warpEnv HakemeFeeds

{-
main :: IO ()
main = do
  let url = "http://hakeme.pl"
  html <- fetchPage url
  mapM_ ((>> putStrLn "\n") . putStrLn . T.unpack . content ) $ pageToPosts url html
-}
