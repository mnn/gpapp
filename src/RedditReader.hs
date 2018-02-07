{-# LANGUAGE OverloadedStrings #-}

module RedditReader ( getPostsFromGamerPals
                    , printPostInfo
                    , PostsOptions(..)
) where

import Reddit
import Reddit.Types.Post
import Reddit.Types.SearchOptions as SO
import Utils

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Monoid
import Data.Bifunctor
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time.Format
import Data.Time.Clock

getPostContent post = case content post of
                        SelfPost a b -> a
                        _ -> "unknown"

getSubredditName :: Post -> Text.Text
getSubredditName p = getR (subreddit p) where
  getR (R a) = a

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%x %R"

-- TODO: link
printPostInfo :: Post -> Text.Text -> IO ()
printPostInfo post customPart = Text.putStr $
                         "date : " <> (Text.pack . formatDate . created) post <> "\n" <>
                         "title: " <> title post <> "\n" <> 
                         "text : " <> (Text.take 128 . head . Text.lines) (getPostContent post) <> "\n" <>
                         customPart <> 
                         ""        <> "\n"

printPostInfoRaw :: Post -> IO ()
printPostInfoRaw post = Text.putStrLn $
                          "[" <> tshow (score post) <> "] " <>
                          title post <> " (" <> tshow (getSubredditName post) <> ")"

runRedditAnonWithUA :: Reddit a -> IO (Either (APIError RedditError) a)
runRedditAnonWithUA =
  runRedditWith defaultRedditOptions { customUserAgent = Just "reddit-haskell:gpapp:0.1.0.0 (by /u/monnef)" }

getPostsFromGamerPalsRaw :: PostsOptions -> IO (Either (APIError RedditError) [Post])
getPostsFromGamerPalsRaw opts = runRedditAnonWithUA $ do
  Listing _ _ posts <- search (Just $ R "GamerPals") (Options Nothing (Just . poLimit $ opts)) SO.New ""
  return posts

convApiErrToString :: APIError RedditError -> String
convApiErrToString = show

data PostsOptions = PostsOptions { poLimit :: Int
                                 }

getPostsFromGamerPals :: PostsOptions -> IO (Either String [Post])
getPostsFromGamerPals opts = do
  e <- getPostsFromGamerPalsRaw opts
  return $ first convApiErrToString e
