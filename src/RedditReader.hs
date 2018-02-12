{-# LANGUAGE OverloadedStrings #-}

module RedditReader ( getPostsFromGamerPals
                    , printPostInfo
                    , PostsOptions(..)
                    , PrintPostInfoOptions(..)
) where

import Reddit
import Reddit.Types.Post
import Reddit.Types.Listing as Listing
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

getSubredditName :: Post -> Text.Text
getSubredditName p = getR (subreddit p) where
  getR (R a) = a

formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%x %R"

data PrintPostInfoOptions = 
  PrintPostInfoOptions {
    ppioFullText :: Bool
  }

-- TODO: link
printPostInfo :: PrintPostInfoOptions -> Post -> Text.Text -> IO ()
printPostInfo opts post customPart = Text.putStr $
                         "title: "  <> title post <> "\n" <> 
                         "date : "  <> (Text.pack . formatDate . created) post <> "\n" <>
                         "text : "  <> textFn (getPostContent post) <> "\n" <>
                         customPart <> 
                         postDelim  <> "\n\n" where
                           textFn = if ppioFullText opts then textFnFull else textFnStripped
                           textFnStripped = Text.take 128 . head . Text.lines
                           textFnFull x = "\n" <> textDelim <> "\n" <> Text.strip x <> "\n" <> textDelim
                           textDelim = Text.pack $ replicate 64 '-'
                           postDelim = Text.pack $ replicate 64 '='

printPostInfoRaw :: Post -> IO ()
printPostInfoRaw post = Text.putStrLn $
                          "[" <> tshow (score post) <> "] " <>
                          title post <> " (" <> tshow (getSubredditName post) <> ")"

runRedditAnonWithUA :: Reddit a -> IO (Either (APIError RedditError) a)
runRedditAnonWithUA =
  runRedditWith defaultRedditOptions { customUserAgent = Just "reddit-haskell:gpapp:0.1.0.0 (by /u/monnef)" }

subredditToUse = R "GamerPals"

getPostsFromGamerPalsRaw :: PostsOptions -> IO (Either (APIError RedditError) [Post])
getPostsFromGamerPalsRaw opts = runRedditAnonWithUA $ do
--  Listing _ _ posts <- search (Just subredditToUse) (Options Nothing (Just . poLimit $ opts)) SO.New ""
  Listing _ _ posts <- getPosts' (Options Nothing (Just . poLimit $ opts)) Listing.New (Just subredditToUse)
  return posts

convApiErrToString :: APIError RedditError -> String
convApiErrToString = show

data PostsOptions = PostsOptions { poLimit :: Int
                                 }

getPostsFromGamerPals :: PostsOptions -> IO (Either String [Post])
getPostsFromGamerPals opts = do
  e <- getPostsFromGamerPalsRaw opts
  return $ first convApiErrToString e
