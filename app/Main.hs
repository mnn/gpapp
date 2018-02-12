{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T

import Data.Either.Utils
import Reddit

import RedditReader
import Tagger
 
type MonadStack a = IO (Either (APIError RedditError) a)

io :: IO a -> IO a 
io = liftIO

printTaggedPostInfo :: TaggedPost -> IO()
printTaggedPostInfo post = printPostInfo PrintPostInfoOptions { ppioFullText = True } (tpPost post) custom where
  custom = "tags : " <> T.intercalate ", " textTags <> "\n" where 
    textTags = map renderTag (tpTags post)

main :: IO ()
main = do
  -- TODO: get limit from args
  rawPosts <- fromRight <$> getPostsFromGamerPals PostsOptions{poLimit = 10}
  let taggedPosts = map tagPost rawPosts 
--  forM_ rawPosts $ \post -> io $ printPostInfo post (const "")
  putStrLn $ "Got " ++ show (length rawPosts) ++ " posts."
  -- TODO: output options - JSON?
  forM_ taggedPosts $ \post -> io $ printTaggedPostInfo post
