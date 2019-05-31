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
import qualified AppArgsParser as AP
 
type MonadStack a = IO (Either (APIError RedditError) a)

io :: IO a -> IO a 
io = liftIO

printTaggedPostInfo :: TaggedPost -> IO()
printTaggedPostInfo post = printPostInfo PrintPostInfoOptions { ppioFullText = True } (tpPost post) custom where
  custom = "tags : " <> T.intercalate ", " textTags <> "\n" where 
    textTags = map renderTag (tpTags post)

main :: IO ()
main = do
  args <- AP.parseArgs
  let limitFromArgs = AP.number args
  when (limitFromArgs < 1) $ error "Invalid limit."
  let options = PostsOptions { poLimit = limitFromArgs
                             }
  rawPosts <- fromRight <$> getPostsFromGamerPals options
  let taggedPosts = map tagPost rawPosts 
--  forM_ rawPosts $ \post -> io $ printPostInfo post (const "")
  putStrLn $ "Got " ++ show (length rawPosts) ++ " posts."
  -- TODO: output options - JSON?
  if AP.json args then putStrLn "TODO: JSON"
  else forM_ taggedPosts $ \post -> io $ printTaggedPostInfo post
