{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Reddit
import Reddit.Types.Post

import qualified Data.Text as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show

getPostContent :: Post -> T.Text
getPostContent post = case content post of
                        SelfPost a b -> a
                        _ -> "unknown"
