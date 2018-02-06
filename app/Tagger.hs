{-# LANGUAGE OverloadedStrings #-}

module Tagger( TaggedPost(..)
             , tagPost
             , renderTag
) where

import qualified Data.Text as T
import Data.Monoid
import Text.RegexPR
import Data.Char
import Control.Arrow ((>>>))
import Data.Function ((&))

import Reddit.Types.Post
import Utils

data Gender = Male | Female
            deriving (Show, Eq)

data PostTag = PostTagGender Gender
             | PostTagGame String
             | PostTagTimeZone Int
             | PostTagAge Int
             | PostTagCountry String
             deriving (Show, Eq)
             
             -- TODO: gay/lesbian?

formatRenderedTag :: T.Text -> T.Text
formatRenderedTag x = "[" <> x <> "]"

renderTag :: PostTag -> T.Text
renderTag (PostTagGender x) = formatRenderedTag $ tshow x

data TaggedPost = TaggedPost { tpPost :: Post
                             , tpTags :: [PostTag]
                             }

getGenderTag :: Post -> [PostTag]
getGenderTag p = getGenderTagFromTitle $ T.unpack $ title p

getGenderTagFromTitle :: String -> [PostTag]
getGenderTagFromTitle title = tags where
  tags = case regexResGroup of
           Just x -> [PostTagGender gender] where
             gender = case  map toLower x of 
               "m" -> Male 
               "f" -> Female
           Nothing -> []
  regexRes = title & take 20 & matchRegexPR regex
  regexResGroup = (snd >>> head >>> snd) <$> regexRes
  regex = "(?:\\/|\\d+|\\||^)\\s*([MmFf])\\s*(?:\\/|\\|| )"

tagPost :: Post -> TaggedPost
tagPost post = TaggedPost { tpPost = post
                          , tpTags = tags
                          } where
  tags = foldl ffun [] [
      getGenderTag
      -- TODO: more tags
    ]
  ffun acc x = acc ++ x post
