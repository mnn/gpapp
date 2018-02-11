{-# LANGUAGE OverloadedStrings #-}

module Tagger( TaggedPost(..)
             , tagPost
             , renderTag
             , getGenderTagFromTitle
             , PostTag(..)
             , Gender(..)
) where

import qualified Data.Text as T
import Data.Monoid
import Text.RegexPR
import Data.Char
import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.Maybe
import Control.Lens.Operators ((<&>))

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
renderTag (PostTagGame x) = formatRenderedTag $ T.pack x

data TaggedPost = TaggedPost { tpPost :: Post
                             , tpTags :: [PostTag]
                             }

extractResultOfMatchRegexPR :: Maybe ((String, (String, String)), [(Int, String)]) -> Maybe String
extractResultOfMatchRegexPR regexRes = (snd >>> head >>> snd) <$> regexRes

matchRegexAndExtract :: String -> String -> Maybe String
matchRegexAndExtract regex x = matchRegexPR regex x & extractResultOfMatchRegexPR

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
  regexResGroup = extractResultOfMatchRegexPR regexRes
  regex = "(?:\\/|\\d+|\\||^)\\s*([MmFf])\\s*(?:\\/|\\|| )"

getGameTag :: Post -> [PostTag]
getGameTag p = concatMap (T.unpack >>> getGameTagFromText) [title p, getPostContent p]

-- TODO
getGameTagFromText :: String -> [PostTag]
getGameTagFromText t = tags where
  tags = concatMap (\x -> t & x & maybeToList) extractors
  extractors = [ gameExtOverwatch
               , gameExtMinecraft
               ]

wordExtractor :: String -> String -> Maybe String
wordExtractor name x = matchRegexAndExtract regex (map toLower x) where
  regex = "\\W" ++ map toLower name ++ "\\W"

simpleGameTagExt :: String -> String -> Maybe PostTag
simpleGameTagExt w x = wordExtractor w x <&> const (PostTagGame w)

-- TODO: refactor - group all simple game extractors

gameExtOverwatch :: String -> Maybe PostTag
gameExtOverwatch = simpleGameTagExt "Overwatch"

gameExtMinecraft :: String -> Maybe PostTag
gameExtMinecraft = simpleGameTagExt "Minecraft"


tagPost :: Post -> TaggedPost
tagPost post = TaggedPost { tpPost = post
                          , tpTags = tags
                          } where
  tags = foldl ffun []
    [ getGenderTag
    , getGameTag
      -- TODO: more tags
    ]
  ffun acc x = acc ++ x post
