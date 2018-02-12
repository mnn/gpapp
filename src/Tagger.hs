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
import Data.List

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

getGameTagFromText :: String -> [PostTag]
getGameTagFromText t = tags where
  tags = concatMap (\x -> t & x & maybeToList) extractors
  extractors = [
               ] ++ simpleGameTagExtractors

genericWordExtractor :: Bool -> String -> String -> Maybe String
genericWordExtractor caseSensitive name input = matchRegexAndExtract regex (p input) where
  regex = "\\W" ++ p name ++ "\\W"
  p = if caseSensitive then id else map toLower

wordExtractor :: String -> String -> Maybe String
wordExtractor = genericWordExtractor True

wordExtractorIgnoreCase :: String -> String -> Maybe String
wordExtractorIgnoreCase = genericWordExtractor False

simpleGameTagExt :: String -> String -> Maybe PostTag
simpleGameTagExt w x = wordExtractorIgnoreCase w x <&> const (PostTagGame w)

simpleGameTagExactCaseExt :: String -> String -> Maybe PostTag
simpleGameTagExactCaseExt w x = wordExtractor w x <&> const (PostTagGame w)

-- TODO: load game tags from file
-- TODO: synonyms (Wow = World of Warcraft)

simpleGameTagExtractors :: [String -> Maybe PostTag]
simpleGameTagExtractors = map simpleGameTagExt caseInsensitive ++ map simpleGameTagExactCaseExt caseSensitive where
  caseInsensitive = [ "Overwatch"
                    , "Minecraft"
                    , "Modded Minecraft"
                    , "PUBG"
                    , "GMOD"
                    , "Fortnite"
                    , "Tabletop Simulator"
                    , "World of Warcraft"
                    , "Starcraft 2"
                    , "Heroes of the Storm"
                    , "HotS"
                    , "Monster Hunter"
                    , "League Of Legends"
                    , "Warframe"
                    , "Dead By Daylight"
                    , "Guild Wars 2"
                    , "GTA V"
                    , "ARK"
                    , "ARK: Survival Evolved"
                    , "Rocket League"
                    , "CSGO"
                    , "Rainbow Six Siege"
                    , "Terraria"
                    , "Killing Floor 2"
                    , "Don't Starve"
                    , "DotA"
                    , "Destiny 2"
                    , "Team Fortress 2"
                    , "TF2"
                    , "FFXIV"
                    , "AC: Origins"
                    , "Runescape"
                    , "Black Desert Online"
                    , "COD:WW2"
                    , "Battlerite"
                    , "Path of Exile"
                    , "Dark Souls 3"
                    , "7 Days To Die"
                    , "7DTD"
                    , "Space Engineers"
                    , "Civilization V"
                    , "Portal Knights"
                    , "Starbound"
                    , "Borderlands 2"
                    , "Atlas Reactor"
                    , "Grim Dawn"
                    , "Portal 2"
                    , "L4D2"
                    , "Rust"
                    , "Smite"
                    , "Paladins"
                    , "Diablo 2"
                    , "Diablo 3"
                    , "Elder Scrolls Online"
                    , "ESO"
                    , "Payday 2"
                    , "Halo Wars 2"
                    , "World of Tanks"
                    , "World of Warships"
                    , "Fire Emblem"
                    , "Pokemon"
                    , "Tera"
                    , "War Thunder"
                    ]
  caseSensitive = [ "WoW"
                  , "LoL"
                  , "GW2"
                  , "D&D"
                  ]

tagPost :: Post -> TaggedPost
tagPost post = TaggedPost { tpPost = post
                          , tpTags = tags
                          } where
  tags = nub $ foldl ffun []
    [ getGenderTag
    , getGameTag
      -- TODO: more tags
    ]
  ffun acc x = acc ++ x post
