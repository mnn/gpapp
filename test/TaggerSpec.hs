{-# OPTIONS_GHC -F -pgmF htfpp #-}

module TaggerSpec where

import Test.Framework

import Tagger

test_getGenderTagFromTitle = do
  assertEmptyTag ""
  assertEmptyTag "21/GMT"
  assertEmptyTag "Looking for EU players for Overwatch"
  assertMaleTag "21/M/GMT"
  assertFemaleTag "F / 25 / EST xxx xx xxxx"
  assertMaleTag "[21|M|GMT] xxx xxx xx xxx"
    where
      assertEmptyTag x = assertEqual [] (f x)
      assertMaleTag x = assertEqual [PostTagGender Male] (f x) 
      assertFemaleTag x = assertEqual [PostTagGender Female] (f x) 
      f = getGenderTagFromTitle
