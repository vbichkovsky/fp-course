{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams :: Chars -> Filename -> IO (List Chars)
fastAnagrams word file = let combos = permutations word
                             swords = S.fromList (hlist combos)
                             toDict = S.fromAscList . hlist . lines
                             toOut dict = listh . S.toList $
                               S.intersection swords (toDict dict)
                         in toOut <$> readFile file

newtype NoCaseString = NoCaseString {ncString :: Chars}

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
