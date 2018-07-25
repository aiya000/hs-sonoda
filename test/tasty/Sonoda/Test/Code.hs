{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Sonoda.Test.Code where

import Data.String.Here (i)
import RIO

-- |
-- Help a coding like
-- `
-- [here|\x:T.
--      |  if p x
--      |    then 10
--      |    else 20
--      |] & trimMargin '|'
-- `
trimMargin :: Char -> String -> String
trimMargin _ (lines -> []) = ""
trimMargin delim (lines -> (firstLine:tailLines)) =
  let removeMargin = dropHead . dropWhile (/= delim) -- remove before '|' and '|'
  in unlines $ firstLine : fmap removeMargin tailLines
  where
    dropHead :: [a] -> [a]
    dropHead [] = []
    dropHead (_:xs) = xs
trimMargin _ _ = error [i|${(__FILE__ :: String)}:L${(__LINE__ :: Int)}: fatal error! Sorry, please report an issue :(|]
