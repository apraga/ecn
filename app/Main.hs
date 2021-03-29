{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import GHC.Generics
import Network.Wreq
import Control.Lens
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL


skipToContent = drop 3 . takeWhile (~/= ("</div>" :: String)) .
                dropWhile (~/= ("<div class=content>" :: String)) .
                dropWhile (~/= ("<article class=summary-preface>" :: String))

-- main :: IO ()
main = do
  r <- get "https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000042402100"
  let d = r ^. responseBody
  let tmp = skipToContent $ parseTags d
  BL.writeFile "raw.txt" $ innerText tmp
