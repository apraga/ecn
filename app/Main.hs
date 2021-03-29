{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Control.Applicative
import GHC.Generics
import Network.Wreq
import Control.Lens
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Text hiding (takeWhile)

data Affectation = Affectation {
  rank :: String,
  town :: String,
  specialty :: String
} deriving (Show)

skipToContent :: [Tag BL.ByteString] -> [Tag BL.ByteString]
skipToContent = drop 3 . takeWhile (~/= ("</div>" :: String)) .
                dropWhile (~/= ("<div class=content>" :: String)) .
                dropWhile (~/= ("<article class=summary-preface>" :: String))

delim = choice ["à l'Assistance Publique-Hôpitaux de"
               , "à l'Assistance Publique des Hôpitaux de"
               , "au CHU de"
               ,"au CHU d'"
               ,"aux Hospices Civils de"]

parseAffect :: Parser Affectation
parseAffect = do
  r <- some digit
  skipSpace
  "M." <|> "Mme"
  _ <- manyTill anyChar (char '(')
  _ <- manyTill anyChar (char ')')
  char ','
  skipSpace
  -- spe <- some letter `sepBy'` space
  spe <- manyTill anyChar delim
  -- delim
  skipSpace
  town <- manyTill anyChar (char '.')
  return $ Affectation r town spe

-- main :: IO ()
main = do
  -- r <- get "https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000042402100"
  -- let d = r ^. responseBody
  -- let tmp = skipToContent $ parseTags d
  -- BL.writeFile "raw.txt" $ innerText tmp

  s <- readFile "raw.txt"
  return $ zip (map (parseOnly parseAffect . T.pack) $ lines s) (lines s)
  -- print "ok"
