{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Data.Either
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative
import GHC.Generics
import Network.Wreq
import Control.Lens
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.Text hiding (takeWhile)

data Affectation = Affectation {
  year :: Int,
  rank :: Int,
  town :: T.Text,
  specialty :: T.Text
} deriving (Show)

printAffect (Affectation y r t s) = T.intercalate ";" [T.pack . show $ y, T.pack . show $ r, t, s]

skipToContent :: [Tag BL.ByteString] -> [Tag BL.ByteString]
skipToContent = drop 3 . takeWhile (~/= ("</div>" :: String)) .
                dropWhile (~/= ("<div class=content>" :: String)) .
                dropWhile (~/= ("<article class=summary-preface>" :: String))

delim = choice ["à l'Assistance Publique-Hôpitaux de"
               , "à l'Assistance Publique des Hôpitaux de"
               , "au CHU de"
               ,"au CHU d'"
               ,"aux Hospices Civils de"]

parseAffect :: Int -> Parser Affectation
parseAffect y = do
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
  return $ Affectation y (read r :: Int) (T.pack town) (T.pack spe)

formatCSV :: [Affectation] -> T.Text
formatCSV l = T.unlines $ "#annee;rang;ville;specialite" : map printAffect l

-- main :: IO ()
main = do
  -- r <- get "https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000042402100"
  -- let d = r ^. responseBody
  -- let tmp = skipToContent $ parseTags d
  -- BL.writeFile "raw.txt" $ innerText tmp
  s <- TIO.readFile "raw.txt"
  let all = rights $ map (parseOnly (parseAffect 2020)) $ T.lines s
  TIO.writeFile "2020.csv" $ formatCSV all
  return ()
  -- print "ok"
