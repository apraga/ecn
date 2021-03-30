{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

-- import Data.Attoparsec.ByteString.Char8 hiding (takeWhile)
-- import qualified Data.ByteString as B
import Control.Applicative
import Control.Lens
import Data.Attoparsec.Text hiding (takeWhile)
import Data.Either
import Data.List (intercalate)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import GHC.Generics
import Network.Wreq
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

parseAll :: Int -> Parser [Affectation]
parseAll y = parseAffect y `sepBy` endOfLine

-- Version without newlines
-- parseAffects :: Int -> Parser [Affectation]
startDelim y = T.concat ["au titre de l'année universitaire "
                        , T.pack $ show y
                        , "-", T.pack $ show (y+1), " :"]
parseAffects y = do
  _ <- manyTill anyChar (string $ startDelim y)
  many (parseAffect y)

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
  return $ Affectation y (read r :: Int) (T.pack town) (T.strip . T.pack $ spe)

formatCSV :: [Affectation] -> T.Text
formatCSV l = T.unlines $ "annee;rang;ville;specialite" : map printAffect l

getYear' :: String -> IO BL.ByteString
getYear' root = do
  r <- get $ "https://www.legifrance.gouv.fr/jorf/id/"++  root
  let d = r ^. responseBody
  let s = innerText . (Prelude.take 15) . skipToContent $ parseTags d
  return s

getYear :: String -> IO T.Text
getYear root = do
  r <- get $ "https://www.legifrance.gouv.fr/jorf/id/"++  root
  let d = r ^. responseBody
  let s = innerText . skipToContent $ parseTags d
  return $ toStrict . decodeUtf8 $ s

-- main :: IO ()
affectYear :: Int -> String -> IO [Affectation]
affectYear y root = do
  s' <- getYear root

  -- BL.writeFile "raw.txt" $ innerText tmp
  -- s <- TIO.readFile "raw.txt"
  -- Sometimes there are no newline
  let all = if length (T.lines s') == 1
            then fromRight [] $ parseOnly (parseAffects y ) s'
            else rights $ map (parseOnly (parseAffect y )) $ T.lines s'
  print $ length all
  return all

main = do
  all <- affectYear 2020 "JORFTEXT000042402100"
  all <- affectYear 2019 "JORFTEXT000039229737"
  -- TIO.writeFile "docs/2019.csv" $ formatCSV all
  return ()
  -- print "ok"
