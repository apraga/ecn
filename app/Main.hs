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
               , "à l'" -- Match pour AP-HP in last resort
               , "au CHU de"
               ,"au CHU d'"
               ,"aux Hospices Civils de"
               , "aux" -- Match HCL
               , "à" -- Just the town (2016)
               , "en" -- Region (2016)
               ]

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
  "M." <|> "Mme" <|> "Mlle" <|> "MME" <|> "mlle"
  _ <- manyTill anyChar (char '(')
  _ <- manyTill anyChar (char ')')
  -- TODO
  test <- option "" $ ", né" *> manyTill anyChar (char ',')
  -- char ','
  skipSpace
  spe <- manyTill anyChar delim
  skipSpace
  town <- manyTill anyChar (char '.')
  return $ Affectation y (read r :: Int) (T.pack town) (T.strip . T.pack $ spe)


formatCSV :: [Affectation] -> T.Text
formatCSV l = T.unlines $ "annee;rang;ville;specialite" : map printAffect l

getYear' :: String -> IO T.Text
getYear' root = do
  r <- get $ "https://www.legifrance.gouv.fr/jorf/id/"++  root
  let d = r ^. responseBody
  let s = innerText . (Prelude.take 50) . skipToContent $ parseTags d
  return $ toStrict . decodeUtf8 $ s

getYear :: String -> IO T.Text
getYear root = do
  r <- get $ "https://www.legifrance.gouv.fr/jorf/id/"++  root
  let d = r ^. responseBody
  let s = innerText . skipToContent $ parseTags d
  return $ toStrict . decodeUtf8 $ s

-- main :: IO ()
affectYear :: (Int, String) -> IO [Affectation]
affectYear (y, root) = do
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
  -- all <- affectYear 2020 "JORFTEXT000042402100"
  let years = [
        -- (2020, "JORFTEXT000042402100")
        -- , (2019, "JORFTEXT000039229737")
        -- , (2018, "JORFTEXT000037523753" )
        -- (2017, "JORFTEXT000035871907" )
        (2016, "JORFTEXT000033253978")
        -- , (2015, "JORFTEXT000031314070")
        -- , (2014, "JORFTEXT000029604463")
        -- , (2013, "JORFTEXT000028160771")
        -- , (2012, "JORFTEXT000026872409")
        -- , (2011, "JORFTEXT000024846862")
        -- , (2010, "JORFTEXT000023100415")]
        ]
  print years
  l <- mapM affectYear years
  -- liftIO length l

-- TIO.writeFile "docs/2019.csv" $ formatCSV all
  return ()
  -- print "ok"
