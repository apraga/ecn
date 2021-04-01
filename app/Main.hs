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
               , " en " -- Region (2016). Warning : space is improtant to avoid matching entérologie
               ]

-- Version without newlines
-- parseAffects :: Int -> Parser [Affectation]
startDelim y = T.concat ["au titre de l'année universitaire "
                        , T.pack $ show y
                        , "-", T.pack $ show (y+1), " :"]
parseAffects y = do
  _ <- manyTill anyChar (string $ startDelim y)
  many (parseAffect y)

-- Haromnize data between 2016 and more recent years
cleanTown :: T.Text -> T.Text
cleanTown "HCL" = "Lyon"
cleanTown "Ile-de-France" = "Paris"
cleanTown "AP-HP" = "Paris"
cleanTown "AP-HM" = "Marseille"
cleanTown x = x

-- Haromnize data between 2016 and more recent years
cleanSpe :: T.Text -> T.Text
cleanSpe "gastro-entérologie et hépatologie" = "hépato-gastro-entérologie"
cleanSpe "cardiologie et maladies vasculaires" = "médecine cardiovasculaire"
cleanSpe "anesthésie réanimation" = "anesthésie-réanimation"
cleanSpe "dermatologie et vénérologie" = "dermatologie et vénéréologie"
cleanSpe "radiodiagnostic et imagerie médicale" = "radiologie et imagerie médicale"
cleanSpe "médecine interne" = "médecine interne et immunologie clinique"
cleanSpe "oto-rhino-laryngologie et chirurgie cervico-faciale" = "oto-rhino-laryngologie - chirurgie cervico-faciale"
cleanSpe "endocrinologie, diabète, maladies métaboliques" = "endocrinologie-diabétologie-nutrition"
cleanSpe "anatomie et cytologie pathologique" = "anatomie et cytologie pathologiques"
cleanSpe x = x

parseAffect :: Int -> Parser Affectation
parseAffect y = do
  r <- some digit
  skipSpace
  "M." <|> "Mme" <|> "Mlle" <|> "MME" <|> "mlle"
  _ <- manyTill anyChar (char '(')
  _ <- manyTill anyChar (char ')')
  char ','
  _ <- option "" $ skipSpace *> "nom d'usage " *> manyTill anyChar (char ',')
  _ <- option "" $ skipSpace *> "épouse " *> manyTill anyChar (char ',')
  _ <- option "" $ skipSpace *> "famille" *> manyTill anyChar (char ',')
  _ <- option "" $ skipSpace *> ("né" <|> "née") *> " le " *> manyTill anyChar (char ',')
  -- char ','
  skipSpace
  spe <- manyTill anyChar delim
  skipSpace
  town <- manyTill anyChar (char '.')
  let town' =cleanTown (T.pack town)
  let spe' = cleanSpe (T.strip . T.pack $ spe)
  return $ Affectation y (read r :: Int) town' spe'

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
  let years = [
        (2020, "JORFTEXT000042402100")
        , (2019, "JORFTEXT000039229737")
        , (2018, "JORFTEXT000037523753" )
        , (2017, "JORFTEXT000035871907" )
        , (2016, "JORFTEXT000033253978")
        -- useless after that
        ]
  print years
  l <- mapM affectYear years
  let l' = concat l
  -- liftIO length l

  TIO.writeFile "docs/raw.csv" $ formatCSV l'
  -- TODO: merge town name
  return ()
  -- print "ok"
