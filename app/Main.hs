{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Lens
import Data.Either
import Data.List
import Data.Text.Lazy.Encoding
import Data.Text.Lazy.Read
import Data.Text.Lazy.Read
import GHC.Generics
import Network.Wreq
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as TIO

-- | Read minimal rank from CNG data.
-- | The CNG has data as HTML tables where a row is a region and a column a specialty

data Affectation = Affectation {
  year :: Int,
  rank :: Int,
  region :: T.Text,
  specialty :: T.Text
} deriving (Show)

printAffect (Affectation y r t s) = T.intercalate ";" [T.pack . show $ y, T.pack . show $ r, t, s]

noNewLine = T.replace "\n" ""

-- | Cell format (inside the <a> tag) :
-- > MIN
-- > <br>
-- > MAX
-- > <span>
-- > dispo: X
-- > <br>
-- > place : X
-- > <br>
-- > offre : X
-- > <hr>
-- > REGION
-- > <br>
-- > SPECIALITY
-- > </br></scan>
-- Some cells may be empty (with non-unicode characters (&nbsp; in html)
parseCell :: Int -> [Tag BL.ByteString] -> Either String Affectation
parseCell year x
  | fromTagText (head c) ==  CL.pack "\160\160\160\160" = Left "Empty cell"
  | otherwise = parseCell' year c
  where c = filter isTagText x

-- | Parse cell when not empty

parseCell' year c = Right $ Affectation year rank region spe
  where
    rank = case decimal (f 1) of
             Left _ -> 0
             Right (x, _) -> x
    region = normalizeRegion (f 5)
    spe = normalizeSpe . T.toTitle $ f 6
    f x = T.strip . noNewLine . decodeUtf8 . cleanSpaces . fromTagText $ c !! x
    
-- | Cannot decode the combination A\160 finto utf8 (maybe encoded in latin 1 ?)
-- Don't use C.pack with accents..
cleanSpaces =  BLS.replace (C.pack "A\160") (C.pack "A ")

parseCells year =  map (parseCell year) . sections (~== ("<a class=limite>" :: String))

formatCSV :: [Affectation] -> T.Text
formatCSV l = T.unlines $ "annee;rang;ville;specialite" : map printAffect l

parseYear year = do
  d <- BL.readFile $ "data/" ++ show year ++ ".html"
  return $ rights . parseCells year . parseTags $ d

-- | Normalize regions before and after 2018
-- | Before 2018, remove CHU 
normalizeRegion :: T.Text -> T.Text
normalizeRegion = normalizeRegion' . T.strip . T.toTitle . chu .T.toTitle
  where chu = T.replace "Chu" "" . T.replace "Chu D'" "" . T.replace "Chu De" ""

normalizeRegion' "Aix Marseille" = "Marseille"
normalizeRegion' "Antilles-guyane" =  "Martinique"
normalizeRegion' "Ap-hm" = "Marseille"
normalizeRegion' "Ap-hp" = "Paris"
normalizeRegion' "Clermont-ferrand" = "Clermont-Ferrand"
normalizeRegion' "Hcl" =  "Lyon"
normalizeRegion' "Ile De France" = "Paris"
normalizeRegion' "La Reunion" = "La Réunion"
normalizeRegion' "La\160Reunion" = "La Réunion"
normalizeRegion' "Martinique/pointe A Pitre" = "Martinique"
normalizeRegion' "Martinique/pointe A\160Pitre" = "Martinique"
normalizeRegion' "Martinique/pointe À Pitre" = "Martinique"
normalizeRegion' "Ocean Indien" = "La Réunion"
normalizeRegion' "Besancon" = "Besançon"
normalizeRegion' x = x -- T.toTitle x

-- | Normalize specialty
normalizeSpe "Anesthésie Réanimation" = "Anesthésie-réanimation"
normalizeSpe "Anatomie Et Cytologie Pathologiques" = "Anatomie Et Cytologie Pathologique"
normalizeSpe "Dermatologie Et Vénérologie" = "Dermatologie Et Vénéréologie"
normalizeSpe "Endocrinologie, Diabète, Maladies Métaboliques" = "Endocrinologie-diabétologie-nutrition"
normalizeSpe "Gastro-entérologie Et Hépatologie" = "Hépato-gastro-entérologie"
normalizeSpe "Médecine Interne" = "Médecine Interne Et Immunologie Clinique"
normalizeSpe "Oto-rhino-laryngologie - Chirurgie Cervico-faciale" = "ORL"
normalizeSpe "Oto-rhino-laryngologie Et Chirurgie Cervico-faciale" = "ORL"
normalizeSpe "Radiodiagnostic Et Imagerie Médicale" = "Radiologie Et Imagerie Médicale"
normalizeSpe "Médecine Du Travail" = "Médecine Et Santé Au Travail"
normalizeSpe x = x

root = "https://www.cng.sante.fr/sites/default/files/"

-- | Each cell in the table has the rank, the region and the specialy, so we only have to parse all cells
main = do
  let years = [2015 ..2020]
  s <- mapM parseYear years
  TIO.writeFile "data/ranks.csv" $ formatCSV (concat s)
  return $ nub . sort . map region $ concat s
  
test = do
  -- r <- get (root ++ "tableau/rangs_limites_2017.html")
  -- let d = r ^. responseBody
  d <- BL.readFile "2019.html"
  return $  sections (~== ("<a class=limite>" :: String)) . parseTags $ d

  
