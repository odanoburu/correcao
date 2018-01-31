module Correcao where

import Data.Maybe
import Data.Time
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.CSV
import Text.Printf

-- TODO: add writer monad to have a log
-- TODO: scanl in isTemporal?
-- TODO: have isTemporal say where the it breaks

---
-- types
type CLine = [Corr]
type Corr = (Day, Double)

---
-- read CSV
readCSVWith :: (CSV -> [a]) -> FilePath -> IO [a]
readCSVWith f fp = do
  res <- parseCSVFromFile fp
  case res of
    Left err -> do
      print err
      return []
    Right csv -> return $ f csv

readMasterFile :: FilePath -> IO [FilePath]
-- reads configuration file
readMasterFile = readCSVWith (concat . tail)

readVariaCSV :: FilePath -> IO CLine
readVariaCSV = readCSVWith csvToCcsv

readPercCSV :: FilePath -> IO CLine
readPercCSV = readCSVWith (percToVariationCL . csvToCcsv)

readIndexCSV :: FilePath -> IO CLine
readIndexCSV = readCSVWith (indexToVariationCL . csvToCcsv)

readCcsv :: FilePath -> IO CLine
readCcsv fp =
  let ext = takeExtension fp
  in case ext of
       ".index" -> readIndexCSV fp
       ".perc" -> readPercCSV fp
       ".varia" -> readVariaCSV fp
       _ -> do
         putStrLn
           "extensão desconhecida. você deve indicar na extensão do arquivo o tipo de dado que ele contém."
         return []
  
---
-- time
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%F"

oneMonthForward :: Day -> Day -> Bool
oneMonthForward pd fd =
  let fd' = addGregorianMonthsClip 1 pd
  in sameMonth fd' fd

sameMonth :: Day -> Day -> Bool
sameMonth d1 d2 =
  let (y1,m1,_) = toGregorian d1
      (y2,m2,_) = toGregorian d2
  in m1 == m2 && y1 == y2

isTemporalL :: [Day] -> Bool
isTemporalL (d1:d2:dt) = oneMonthForward d1 d2 && isTemporalL (d2:dt)
isTemporalL _ = True

clineTemporal :: CLine -> Bool
clineTemporal = isTemporalL . map fst

---
-- conversion
csvToCcsv :: CSV -> CLine
csvToCcsv = map recordToC . tail

recordToC :: Record -> Corr
recordToC r =
  let [ds, is] = r
  in (fromJust $ parseDate ds, read is :: Double)

-- convert index csv's to variation csv's
indexToVariationCL :: CLine -> CLine
indexToVariationCL c = zipWith indexToVariation c (tail c)

indexToVariation :: Corr -> Corr -> Corr
indexToVariation (_, pi) (fd, fi) =
  let var = fi / pi
  in (fd, var)

percToVariationCL :: CLine -> CLine
percToVariationCL = map percToVariation

percToVariation :: Corr -> Corr
percToVariation (d,v) = (d, 1 + v/100)

---
-- correction line
absoluteCLPaths :: FilePath -> [FilePath] -> [FilePath]
absoluteCLPaths fp = map (fp </>)

getAbsPathsInMaster :: FilePath -> IO [FilePath]
getAbsPathsInMaster fp = do
  fps <- readMasterFile fp
  let dir = takeDirectory fp
      afps = absoluteCLPaths dir fps
  return afps

readCLine :: [FilePath] -> IO CLine
readCLine fs = fmap concat $ mapM readCcsv fs

dropPrevious :: CLine -> Day -> CLine
dropPrevious l d = dropWhile (not . oneMonthForward d . fst) l

applyCorrL :: CLine -> Double -> Double
applyCorrL l v = foldr applyCorr v l

applyCorr :: Corr -> Double -> Double
applyCorr (_,i) v = i * v

correctValue :: CLine -> Day -> Double -> Double
correctValue l d v =
  let l' = dropPrevious l d
  in applyCorrL l' v

---
-- main
main :: IO ()
main = do
  [mfp, vfp] <- getArgs
  afps <- getAbsPathsInMaster mfp
  l <- readCLine afps
  vs <- readVariaCSV vfp
  putStr $
    unlines $ map (\(d, v) -> printMoney $ correctValue l d v) vs

---
-- utils
printMoney :: Double -> String
printMoney v = printf "%.2f" v :: String
