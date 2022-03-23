{-# LANGUAGE RecordWildCards #-} -- required to use wildcard pattern in toJSON instance def
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( genJSON
    , symbolToProductJSON
    , csvToJson
    , csvToJson'
    , safediv
    , TradeData(..)
    ) where

import Data.Text
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics (Generic)
import Data.Aeson as A
import Data.Vector as V
import Data.Csv as C
import Data.Map as M
import Data.Csv.Streaming as CS 
import Data.Foldable as F
import Control.Monad
import System.Directory (doesFileExist)
import Text.Printf

-- Data Type to model trade 
data TradeData = TradeData
    { makerAccountId ::  Text
    , takerAccountId ::  Text
    , productSymbol ::  Text
    , takerSide ::  Text
    , price ::  Double
    , quantity ::  Double
    } deriving (Show)

-- Data Type to model product report
data ProductReport = ProductReport
    { vwap :: Maybe Double
    , volume :: Double
    } deriving (Show,Generic)

{-
Implementing FromRecord type class to decode csv
if min no of columns/fields not present, return fail
don't care about additional columns, only evaluate first 6
parseRecord can be further enhanced to validate specific data, but skipping this due to time & scope of exercise
-}
instance FromRecord TradeData where
    parseRecord rec
        | V.length rec >= 6 = TradeData <$>
                              rec .! 0 <*>
                              rec .! 1 <*>
                              rec .! 2 <*>
                              rec .! 3 <*>
                              rec .! 4 <*>
                              rec .! 5
        | V.length rec < 6    = fail "not enough fields"
        | otherwise     = mzero

-- Implementing ToJSON type class to encode product report
-- Implemented toEncoding method to strip null values, set order of keys and round off volume
instance ToJSON ProductReport where
    toJSON ProductReport{..} =
        object [ "vwap" A..= vwap
                , "volume" A..= volume
                ]
    toEncoding ProductReport{..} = pairs ("vwap" A..= stripNull vwap <> "volume" A..= (round volume :: Int))

-- Helper method to strip null value in json object
stripNull :: Maybe Double -> Double
stripNull maybeval = case maybeval of 
                  Nothing -> 0
                  Just x -> x

-- symbolToProductJSON takes TradeData and generates map of {symbol->product report} 
-- {symbol->product report} map is then returned as json encoded bytestring
symbolToProductJSON :: TradeData -> ByteString
symbolToProductJSON tradedata =  (A.encode symbolToProductMap) <> "\n" --encode to json and append newline
    where
        productsymbol = productSymbol tradedata
        tradePrice = price tradedata
        tradeQty = quantity tradedata
        vwap = safediv (tradePrice * tradeQty) tradeQty
        productreport = ProductReport {vwap= vwap, volume= tradeQty}
        symbolToProductMap = M.insert productsymbol productreport M.empty

-- Helper method to check and handle divide by zero 
safediv :: Double -> Double -> Maybe Double
safediv n m = if m == 0 then Nothing else Just (n / m)
    
-- parseCsv will read csv filename, return error if filepath is invalid/does not exist
parseCsv :: FilePath -> IO (Either String (V.Vector TradeData))
parseCsv filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then C.decode NoHeader <$> BL.readFile filePath
    else return . Left $ printf "The file %s does not exist" filePath

-- genJSON will apply symbolToProductJSON method to transform TradeData in Vector to json ByteString
genJSON :: FilePath -> IO (Either String (Vector ByteString))
genJSON filePath =
  (fmap . fmap .fmap) (symbolToProductJSON) (parseCsv filePath)

-- csvToJson will apply genJSON method to transform csv to json and write to file
csvToJson :: String -> String -> IO ()
csvToJson csvFile jsonFile =  do
    file <- genJSON csvFile 
    case file of
        Left err -> print err
        Right xs -> V.forM_ xs $ \x -> BL.appendFile jsonFile x

-- csvToJson' is optimised version of csvToJson, uses the csv streaming lib to process 1 record at a time
-- csvToJson' will ignore/skip if any erros in csv line
-- Used Foldable to map symbolToProductJSON over the Records data type
csvToJson' :: String -> String -> IO ()
csvToJson' csvFile jsonFile =  do
    file <- BL.readFile csvFile
    let ls = CS.decode NoHeader file :: Records TradeData
    let ps = fmap (symbolToProductJSON) ls
    F.mapM_ (BL.appendFile jsonFile) ps