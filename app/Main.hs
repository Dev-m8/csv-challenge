module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    putStrLn "Enter input csv file:"
    csvFile <- getLine
    putStrLn "Enter output json file:"
    jsonFile <- getLine
    putStrLn "Enter CSV process method:\n  [options]\n    -s    standard\n    -o    optimised\n"
    method <- getLine
    case method of 
        "-s" -> csvToJson csvFile jsonFile >> putStrLn "Done"
        "-o" -> csvToJson' csvFile jsonFile >> putStrLn "Done"
        _ -> putStrLn "invalid input provided, try again!" >> main


