import Test.Hspec
import Lib
import Text.Printf

main :: IO ()
main = hspec $ do 
    describe "verify genJSON" $ do
        it "returns IO (Left ErrorMsg) when the file does not exist" $ do
          let badFilePath = "data/csv-data/invalid.csv"
          let errorMessage = printf "The file %s does not exist" badFilePath
          genJSON badFilePath `shouldReturn` Left errorMessage

        it "returns IO (Left ErrorMsg) if volume for particular trade is set to string like abc" $ do
            let badFilePath = "data/csv-data/trade-matches-bad1.csv"
            let errorMessage = "parse error (Failed reading: conversion error: expected Double, got \"abc\" (Failed reading: takeWhile1)) at \"\""
            genJSON badFilePath `shouldReturn` Left errorMessage

        it "returns IO (Left ErrorMsg) if there is a missing field" $ do
            let badFilePath = "data/csv-data/trade-matches-bad2.csv"
            let errorMessage = "parse error (Failed reading: conversion error: not enough fields) at \"\\nInGen Tech BCZ232,BioSynFG332,BUSM2,Bid,43250\""
            genJSON badFilePath `shouldReturn` Left errorMessage

    describe "verify symbolToProductJSON method" $ do
        it "symbolToProductJSON returns valid json string for given trade record" $ do
            let sampleTrade1 = TradeData{makerAccountId= "Tyrell Corp A123", takerAccountId= "Wayland-Yutani Corp BC32", productSymbol= "BUSU1", takerSide= "Bid", price= 42, quantity= 100}
            let sampleTrade2 = TradeData{makerAccountId= "CHOAM Arakis Z23", takerAccountId= "OPEC 897", productSymbol= "BUIZ1", takerSide= "Ask", price= -2, quantity= 14}
            let sampleTrade3 = TradeData{makerAccountId= "InGen Tech BCZ232", takerAccountId= "BioSynFG332", productSymbol= "BUSM2", takerSide= "Bid", price= 43250, quantity= 23}
            let jsonOutput1 = "{\"BUSU1\":{\"vwap\":42.0,\"volume\":100}}\n"
            let jsonOutput2 = "{\"BUIZ1\":{\"vwap\":-2.0,\"volume\":14}}\n"
            let jsonOutput3 = "{\"BUSM2\":{\"vwap\":43250.0,\"volume\":23}}\n"
            symbolToProductJSON sampleTrade1 `shouldBe` jsonOutput1
            symbolToProductJSON sampleTrade2 `shouldBe` jsonOutput2
            symbolToProductJSON sampleTrade3 `shouldBe` jsonOutput3

    describe "verify safediv" $ do
         it "should return Nothing if div by zero" $ do
            safediv 9 0 `shouldBe` Nothing
            safediv 0 9 `shouldBe` Just 0.0
            safediv 0 0 `shouldBe` Nothing