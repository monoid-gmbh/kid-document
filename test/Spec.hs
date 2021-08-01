{-# LANGUAGE OverloadedStrings #-}
module Main
  where

import           Control.Monad.Identity
import           Data.Aeson
import qualified Data.ByteString        as SB
import           Data.Maybe
import           Data.Text              hiding (head, map)
import           Data.Text.Encoding     (encodeUtf8)
import           Data.Text.Read
import           Data.Time.Calendar
import           KID.Document.Template
import           KID.Document.Domain
import           Text.Pandoc
import           Text.Pandoc.Walk

import           Data.Aeson.Text        (encodeToLazyText)
import           Data.Text.Lazy.IO      as I

main :: IO ()
main = I.writeFile "myfile.json" (encodeToLazyText (sampleContract, sampleRisk))

sampleIssuer :: Issuer
sampleIssuer = Issuer "testname" "http://testname.bank"

sampleContract :: Contract
sampleContract = Contract "testname" CHF SampleContract1 [] [sampleUnderlying1, sampleUnderlying2] sampleIssuer d d sampleCosts
  where d = fromGregorian 2020 5 11

sampleCosts :: Costs
sampleCosts = Costs 10000 0.23 0.54 1.1 0.0 0.0 0.6

sampleUnderlying1 :: Underlying
sampleUnderlying1 = Underlying "test1" instrumentId 0.5
  where instrumentId = DummyData

sampleUnderlying2 :: Underlying
sampleUnderlying2 = Underlying "test2" instrumentId 0.5
  where instrumentId = DummyData

sampleRisk :: RiskSummary
sampleRisk = RiskSummary 1.2 2.3 3 4 0.0 sampleScenario Nothing (Just sampleScenario)

sampleScenario :: Scenario
sampleScenario = Scenario 1.0 2.0 3.0 4.0
