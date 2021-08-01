{-| Domain model for document generation

   * Contract data types
   * Risk number data types

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}

module KID.Document.Domain
  where

import           Control.Lens.TH
import           Data.Aeson.TH
import           Data.Data
import           Data.Text
import           Data.Time.Calendar

-- | Supported languages
data Language =
    DE
  | FR
  | IT
  | EN
  deriving (Read, Show, Enum, Bounded, Data)

-- | Supported currencies
data Currency =
    CHF
  | EUR
  | USD
  deriving (Read, Show, Enum, Bounded)

-- | Supported product types
data ProductType =
    SampleContract1
  | SampleContract2 {
      _barrier      :: Double
    , _strike       :: Double
    , _denomination :: Double
  }
  deriving (Eq, Read, Show)

-- | Cost parameters
data Costs = Costs {
    _standard_investment_amount :: Double -- ^ standard investment amount (10k)
  , _entry                      :: Double -- ^ one off entry costs
  , _exit                       :: Double -- ^ one off exit costs
  , _portfolio                  :: Double -- ^ ongoing portfoltio costs
  , _other                      :: Double -- ^ ongoing other costs
  , _performance                :: Double -- ^ incidental performance costs
  , _carried                    :: Double -- ^ incidental carried intrest costs
  }

-- | Underlying specification
data Underlying = Underlying {
    _underlying_name          :: Text
  , _underlying_instrument_id :: InstrumentId
  , _weight                   :: Double
  } deriving (Show)

-- | Instrument identifier
data InstrumentId =
  QuandlId {
    _database_code :: Text
  , _dataset_code  :: Text
  , _dataset_field :: Text
  } |
  DummyData
  deriving (Show)

-- | Issuer specification
data Issuer = Issuer {
    _issuer_name :: Text
  , _issuer_web  :: Text
  }

-- | Contract definition
data Contract = Contract {
    _product_name     :: Text
  , _product_currency :: Currency
  , _product_type     :: ProductType
  , _instrument_id    :: [InstrumentId]
  , _underlyings      :: [Underlying]
  , _issuer           :: Issuer
  , _issue_date       :: Day
  , _redemption_date  :: Day
  , _costs            :: Costs
  }

-- | Risk scenarios
data Scenario = Scenario {
    _favourable   :: Double
  , _moderate     :: Double
  , _unfavourable :: Double
  , _stress       :: Double
} deriving (Show)

-- | Risk summary
data RiskSummary = RiskSummary {
    _var               :: Double         -- ^ Value at risk
  , _vev               :: Double         -- ^ VaR equivalent volatility
  , _mrm               :: Int            -- ^ Market risk measure
  , _sri               :: Int            -- ^ Summary risk indicator
  , _rhp               :: Double         -- ^ Recommended holding period
  , _scenario_rhp      :: Scenario       -- ^ Scenario at rhp
  , _scenario_rhp_half :: Maybe Scenario -- ^ Scenario at half of the rhp
  , _scenario_one_year :: Maybe Scenario -- ^ Scenario after one year
} deriving (Show)

-- | Terms for the KID
type Terms = (Contract, RiskSummary)

-- === Lens instances

makeLenses ''Costs
makeLenses ''Contract
makeLenses ''Issuer
makeLenses ''Scenario
makeLenses ''RiskSummary

makePrisms ''Scenario
makePrisms ''RiskSummary

-- === JSON instances

deriveJSON defaultOptions ''InstrumentId
deriveJSON defaultOptions ''Underlying
deriveJSON defaultOptions ''Costs
deriveJSON defaultOptions ''ProductType
deriveJSON defaultOptions ''Currency
deriveJSON defaultOptions ''Issuer
deriveJSON defaultOptions ''Contract
deriveJSON defaultOptions ''Scenario
deriveJSON defaultOptions ''RiskSummary

