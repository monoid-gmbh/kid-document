{-| Template composition -}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}

module KID.Document.Template
  where

import           Control.Lens
import qualified Data.ByteString     as SB
import           Data.Char           (toLower)
import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Text.Encoding  (encodeUtf8)
import           Data.Text.Read      (decimal)
import           Data.Time.Calendar
import           KID.Document.Domain
import           KID.Document.Loader
import           Text.Pandoc
import           Text.Pandoc.Builder
import           Text.Pandoc.Walk
import           Text.Printf

-- === Template Definition

-- | `generateTemplate` build the template
generateTemplate :: Language -- ^ The Language of the template
                 -> Pandoc   -- ^ The template
generateTemplate =
     section1  -- Purpose
  <> section2  -- Product
  <> section3  -- What is this product
  <> section4  -- What are the risks and what could I get in return
  <> section5  -- What happens if the issuer is unable to pay out
  <> section6  -- What are the costs
  <> section7  -- How long should I hold it and can I take money out early
  <> section8  -- How can I complain
  <> section9  -- Other relevant information

-- | Section 1, Purpose
section1 :: Language -> Pandoc
section1 DE = [mdFileQQ|src/KID/Document/Template/section1_de.md|]
section1 FR = [mdFileQQ|src/KID/Document/Template/section1_fr.md|]
section1 IT = [mdFileQQ|src/KID/Document/Template/section1_it.md|]
section1 EN = [mdFileQQ|src/KID/Document/Template/section1_en.md|]

-- | Section 2, Product text block
section2 :: Language -> Pandoc
section2 DE = [mdFileQQ|src/KID/Document/Template/section2_de.md|]
section2 FR = [mdFileQQ|src/KID/Document/Template/section2_fr.md|]
section2 IT = [mdFileQQ|src/KID/Document/Template/section2_it.md|]
section2 EN = [mdFileQQ|src/KID/Document/Template/section2_en.md|]

-- | Section 3, What is this product
section3 :: Language -> Pandoc
section3 DE = [mdFileQQ|src/KID/Document/Template/section3_de.md|]
section3 FR = [mdFileQQ|src/KID/Document/Template/section3_fr.md|]
section3 IT = [mdFileQQ|src/KID/Document/Template/section3_it.md|]
section3 EN = [mdFileQQ|src/KID/Document/Template/section3_en.md|]

-- | Section 4, What are the risks and what could I get in return
section4 :: Language -> Pandoc
section4 = section4Template <> section4PerformanceTable
  where
    section4Template :: Language -> Pandoc
    section4Template DE = [mdFileQQ|src/KID/Document/Template/section4_de.md|]
    section4Template FR = [mdFileQQ|src/KID/Document/Template/section4_fr.md|]
    section4Template IT = [mdFileQQ|src/KID/Document/Template/section4_it.md|]
    section4Template EN = [mdFileQQ|src/KID/Document/Template/section4_en.md|]

    section4PerformanceTable :: Language -> Pandoc
    section4PerformanceTable DE = [htmlFileQQ|src/KID/Document/Template/section4_performance_scenarios_de.html|]
    section4PerformanceTable FR = [htmlFileQQ|src/KID/Document/Template/section4_performance_scenarios_fr.html|]
    section4PerformanceTable IT = [htmlFileQQ|src/KID/Document/Template/section4_performance_scenarios_it.html|]
    section4PerformanceTable EN = [htmlFileQQ|src/KID/Document/Template/section4_performance_scenarios_en.html|]

-- | Section 5, What happens if the issuer is unable to pay out
section5 :: Language -> Pandoc
section5 DE = [mdFileQQ|src/KID/Document/Template/section5_de.md|]
section5 FR = [mdFileQQ|src/KID/Document/Template/section5_fr.md|]
section5 IT = [mdFileQQ|src/KID/Document/Template/section5_it.md|]
section5 EN = [mdFileQQ|src/KID/Document/Template/section5_en.md|]

-- | Section 6, What are the costs
section6 :: Language -> Pandoc
section6 = section6Template <> compositonOfCosts
  where
    section6Template :: Language -> Pandoc
    section6Template DE = [mdFileQQ|src/KID/Document/Template/section6_de.md|]
    section6Template FR = [mdFileQQ|src/KID/Document/Template/section6_fr.md|]
    section6Template IT = [mdFileQQ|src/KID/Document/Template/section6_it.md|]
    section6Template EN = [mdFileQQ|src/KID/Document/Template/section6_en.md|]

    compositonOfCosts :: Language -> Pandoc
    compositonOfCosts DE = [htmlFileQQ|src/KID/Document/Template/section6_composition_of_costs_de.html|]
    compositonOfCosts FR = [htmlFileQQ|src/KID/Document/Template/section6_composition_of_costs_fr.html|]
    compositonOfCosts IT = [htmlFileQQ|src/KID/Document/Template/section6_composition_of_costs_it.html|]
    compositonOfCosts EN = [htmlFileQQ|src/KID/Document/Template/section6_composition_of_costs_en.html|]

-- | Section 7, How long should I hold it and can I take money out early
section7 :: Language -> Pandoc
section7 DE = [mdFileQQ|src/KID/Document/Template/section7_de.md|]
section7 FR = [mdFileQQ|src/KID/Document/Template/section7_fr.md|]
section7 IT = [mdFileQQ|src/KID/Document/Template/section7_it.md|]
section7 EN = [mdFileQQ|src/KID/Document/Template/section7_en.md|]

-- | Section 8, How can I complain?
section8 :: Language -> Pandoc
section8 = section8Template <> addrDoc
  where
    section8Template :: Language -> Pandoc
    section8Template DE = [mdFileQQ|src/KID/Document/Template/section8_de.md|]
    section8Template FR = [mdFileQQ|src/KID/Document/Template/section8_fr.md|]
    section8Template IT = [mdFileQQ|src/KID/Document/Template/section8_it.md|]
    section8Template EN = [mdFileQQ|src/KID/Document/Template/section8_en.md|]

    addrDoc :: Language -> Pandoc
    addrDoc _ = doc $ singleton (Plain [Span attr []])
      where attr = ("Issuer", [], [])

-- | Section 9, Other relevant information
section9 :: Language -> Pandoc
section9 DE = [mdFileQQ|src/KID/Document/Template/section9_de.md|]
section9 FR = [mdFileQQ|src/KID/Document/Template/section9_fr.md|]
section9 IT = [mdFileQQ|src/KID/Document/Template/section9_it.md|]
section9 EN = [mdFileQQ|src/KID/Document/Template/section9_en.md|]

-- ==== Import and export template

-- | export template
exportNative :: Language
             -> Pandoc
             -> IO ()
exportNative l d = case runPure $ writeNative cfg d of
    Left e  -> print e
    Right b -> let s = map toLower (show l) in SB.writeFile ("app/etc/kid_" <> s <> ".native") (encodeUtf8 b)
  where
    cfg :: WriterOptions
    cfg = def {writerReferenceLinks = True, writerExtensions = pandocExtensions, writerWrapText = WrapNone}

-- | update templates
writeTemplate :: IO ()
writeTemplate  = mapM_ (exportNative <*> generateTemplate) languages
  where
    languages = [minBound .. maxBound] :: [Language]

-- | `replaceSpan` substutes Span elements -- TODO: special tag for Spans to be replaced
-- To be used as pandoc-filter, in order to replace the template nodes
replaceSpan :: Language -- ^ Language
            -> Terms    -- ^ Terms
            -> Inline   -- ^ Template document
            -> Inline   -- ^ Target document

-- Section 2
replaceSpan _ (c,_) (Span ("ProductName", _, _) _) = Str $ c ^. product_name
replaceSpan _ (c,_) (Span ("Issuer", _, _) _)      = Str $ c ^. issuer . issuer_name

-- Section 4
replaceSpan _ (_,r) (Span ("sri", _, _) _) = Str . format $ r ^. sri

replaceSpan _ (_,r) (Span ("strs_one_year"    , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . stress
replaceSpan _ (_,r) (Span ("strs_rhp_half"    , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . stress
replaceSpan _ (_,r) (Span ("strs_rhp"         , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . stress

replaceSpan _ (_,r) (Span ("ufav_one_year"    , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . unfavourable
replaceSpan _ (_,r) (Span ("ufav_rhp_half"    , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . unfavourable
replaceSpan _ (_,r) (Span ("ufav_rhp"         , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . unfavourable

replaceSpan _ (_,r) (Span ("mod_one_year"     , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . moderate
replaceSpan _ (_,r) (Span ("mod_rhp_half"     , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . moderate
replaceSpan _ (_,r) (Span ("mod_rhp"          , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . moderate

replaceSpan _ (_,r) (Span ("fav_one_year"     , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . favourable
replaceSpan _ (_,r) (Span ("fav_rhp_half"     , cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . favourable
replaceSpan _ (_,r) (Span ("fav_rhp"          , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . favourable

replaceSpan _ (_,r) (Span ("strs_one_year_ret", cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . stress
replaceSpan _ (_,r) (Span ("strs_rhp_half_ret", cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . stress
replaceSpan _ (_,r) (Span ("strs_rhp_ret"     , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . stress

replaceSpan _ (_,r) (Span ("ufav_one_year_ret", cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . unfavourable
replaceSpan _ (_,r) (Span ("ufav_rhp_half_ret", cl, _) _) = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . unfavourable
replaceSpan _ (_,r) (Span ("ufav_rhp_ret"     , cl, _) _) =              formatDouble cl  $ r ^. scenario_rhp              . unfavourable

replaceSpan _ (_,r) (Span ("mod_one_year_ret", cl, _) _)  = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . moderate
replaceSpan _ (_,r) (Span ("mod_rhp_half_ret", cl, _) _)  = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . moderate
replaceSpan _ (_,r) (Span ("mod_rhp_ret"     , cl, _) _)  =              formatDouble cl  $ r ^. scenario_rhp              . moderate

replaceSpan _ (_,r) (Span ("fav_one_year_ret", cl, _) _)  = maybe Space (formatDouble cl) $ r ^? scenario_one_year . _Just . favourable
replaceSpan _ (_,r) (Span ("fav_rhp_half_ret", cl, _) _)  = maybe Space (formatDouble cl) $ r ^? scenario_rhp_half . _Just . favourable
replaceSpan _ (_,r) (Span ("fav_rhp_ret"     , cl, _) _)  =              formatDouble cl  $ r ^. scenario_rhp              . favourable

-- Section 6
replaceSpan _ (c,_) (Span ("entry_costs"                , cl, _) _) = formatDouble cl  $  c ^. costs . entry
replaceSpan _ (c,_) (Span ("exit_costs"                 , cl, _) _) = formatDouble cl  $  c ^. costs . exit
replaceSpan _ (c,_) (Span ("portfolio_transaction_costs", cl, _) _) = formatDouble cl  $  c ^. costs . portfolio
replaceSpan _ (c,_) (Span ("other_ongoing_costs"        , cl, _) _) = formatDouble cl  $  c ^. costs . other
replaceSpan _ (c,_) (Span ("performance_fees"           , cl, _) _) = formatDouble cl  $  c ^. costs . performance
replaceSpan _ (c,_) (Span ("carried_interest"           , cl, _) _) = formatDouble cl  $  c ^. costs . carried

-- Section 7
replaceSpan _ (c,_) (Span ("RedemptionDate", _, _) _) = Str . formatDay $ c ^. redemption_date

-- Otherwise
replaceSpan _ _ x = x

-- | replace figure
replaceFig :: Inline -> Inline
replaceFig (Span ("fig", cs, attr) is) =
  fromMaybe Space $ do
    h <- lookup "higher" attr
    l <- lookup "lower" attr
    return $ head $ map (maybeDecimal h l) is
  where
    maybeDecimal :: T.Text -> T.Text -> Inline -> Inline
    maybeDecimal h l (Str t) = case decimal t of
      Left _      -> Str $ l <> sriTxt 7 <> h
      Right (i,_) -> Str $ l <> sriTxt i <> h
    maybeDecimal h l x = x

    sriTxt :: Int -> T.Text
    sriTxt 1 = "   <   |1|    2     3     4     5     6     7    >   "
    sriTxt 2 = "   <    1    |2|    3     4     5     6     7    >   "
    sriTxt 3 = "   <    1     2    |3|    4     5     6     7    >   "
    sriTxt 4 = "   <    1     2     3    |4|    5     6     7    >   "
    sriTxt 5 = "   <    1     2     3     4    |5|    6     7    >   "
    sriTxt 6 = "   <    1     2     3     4     5    |6|    7    >   "
    sriTxt _ = "   <    1     2     3     4     5     6    |7|   >   "

replaceFig x = x

-- === Data sturcture

newtype Document (l :: Language) = Document Pandoc
newtype Filter (l :: Language) = Filter (Inline -> Inline)

(#>) :: Filter l -> Filter l -> Filter l
(#>) (Filter f) (Filter g) = Filter $ g . f

-- | Like a pandoc filter
(|>) :: HasFilter l => Document l -> Filter l -> Pandoc
(|>) (Document d) (Filter f) = pandocFilter f d
  where
    pandocFilter :: (Inline -> Inline) -> Pandoc -> Pandoc
    pandocFilter f d = runIdentity $ walkPandocM (return . f) d

class HasFilter (l :: Language) where
  filter :: Terms -> Filter l

instance HasFilter DE where
  filter = filter' DE

instance HasFilter FR where
  filter = filter' FR

instance HasFilter IT where
  filter = filter' IT

instance HasFilter EN where
  filter = filter' EN

filter' :: Language -> Terms -> Filter l
filter' l t = Filter replaceFig #> Filter (replaceSpan l t)

-- ==== Formatters

-- | format converts to Text
format :: (Show a) => a -> T.Text
format = T.pack . show

-- | format a Day
formatDay :: Day -> T.Text
formatDay = T.pack . showGregorian

-- | format a Double
formatDouble :: [T.Text] -> (Double -> Inline)
formatDouble cl | "percent"  `elem` cl = Str . T.pack . printf "%.2f%%"
formatDouble cl | "2_digits" `elem` cl = Str . T.pack . printf "%.2f"
formatDouble cl | "4_digits" `elem` cl = Str . T.pack . printf "%.4f"
formatDouble _ = Str . format
