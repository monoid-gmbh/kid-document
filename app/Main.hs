{-| Sample main module

-}

{-# LANGUAGE DeriveDataTypeable #-}

module Main
  where

import           Data.Aeson             (eitherDecode)
import qualified Data.ByteString.Lazy   as B (readFile)
import           Data.Maybe             (fromMaybe)
import           KID.Document           as T (generateDocument)
import           KID.Document.Domain    (Language (..), Terms)
import           System.Console.CmdArgs
import           Text.Pandoc            (Pandoc)

-- | Data type for the command line arguments
data KidDoc = KidDoc {
    file :: FilePath
  , lang :: Language
  } deriving (Show, Data, Typeable)

-- | Command line arguments
kidArgs :: KidDoc
kidArgs = KidDoc {
    file = def &= help "JSON file for Terms"
  , lang = DE &= help "Language of the target document" &= opt "DE"
  }

-- | Main
main :: IO ()
main = do
  k <- cmdArgs kidArgs
  d <- eitherDecode <$> B.readFile (file k)
  case d of
     Left err -> putStrLn err
     Right t  -> print $ generateDocument (lang k) t
