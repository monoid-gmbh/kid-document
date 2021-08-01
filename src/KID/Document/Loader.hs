{-| QuasiQuoter for loading files at compile time -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module KID.Document.Loader (
     mdFileQQ
   , htmlFileQQ
   , nativeFileQQ
  ) where

import           Data.Data
import           Data.Either
import qualified Data.Text                  as T
import           Data.Text.IO               as T
import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Quote  as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified System.IO                  as IO
import           Text.Pandoc
import qualified Text.Pandoc.Class          as PC

-- | QuasiQuoter for loading files into Pandoc AST

readerOptions :: ReaderOptions
readerOptions = def { readerExtensions = pandocExtensions }

genericQQ :: Data a => (T.Text -> PandocPure a) -> TH.QuasiQuoter
genericQQ r = TH.QuasiQuoter{..}
  where quotePat _  = TH.reportError "Used as a pattern" >> pure TH.WildP
        quoteType _ = TH.reportError "Used as a type" >> pure TH.StarT
        quoteDec _  = TH.reportError "Used as a declaration" >> pure []
        quoteExp    = liftDataWithText . fromRight empty_doc . PC.runPure . r . T.strip . T.pack
        empty_doc   = error "Error processing template file"

-- | QuasiQuoter for reading md files
mdFileQQ :: TH.QuasiQuoter
mdFileQQ = quoteFile . genericQQ $ readMarkdown readerOptions

-- | QuasiQuoter for reading html files
htmlFileQQ :: TH.QuasiQuoter
htmlFileQQ = quoteFile . genericQQ $ readHtml readerOptions

-- | QuasiQuoter for reading pandoc files
nativeFileQQ :: TH.QuasiQuoter
nativeFileQQ = quoteFile . genericQQ $ readNative readerOptions

quoteFile :: TH.QuasiQuoter -> TH.QuasiQuoter
quoteFile TH.QuasiQuoter { quoteExp = qe, quotePat = qp, quoteType = qt, quoteDec = qd } =
  TH.QuasiQuoter { quoteExp = get qe, quotePat = get qp, quoteType = get qt, quoteDec = get qd }
    where
      get :: (String -> TH.Q a) -> String -> TH.Q a
      get quoter filename = TH.runIO (readFile filename) >>= quoter
      readFile filename =
        do h <- IO.openFile filename IO.ReadMode
           IO.hSetEncoding h IO.utf8_bom
           text <- T.hGetContents h
           return $ T.unpack text

-- | see: https://stackoverflow.com/questions/38143464/cant-find-inerface-file-declaration-for-variable
liftDataWithText :: Data a => a -> TH.Q TH.Exp
liftDataWithText = TH.dataToExpQ (fmap liftText . cast)

liftText :: T.Text -> TH.Q TH.Exp
liftText txt = TH.AppE (TH.VarE 'T.pack) <$> TH.lift (T.unpack txt)
