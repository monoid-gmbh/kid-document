{-|

  The Pandoc library is used for generating the documents.

  === Template

  What is called a template in the following, is simply a Pandoc
  AST containing nodes that will be substituted with values
  resulting in another Pandoc AST. The node representing the
  template variables are the Pandoc /Span/ elements.
  For example the node referenced by the identifier /strs_one_year/

> Span ("strs_one_year",["2_digits"],[]) []

  will be replaced as a formatted string as follows

> Str "1434.33"

  to produce another Pandoc document, where all the /Span/ elements
  representing the variables are replace be formatted values.
  In the module "KID.Document.Template" the parts are put together
  to define the entire Key Information Document as a single document
  template. Assembling is done by the function `generateTemplate`.

  ==== Languages

  Templates are defined in four different `Language`s. By definition of
  `generateTemplate` we assure that documents in the different languages
  have the same structure, resp. the same contents.

  ==== Readers

  The module "KID.Document.Loader" contains QuasiQuoters for reading
  template definitions of various document types into Pandoc AST at
  compile time. This way the templates are part of the library which
  guarantees stability in a productive environment.

  === Document

  Finally a document is produced by applying the `Terms` to a template, see
  `generateDocument`

-}

{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}

module KID.Document (
    generateDocument
  ) where

import           Control.Monad.Identity (runIdentity)
import           KID.Document.Domain    (Language (..), Terms)
import           KID.Document.Loader    (nativeFileQQ)
import           KID.Document.Template  as T
import           Text.Pandoc.Builder    (Pandoc)

-- | `generateDocument` applies the `Terms` to the template for a given `Language`
generateDocument :: Language -- ^ Language
                 -> Terms    -- ^ Terms
                 -> Pandoc   -- ^ Target document
generateDocument DE t = (Document [nativeFileQQ|app/etc/kid_de.native|] :: Document DE) |> T.filter t
generateDocument FR t = (Document [nativeFileQQ|app/etc/kid_fr.native|] :: Document FR) |> T.filter t
generateDocument IT t = (Document [nativeFileQQ|app/etc/kid_it.native|] :: Document IT) |> T.filter t
generateDocument EN t = (Document [nativeFileQQ|app/etc/kid_en.native|] :: Document EN) |> T.filter t
