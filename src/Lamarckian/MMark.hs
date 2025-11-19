{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Lamarckian.MMark where


import Text.IStr
import Scrappy.Scrape
import Scrappy.Elem hiding (tag)
import Text.Parsec as P
import Language.Haskell.TH
import Language.Haskell.Meta.Parse
import Lucid.Base (renderText)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Text.MMark as MMark
import Text.MMark.Extension.GhcSyntaxHighlighter
import Text.MMark.Extension.LinkTarget
import Text.MMark.Extension.Skylighting
import qualified Text.Megaparsec as MP

import Control.Monad.IO.Class
import Data.Text as T
import Data.Text.Lazy as LT
--import qualified Data.Text.IO as T

extensions :: [MMark.Extension]
extensions =
  [ ghcSyntaxHighlighter
  , linkTarget
  , skylighting
  ]

-- | TODO: can we use istrExp here too? or even scrappy-template might do the job?
immarkFile :: FilePath -> Q Exp
immarkFile fp = do
  _contents <- runIO $ readFile fp
--   _ <- error $ show $ Prelude.length _contents
  case MMark.parse fp $ T.pack _contents of
    Left (e) -> error $ MP.errorBundlePretty e
    Right bundle ->

      let
        _x = LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle
      in
        -- error $ show $ bundle
        ---error $ T.unpack  x
        [| x |]



readIMMarkFile :: FilePath -> IO (Either String T.Text)
readIMMarkFile fp = do
  _contents <- liftIO $ readFile fp
--   _ <- error $ show $ Prelude.length _contents
  case MMark.parse fp $ T.pack _contents of
    Left (e) -> pure . Left $ MP.errorBundlePretty e
    Right bundle -> pure . Right $ LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle

      -- let
      --   _x = LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle
      -- in
      --   -- error $ show $ bundle
      --   ---error $ T.unpack  x
      --   [| x |]


immark :: QuasiQuoter
immark = QuasiQuoter
  { quoteExp  = \md -> do
      [|
        case MMark.parse "<no name>" $ T.pack $(rstrExp md) of
          Left (e) -> error $ MP.errorBundlePretty e
          Right bundle -> LT.toStrict . renderText $ MMark.render $ useExtensions extensions bundle
       |]
  , quotePat  = error "markdown cannot be used as a pattern"
  , quoteType = error "markdown cannot be used as a type"
  , quoteDec  = error "markdown cannot be used as a declaration"
  }

      -- >> [| "" |]

      -- [| LT.toStrict
      --                    $ renderText
      --                    $ MMark.render
      --                    $ useExtensions
      --                    [ commentParagraph "-- |"
      --                    , fontAwesome
      --                    , footnotes
      --                    , ghcSyntaxHighlighter
      --                    , kbd
      --                    , linkTarget
      --                    , mathJax $ Just 'x'
      --                    --, obfuscateEmail
      --                    , punctuationPrettifier
      --                    , skylighting
      --                    --, toc
      --                    ]
      --                    $ either (\e -> error $ "invalid markdown: " <> show e) id
      --                    $ MMark.parse "filename"-- md
      --                    --   $ streamEdit matchPat (runHaskellExpr) md
      --                    $ T.pack
      --                    $(rstrExp md)

      --                   |]
    -- quoteExp  = \md -> [| LT.toStrict
    --                      $ renderText
    --                      $ MMark.render
    --                      $ useExtensions
    --                      [ commentParagraph "-- |"
    --                      , fontAwesome
    --                      , footnotes
    --                      , ghcSyntaxHighlighter
    --                      , kbd
    --                      , linkTarget
    --                      , mathJax $ Just 'x'
    --                      --, obfuscateEmail
    --                      , punctuationPrettifier
    --                      , skylighting
    --                      --, toc
    --                      ]
    --                      $ either (\e -> error $ "invalid markdown: " <> show e) id
    --                      $ MMark.parse "filename"-- md
    --                      --   $ streamEdit matchPat (runHaskellExpr) md
    --                      $ T.pack
    --                      $(rstrExp md)

    --                     |]


-- Helper to parse and inject evaluated Haskell expressions into markdown
runHaskellExpr :: String -> Q Exp
runHaskellExpr expr = do
  case parseExp expr of
    Left err  -> fail $ "Failed to parse Haskell expression: " ++ err
    Right ast -> [| trace $(stringE expr) (show $(return ast)) |]


matchPat :: ScraperT String
matchPat = do
  _ <- P.string "{{>>="
  fst <$> manyTill_ (P.anyChar) (try $ string "=<<}}")

-- main :: IO ()
-- main = do
--   putStrLn $ [markdown|
-- # Hello World
-- This is a **Markdown** example with _quasi-quotes_.

-- - List item 1
-- - List item 2
--   |]
