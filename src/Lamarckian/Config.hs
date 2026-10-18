{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Read @config/@ at TH-evaluation time and hand the raw file contents to a
-- user-supplied parser that returns the user's typed config record.
--
-- Shape:
--
-- @readConfigQ :: (Map FilePath Text -> IO cfg) -> Q cfg@
--
-- The infrastructure walks @config\/@ (relative to the current directory),
-- reads every regular file as 'Text', keys them by relative 'FilePath', and
-- hands the map to the caller's parser. The parser owns schema, defaults,
-- validation. The caller gets a plain 'cfg' value in the 'Q' monad which it
-- can then pass to any TH function (e.g. @runReaderT widget cfg@).
--
-- Degraded behavior: if @config\/@ does not exist (e.g. a @cabal build@ run
-- in a tree without a mounted config dir, or the Nix build sandbox), the
-- parser receives an empty 'Map'. The parser's responsibility is to provide
-- sensible fallbacks for every expected key via 'Map.findWithDefault' or
-- similar.
module Lamarckian.Config
  ( readConfigQ
  , readConfigFilesIO
  ) where

import Control.Monad (filterM)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Language.Haskell.TH (Q, runIO)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))

-- | Enumerate every regular file under @config\/@ relative to the current
-- directory, read each as UTF-8 'Text' (trailing whitespace stripped), and
-- return a map keyed by the relative 'FilePath'. Returns an empty map if the
-- @config\/@ directory does not exist.
readConfigFilesIO :: IO (Map FilePath Text)
readConfigFilesIO = do
  rootExists <- doesDirectoryExist "config"
  if rootExists
    then do
      paths <- walk "config"
      pairs <- traverse readPair paths
      pure $ Map.fromList pairs
    else pure Map.empty

readPair :: FilePath -> IO (FilePath, Text)
readPair fp = do
  bytes <- BS.readFile fp
  pure (fp, T.stripEnd (TE.decodeUtf8 bytes))

walk :: FilePath -> IO [FilePath]
walk root = do
  entries <- listDirectory root
  let full = map (root </>) entries
  files <- filterM doesFileExist full
  dirs <- filterM doesDirectoryExist full
  subs <- traverse walk dirs
  pure (files ++ concat subs)

-- | Read every @config\/@ file at TH time, pass the resulting
-- @Map FilePath Text@ to the user-supplied parser, and lift the parser's
-- 'IO' into 'Q'. The parser owns schema and defaults.
readConfigQ :: (Map FilePath Text -> IO cfg) -> Q cfg
readConfigQ parser = runIO (readConfigFilesIO >>= parser)
