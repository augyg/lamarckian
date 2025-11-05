module Lamarckian.Compiler where


import Lamarckian.Types
import Lamarckian.Render

-- import Control.Exception

import Language.Haskell.TH
import Obelisk.Route
import Control.Monad.IO.Class
import qualified Control.Exception as CE
import Control.Exception (IOException)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

type URL = T.Text
type CompiledFilePath = Q Exp

data StaticSite r = StaticSite
  { _staticSite_baseWritableFolder :: FilePath
  -- ^ the prefix not included in staticFilePath's argument
  , _staticSite_staticFilePath  :: FilePath -> CompiledFilePath
  -- ^ Your obelisk generated staticFilePath function
  , _staticSite_routeEncoder :: R r -> URL
  -- ^ In the simplest of cases, this is just simply rendering the route
  -- ie. (<rendered-route>|"root" if nothing).html
  , _staticSite_subFolder :: Maybe FilePath
  -- ^ Allows for configuring cases like
  -- baseWritableFolder/<subFolder>/filepathFromRoute
  , _staticSite_localHint :: IO Bool
  -- ^ Flag for whether or not to get file directly from
  -- where it was written, which only works if local.
  }

compileStaticSite :: StaticSite r -> (R r -> StaticWidget' r t () -> Q Exp)
compileStaticSite cfg =
  tryWriteStaticWidgetGetPath
     (_staticSite_localHint cfg)
     (_staticSite_baseWritableFolder cfg)
     (_staticSite_staticFilePath cfg)
     (_staticSite_subFolder cfg)
     (_staticSite_routeEncoder cfg)

compilePlainTextStaticSite :: StaticSite r -> (R r -> BS.ByteString -> Q Exp)
compilePlainTextStaticSite cfg =
  tryWritePlainTextPageGetPath
     (_staticSite_localHint cfg)
     (_staticSite_baseWritableFolder cfg)
     (_staticSite_staticFilePath cfg)
     (_staticSite_subFolder cfg)
     (_staticSite_routeEncoder cfg)


tryWriteStaticWidgetGetPath
  :: IO Bool
  -> StaticPackagePath -- baseWritableFolder
  -> (FilePath -> CompiledFilePath) -- staticFilePath
  -> Maybe FilePath -- m_subFolder
  -> (R r -> URL) -- routeToPath
  -> R r          -- Actual Route
  -> (StaticWidget' t r ()) -- Html
  -> CompiledFilePath -- A Compiled FilePath, from lookupFile
tryWriteStaticWidgetGetPath = tryWritePageGetPath

-- If we try to hard code the route we are looking for then it works so its
-- definitely a value error and not a compilation limitation
tryWritePlainTextPageGetPath
  :: IO Bool
  -> StaticPackagePath -- baseWritableFolder
  -> (FilePath -> CompiledFilePath) -- staticFilePath
  -> Maybe FilePath -- m_subFolder
  -> (R r -> URL) -- routeToPath
  -> R r          -- Actual Route
  -> (BS.ByteString) -- Html
  -> CompiledFilePath -- A Compiled FilePath, from lookupFile
tryWritePlainTextPageGetPath localHint baseWritableFolder staticFilePath m_subFolder routeEncoder route page = do
  let filepath = fromMaybe "" m_subFolder </> (coerceRouteToHtmlPath $ T.unpack (routeEncoder route))
  currentDirectory <- liftIO $ getCurrentDirectory
  let fpPush =
        currentDirectory
        </> baseWritableFolder
        </> fromMaybe "" m_subFolder
        </> (coerceRouteToHtmlPath $ T.unpack (routeEncoder route))
  --(body' :: BS.ByteString) <- runIO $ createHtmlDoc page
  let body' = page
  pipelineStage <- runIO $ tryWritePageIO fpPush body'
  case pipelineStage of
    CanWriteFile fpPushed -> do
      isLocal <- runIO localHint
      if isLocal
        then [| fpPushed |]
        else staticFilePath filepath
    FailedWriteFile -> do
      staticFilePath filepath

-- If we try to hard code the route we are looking for then it works so its
-- definitely a value error and not a compilation limitation
tryWritePageGetPath
  :: IO Bool
  -> StaticPackagePath -- baseWritableFolder
  -> (FilePath -> CompiledFilePath) -- staticFilePath
  -> Maybe FilePath -- m_subFolder
  -> (R r -> URL) -- routeToPath
  -> R r          -- Actual Route
  -> (StaticWidget' t r ()) -- Html
  -> CompiledFilePath -- A Compiled FilePath, from lookupFile
tryWritePageGetPath localHint baseWritableFolder staticFilePath m_subFolder routeEncoder route page = do
  let filepath = fromMaybe "" m_subFolder </> (coerceRouteToHtmlPath $ T.unpack (routeEncoder route))
  currentDirectory <- liftIO $ getCurrentDirectory
  let fpPush =
        currentDirectory
        </> baseWritableFolder
        </> fromMaybe "" m_subFolder
        </> (coerceRouteToHtmlPath $ T.unpack (routeEncoder route))
  (body' :: BS.ByteString) <- runIO $ createHtmlDoc page
  pipelineStage <- runIO $ tryWritePageIO fpPush body'
  case pipelineStage of
    CanWriteFile fpPushed -> do
      isLocal <- runIO localHint
      if isLocal
        then [| fpPushed |]
        else staticFilePath filepath
    FailedWriteFile -> do
      staticFilePath filepath

data Promised = NotDone | DoneFailed Reason | DoneSuccess
type Reason = String

data CouldWrite = CanWriteFile FilePath | FailedWriteFile deriving Show

-- | We should almost have a wrapper that tries to write a page with any number of steps
tryWritePageIO :: FilePath -> BS.ByteString -> IO CouldWrite
tryWritePageIO fpPush body = do
  --let base = "static" </> "src" </> "html"
  CE.catch
       (do
           let (dir, _) = splitFileName fpPush
           createDirectoryIfMissing True dir
           BS.writeFile fpPush body
           pure $ CanWriteFile fpPush
       )
       (\(_ :: IOException) -> do
           pure FailedWriteFile
       )

-- Produces something like x.html or x/y/z.html ; a true suffix
coerceRouteToHtmlPath :: String -> String
coerceRouteToHtmlPath route =
  let
    fp = if route == "/"
         then "base"
         else drop 1 route
  in
    fp <> ".html"

staticRouteToRelativeFilePath :: R r -> (R r -> T.Text) -> FilePath
staticRouteToRelativeFilePath landingRoute toPath =
  drop 1
  $ (T.unpack $ toPath landingRoute) <> ".html"

createHtmlDoc :: StaticWidget' t r () -> IO BS.ByteString
createHtmlDoc dom = do
  html <- runStaticWidget dom
  pure $ "<!DOCTYPE html>" <> html
