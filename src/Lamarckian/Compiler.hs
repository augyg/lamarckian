module Lamarckian.Compiler where


import Lamarckian.Types
import Lamarckian.Render

-- import Control.Exception

import Control.Concurrent
import Language.Haskell.TH
import Obelisk.Route
import qualified Control.Exception as CE
import Control.Exception (IOException)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

type URL = T.Text
data StaticSite t r = StaticSite
  { _staticSite_baseFilePath :: FilePath
  , _staticSite_getFromFile  :: FilePath -> Q Exp
  , _staticSite_routeEncoder :: R r -> URL
  }

compileStaticSite :: StaticSite t r -> (R r -> StaticWidget' r t () -> Q Exp)
compileStaticSite cfg =
  tryWritePageGetPath
     (_staticSite_baseFilePath cfg)
     (_staticSite_getFromFile cfg)
     (_staticSite_routeEncoder cfg)

tryWritePageGetPath
  :: StaticPackagePath
  -> (FilePath -> Q Exp)
  -> (R r -> T.Text)
  -> R r
  -> (StaticWidget' t r ()) 
  -> Q Exp
tryWritePageGetPath staticPackagePath lookupFile routeToPath route page = do
  let filepath = staticRouteToRelativeFilePath (route) routeToPath
  let (dir, _) = splitFileName filepath
  (body' :: BS.ByteString) <- runIO $ createHtmlDoc page
  couldWrite <- runIO $ tryWritePageIO staticPackagePath filepath dir body'
  pipelineStage <- runIO $ tryWritePageIO staticPackagePath filepath dir body'
  case pipelineStage of
    CanWriteFile fpPushed -> [| fpPushed |]
    FailedWriteFile -> lookupFile $ "html" </> filepath

data Promised = NotDone | DoneFailed Reason | DoneSuccess
type Reason = String

-- | We should almost have a wrapper that tries to write a page with any number of steps
tryWritePage :: StaticPackagePath -> FilePath -> String -> BS.ByteString -> Q ()
tryWritePage staticPackagePath filepath dir body = do
  --let base = "static" </> "src" </> "html"
  _ <- recover (pure False) $ runIO $ CE.catch
       (do 
           createDirectoryIfMissing True (staticPackagePath </> dir)  
           BS.writeFile (staticPackagePath </> filepath) body
           pure True
       )
       (\(_ :: IOException) -> pure $ const False False)
  pure () 

data CouldWrite = CanWriteFile FilePath | FailedWriteFile  
-- | We should almost have a wrapper that tries to write a page with any number of steps
tryWritePageIO :: StaticPackagePath -> FilePath -> String -> BS.ByteString -> IO CouldWrite
tryWritePageIO staticPackagePath filepath dir body = do
  --let base = "static" </> "src" </> "html"
  CE.catch
       (do 
           createDirectoryIfMissing True (staticPackagePath </> dir)
           let fpPush = staticPackagePath </> filepath
           BS.writeFile fpPush body
           pure $ CanWriteFile fpPush
       )
       (\(_ :: IOException) -> do
           pure FailedWriteFile
       )
       
staticRouteToRelativeFilePath :: R r -> (R r -> T.Text) -> FilePath
staticRouteToRelativeFilePath landingRoute toPath =
  drop 1 $ (T.unpack $ toPath landingRoute) <> ".html"

createHtmlDoc :: StaticWidget' t r () -> IO BS.ByteString
createHtmlDoc dom = do
  html <- runStaticWidget dom
  pure $ "<!DOCTYPE html>" <> html
