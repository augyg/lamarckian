module Lamarckian.Compiler where

import Common.Route
--import Landing.Router
import Landing.Static as Landing

import Language.Haskell.TH
import Obelisk.Route
import qualified Control.Exception as CE
import Control.Exception (IOException)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

type StaticPackagePath = FilePath 
tryWritePageGetPath
  :: StaticPackagePath
  -> R LandingRoute
  -> (R LandingRoute -> Q BS.ByteString)
  -> Q Exp
tryWritePageGetPath staticPackagePath route f = do
  body <- putDocType route f -- route 
  let filepath = staticRouteToRelativeFilePath (route) 
  let (dir, _) = splitFileName filepath 
  tryWritePage staticPackagePath filepath dir body
  Landing.staticFilePath $ "html" </> filepath

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

staticRouteToRelativeFilePath :: R LandingRoute -> FilePath
staticRouteToRelativeFilePath landingRoute = 
  drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder $ Landing :/ landingRoute) <> ".html"

-- Helper func since we need the source to be ByteString
putDocType
  :: R LandingRoute
  -> (R LandingRoute -> Q BS.ByteString)
  -> Q BS.ByteString
putDocType route router = do
  html <- router route
  pure $ "<!DOCTYPE html>" <> html
