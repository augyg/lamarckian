module Lamarckian.Compiler where

import Common.Route
import Language.Haskell.TH
import Obelisk.Route
import qualified Control.Exception as CE
import Control.Exception (IOException)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath

-- TODO: currently this gives all power to staticRouter to set the head
-- it would be nice to abstract an interface like this

-- runStaticSite :: StaticSite t route -> R route -> Q Exp

-- data StaticSite t route = StaticSite
--   { staticDir :: FilePath
--   , frontendHead :: R route -> StaticWidget' t ()
--   , frontendBody :: R route -> StaticWidget' t ()
---  maybe more:
--   , snapOptions :: OptionsSnap 
--   }



data StaticSite r = StaticSite
  { _staticSite_baseFilePath
    :: FilePath
  -- TODO: split up head and body... OF THE DOM I MEAN!!
  , _staticSite_router
    :: R r -> Q BS.ByteString
  , _staticSite_getFromFile
    :: FilePath -> Q Exp
  , _staticSite_routeEncoder
    :: R r -> T.Text
  }

compileStaticSite :: StaticSite r -> (R r -> Q Exp)
compileStaticSite cfg =
  compileStaticSite'
     (_staticSite_baseFilePath cfg)
     (_staticSite_router cfg)
     (_staticSite_getFromFile cfg)
     (_staticSite_routeEncoder cfg)

compileStaticSite'
  :: StaticPackagePath
  -> (R r -> Q BS.ByteString)
  -> (FilePath -> Q Exp)
  -> (R r -> T.Text)
  -> R r
  -> Q Exp
compileStaticSite' = tryWritePageGetPath

type StaticPackagePath = FilePath 
tryWritePageGetPath
  :: StaticPackagePath
  -> (R r -> Q BS.ByteString)
  -> (FilePath -> Q Exp)
  -> (R r -> T.Text)
  -> R r
  -> Q Exp
tryWritePageGetPath staticPackagePath f getFromFile routeToPath route = do
  body <- putDocType route f -- route 
  let filepath = staticRouteToRelativeFilePath (route) routeToPath
  let (dir, _) = splitFileName filepath 
  tryWritePage staticPackagePath filepath dir body
  getFromFile $ "html" </> filepath

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

-- staticRouteToRelativeFilePath :: R LandingRoute -> FilePath
-- staticRouteToRelativeFilePath landingRoute = 
--   drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder $ Landing :/ landingRoute) <> ".html"

staticRouteToRelativeFilePath :: R r -> (R r -> T.Text) -> FilePath
staticRouteToRelativeFilePath landingRoute toPath = 
  --drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder landingRoute) <> ".html"
  drop 1 $ (T.unpack $ toPath landingRoute) <> ".html"
  
myStaticFP :: R LandingRoute -> FilePath
myStaticFP r =
  staticRouteToRelativeFilePath
    ( (\x -> Landing :/ x)  r )
    (renderBackendRoute checkedFullRouteEncoder) 
  
  
  --drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder $ Landing :/ landingRoute) <> ".html"


-- Helper func since we need the source to be ByteString
putDocType
  :: R r
  -> (R r -> Q BS.ByteString)
  -> Q BS.ByteString
putDocType route router = do
  html <- router route
  pure $ "<!DOCTYPE html>" <> html

-- getPageCompiled :: (FilePath -> Q FilePath) -> R LandingRoute -> Q Exp
-- getPageCompiled getFile route = tryWritePageGetPath ("staticSite" </> "src" </> "html") route staticRouter getFile
