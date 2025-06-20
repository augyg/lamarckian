module Lamarckian.Compiler where

import Lamarckian.Types
import Lamarckian.Render

import Control.Exception

import Control.Concurrent
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



data StaticSite t r = StaticSite
  { _staticSite_baseFilePath
    :: FilePath
  -- TODO: split up head and body... OF THE DOM I MEAN!!
  , _staticSite_router
    :: R r -> Q (StaticWidget' r t ())
  , _staticSite_getFromFile
    :: FilePath -> Q Exp
  , _staticSite_routeEncoder
    :: R r -> T.Text
  }

compileStaticSite :: StaticSite t r -> (R r -> Q Exp)
compileStaticSite cfg =
  compileStaticSite'
     (_staticSite_baseFilePath cfg)
     (_staticSite_router cfg)
     (_staticSite_getFromFile cfg)
     (_staticSite_routeEncoder cfg)

compileStaticSite'
  :: StaticPackagePath
  -> (R r -> Q (StaticWidget' t r ()))
  -> (FilePath -> Q Exp)
  -> (R r -> T.Text)
  -> R r
  -> Q Exp
compileStaticSite' = tryWritePageGetPath

tryWritePageGetPath
  :: StaticPackagePath
  -> (R r -> Q (StaticWidget' t r ()))
  -> (FilePath -> Q Exp)
  -> (R r -> T.Text)
  -> R r
  -> Q Exp
tryWritePageGetPath staticPackagePath routeToSelectedPage getFromFile routeToPath route = do
  let filepath = staticRouteToRelativeFilePath (route) routeToPath
  let (dir, _) = splitFileName filepath

  haskellDOM' {-StaticWidget-} <- routeToSelectedPage route
  haskellDOM <- pure $! haskellDOM' 
  status <- runIO $ newMVar NotDone 
  runIO $ forkIO $ do 
    (body' :: Either SomeException BS.ByteString) <- try $ createHtmlDoc haskellDOM

    
    case body' of
      Left e -> do
        -- status <|= newVal , like modifyMVar
        modifyMVar_ status (\_ -> pure $ DoneFailed (show e))
        --appendFile "exception.txt" "EXCEPTION"
      Right body -> do 
        tryWritePageIO staticPackagePath filepath dir body
        modifyMVar_ status $ \_ -> pure DoneSuccess

  runIO $ checkAndRethrowError status
  
  getFromFile $ "html" </> filepath

data Promised = NotDone | DoneFailed Reason | DoneSuccess
type Reason = String

checkAndRethrowError :: MVar Promised -> IO () 
checkAndRethrowError status = do
  prom <- readMVar status
  case prom of
    NotDone -> do
      threadDelay 10000000
      checkAndRethrowError status
    DoneFailed e -> error e
    DoneSuccess -> pure ()

  
  -- modifyMVar_ status $ \case
  --   NotDone -> do
  --     threadDelay 10000000
  --     checkAndRethrowError status
  --   DoneFailed reason -> do
  --     error reason
  --     pure ()
  --   DoneSuccess -> do
  --     pure ()
  -- pure ()
    

  -- this should wait until its told YES 

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


-- | We should almost have a wrapper that tries to write a page with any number of steps
tryWritePageIO :: StaticPackagePath -> FilePath -> String -> BS.ByteString -> IO ()
tryWritePageIO staticPackagePath filepath dir body = do
  --let base = "static" </> "src" </> "html"
  _ <- CE.catch
       (do 
           createDirectoryIfMissing True (staticPackagePath </> dir)  
           BS.writeFile (staticPackagePath </> filepath) body
           appendFile "exception.txt" "SUCCESS"
           pure True
       )
       (\(_ :: IOException) -> do
           appendFile "exception.txt" "EXCEPTION"
           pure $ const False False
       )
  pure () 

-- staticRouteToRelativeFilePath :: R LandingRoute -> FilePath
-- staticRouteToRelativeFilePath landingRoute = 
--   drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder $ Landing :/ landingRoute) <> ".html"

staticRouteToRelativeFilePath :: R r -> (R r -> T.Text) -> FilePath
staticRouteToRelativeFilePath landingRoute toPath = 
  --drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder landingRoute) <> ".html"
  drop 1 $ (T.unpack $ toPath landingRoute) <> ".html"
  
-- myStaticFP :: R LandingRoute -> FilePath
-- myStaticFP r =
--   staticRouteToRelativeFilePath
--     ( (\x -> Landing :/ x)  r )
--     (renderBackendRoute checkedFullRouteEncoder) 
  
  
  --drop 1 $ (T.unpack $ renderBackendRoute checkedFullRouteEncoder $ Landing :/ landingRoute) <> ".html"

createHtmlDoc :: StaticWidget' t r () -> IO BS.ByteString
createHtmlDoc dom = do
  html <- runStaticWidget dom
  pure $ "<!DOCTYPE html>" <> html

-- -- Helper func since we need the source to be ByteString
-- createHtmlDoc
--   :: R r
--   -> (R r -> IO BS.ByteString)
--   -> IO BS.ByteString
-- createHtmlDoc route router = do
--   html <- router route
--   pure $ "<!DOCTYPE html>" <> html

-- getPageCompiled :: (FilePath -> Q FilePath) -> R LandingRoute -> Q Exp
-- getPageCompiled getFile route = tryWritePageGetPath ("staticSite" </> "src" </> "html") route staticRouter getFile
