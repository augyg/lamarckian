module Lamarckian.Compiler where


import Lamarckian.Types
import Lamarckian.Render

-- import Control.Exception

import Control.Concurrent
import Language.Haskell.TH
import Obelisk.Route
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Control.Exception as CE
import Control.Exception (IOException)
import Data.Maybe
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
--experimental
import System.PosixCompat.Files
import System.PosixCompat.Types
import System.PosixCompat.User


type URL = T.Text
type CompiledFilePath = Q Exp
data StaticSite t r = StaticSite
  { _staticSite_baseWritableFolder :: FilePath
  -- ^ the prefix not included in staticFilePath's argument
  , _staticSite_staticFilePath  :: FilePath -> Q Exp
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

compileStaticSite :: StaticSite t r -> (R r -> StaticWidget' r t () -> Q Exp)
compileStaticSite cfg =
  tryWritePageGetPath
     (_staticSite_localHint cfg)
     (_staticSite_baseWritableFolder cfg)
     (_staticSite_staticFilePath cfg)
     (_staticSite_subFolder cfg)
     (_staticSite_routeEncoder cfg)


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

  -- x <- runIO $ doesDirectoryExist "config"
  -- reportError $ show x
  -- eg html/landing/FAQ.html
  let filepath = fromMaybe "" m_subFolder </> (coerceRouteToHtmlPath $ T.unpack (routeEncoder route))
  let (dir, _) = splitFileName filepath
  -- reportError dir
  (body' :: BS.ByteString) <- runIO $ createHtmlDoc page
  currentDirectory <- liftIO $ getCurrentDirectory
  pipelineStage <- runIO $ tryWritePageIO (currentDirectory </> baseWritableFolder) (filepath) dir body'
  case pipelineStage of
    CanWriteFile fpPushed -> do
      isLocal <- runIO localHint
      if isLocal
        then [| fpPushed |]
        else staticFilePath filepath
      -- --staticFilePath filepath
      -- -- last result was CanWriteFile
      -- -- reportError $ "CanWriteFile" <> fp2
      -- [| fpPushed |]
    FailedWriteFile -> do
      --reportError $ "FailedWriteFile" <> filepath
      staticFilePath filepath
    --FailedWriteFile -> lookupFile $ "html" </> filepath

data Promised = NotDone | DoneFailed Reason | DoneSuccess
type Reason = String

-- | We should almost have a wrapper that tries to write a page with any number of steps
tryWritePage :: StaticPackagePath -> FilePath -> String -> BS.ByteString -> Q ()
tryWritePage staticPackagePath filepath dir body = do
  --let base = "static" </> "src" </> "html"
  _ <- recover (pure False) $ runIO $ CE.catch
       (do
           createDirectoryIfMissing True (staticPackagePath </> dir)
           BS.writeFile (staticPackagePath </> dir </> filepath) body
           --reportError (staticPackagePath </> filepath)
           pure True
       )
       (\(_ :: IOException) -> pure $ const False False)
  pure ()

data CouldWrite = CanWriteFile FilePath | FailedWriteFile deriving Show
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

-- Produces something like x.html or x/y/z.html ; a true suffix
coerceRouteToHtmlPath :: String -> String
coerceRouteToHtmlPath route =
  let
    fp = if route == "/"
         then "base"
         else drop 1 route
  in
    fp <> ".html"

--  drop 1 . \t -> if t T.unpack t <> ".html"

staticRouteToRelativeFilePath :: R r -> (R r -> T.Text) -> FilePath
staticRouteToRelativeFilePath landingRoute toPath =
  drop 1
  $ (T.unpack $ toPath landingRoute) <> ".html"

createHtmlDoc :: StaticWidget' t r () -> IO BS.ByteString
createHtmlDoc dom = do
  html <- runStaticWidget dom
  pure $ "<!DOCTYPE html>" <> html

type Dir = FilePath
type FileName = FilePath
data VirtualPath = VirtualPath
  { _virtualPath_writePrefix :: FilePath
  , _virtualPath_filePath :: (Maybe Dir, FileName) -- <- should be fully determinable by the Route
  }

-- staticSite/<folders>/

-- whole@<folders-real-writable>/<Maybe src-subdir>/<filename>
--   -> Path is basically a Pair of readPath writePath
--     -> where readPath  = staticFilePath ( <Maybe src-subdir>/<filename> )
--     ->       writePath = whole

-- -> where filename = if empty then base.html el

-- R r -> (FilePath, FilePath)
-- -- allow for output where
--
-- toObeliskDir :: FilePath -> FilePath
-- toObeliskDir p = p </> obeliskDirName
--   where obeliskDirName = ".obelisk"

-- findProjectRoot' :: MonadIO m => m (Maybe FilePath)
-- findProjectRoot' = findProjectRoot "."

-- -- | Get the FilePath to the containing project directory, if there is one
-- findProjectRoot :: MonadIO m => FilePath -> m (Maybe FilePath)
-- findProjectRoot target = do
--   myUid <- liftIO getRealUserID
--   targetStat <- liftIO $ getFileStatus target
--   -- umask <- liftIO getUmask
--   (result, _) <- liftIO $ runStateT (walkToProjectRoot target targetStat) []
--   return $ makeRelative "." <$> result

-- walkToProjectRoot
--   :: (MonadIO m)
--   => FilePath -> FileStatus -> m (Maybe FilePath)
-- walkToProjectRoot this thisStat = liftIO (doesDirectoryExist this) >>= \case
--   -- It's not a directory, so it can't be a project
--   False -> do
--     let dir = takeDirectory this
--     dirStat <- liftIO $ getFileStatus dir
--     walkToProjectRoot dir dirStat
--   True -> do
--     --when (not $ isWellOwnedAndWellPermissioned thisStat myUid desiredUmask) $ modify (this:)
--     liftIO (doesDirectoryExist $ toObeliskDir this) >>= \case
--       True -> return $ Just this
--       False -> do
--         let next = this </> ".." -- Use ".." instead of chopping off path segments, so that if the current directory is moved during the traversal, the traversal stays consistent
--         nextStat <- liftIO $ getFileStatus next
--         let fileIdentity fs = (deviceID fs, fileID fs)
--             isSameFileAs = (==) `on` fileIdentity
--         if thisStat `isSameFileAs` nextStat
--           then return Nothing -- Found a cycle; probably hit root directory
--           else walkToProjectRoot next nextStat
