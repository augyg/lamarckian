module Lamarckian.Snap where

import Snap
import Snap.Internal.Util.FileServe
import Control.Monad.IO.Class
import System.Directory
import qualified Data.ByteString as BS

serveCompressed :: MonadSnap m => FilePath -> m ()
serveCompressed = serveFileIfExistsAs "text/html; charset=utf-8"

-- | From Obelisk, we just dont want to force a dep
-- | Like 'serveFileIfExists', but with a given MIME type
serveFileIfExistsAs :: MonadSnap m => BS.ByteString -> FilePath -> m ()
serveFileIfExistsAs mimeType f = do
  exists <- liftIO $ doesFileExist f
  if exists then serveFileAs mimeType f else pass

-- serveCompressed :: MonadSnap m => FilePath -> m T.Text
-- serveCompressed fp = do
--   withCompression $ do
--     writeLBS =<< liftIO (LBS.readFile fp)
--   let pageName = dropExtension . takeFileName $ fp
--   pure $ T.pack pageName
