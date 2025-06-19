module Lamarckian.Snap where


import Snap
import Snap.Util.GZip
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS



serveCompressed :: MonadSnap m => FilePath -> EnvT m T.Text
serveCompressed fp = do
  withCompression $ do
    writeLBS =<< liftIO (LBS.readFile fp)
  let pageName = dropExtension . takeFileName $ fp
  pure $ T.pack pageName 
