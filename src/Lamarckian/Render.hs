module Lamarckian.Render where

import Lamarckian.Types
import Lamarckian.Template as Template

import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Language.Haskell.TH
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS

-- | OH MY GOD HOW DO I USE THE TH TO CHECK THE ROUTE
-- | OR use classes and resolve based on that?

-- instance RenderDOM (StaticWidget MyRoute t a) where 
-- instance RenderDOM (StaticWidget MyRoute2 t a) where 
  -- Could subroutes derive from each other?

-- instance RenderDOM (_ MyRoute) => RenderDOM (StaticWidget (MyRoute :> MySubRoute) t a) where ...



unTemplateHTMLString :: Map.Map String HtmlString -> String -> String
unTemplateHTMLString tmplMap input = Template.unTemplate (T.unpack . getHtml <$> tmplMap) input

-- | TODO use ReaderT to ensure use of templateSlot matches existing key 
renderStaticTemplate' :: TemplateVars -> StaticWidget' r x () -> Q BS.ByteString
renderStaticTemplate' mappy dom = do
  container <- runIO $ runStaticWidget dom
  pure
    $ T.encodeUtf8 . T.pack 
    $ unTemplate mappy
    $ T.unpack . T.decodeUtf8 
    $ container

runStaticWidget :: StaticWidget' r x a -> IO BS.ByteString 
runStaticWidget = fmap snd . renderStatic . flip runRouteToUrlT renderRoute . runSetRouteT
  where
    renderRoute = const "" -- doesnt seem to do anything anyways

runStaticTemplateWidget :: TemplateVars -> StaticWidget' r x () -> IO BS.ByteString
runStaticTemplateWidget mappy dom = do
  container <- runStaticWidget dom
  pure
    $ T.encodeUtf8 . T.pack 
    $ Template.unTemplate mappy
    $ T.unpack . T.decodeUtf8 
    $ container

runStaticHTemplateWidget
  :: HTemplateVars k a
  -> (HTemplateRefs k a -> StaticWidget' r x ())
  -> IO BS.ByteString
runStaticHTemplateWidget mappy mkDom = do
  container <- runStaticWidget $ mkDom (justReferences mappy) --dom
  x <- pure
    $ T.encodeUtf8 . T.pack 
    $ unTemplateHTMLString (getHTemplateValues mappy)
    $ T.unpack . T.decodeUtf8 
    $ container
  putStrLn "runEmailHTemplateWidget"
  print x
  pure x
  where
    justReferences :: HTemplateVars k a -> HTemplateRefs k a
    justReferences = fmap (fmap grab_1_3)

    grab_1_3 (x,_,a) = (x,a)

