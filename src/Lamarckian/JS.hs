module Lamarckian.JS where

import Lamarckian.Render
import Lamarckian.Types
import Text.IStr
import Reflex.Dom.Core
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype JSFunc = JSFunc String deriving Show 


-- TODO: better , more valid for other common types
js2 :: (Show a, Show b) => Name -> a -> b -> JSFunc
js2 func a b = JSFunc [istr| #{func}#{(a, b)} |]

js3 :: (Show a, Show b, Show c) => Name -> a -> b -> c -> JSFunc
js3 func a b c = JSFunc [istr| #{func}#{(a, b, c)} |]

js4 :: (Show a, Show b, Show c, Show d) => Name -> a -> b -> c -> d -> JSFunc
js4 func a b c d = JSFunc [istr| #{func}#{(a, b, c, d)} |]
  
js5 :: (Show a, Show b, Show c, Show d, Show e) => Name -> a -> b -> c -> d -> e -> JSFunc
js5 func a b c d e = JSFunc [istr| #{func}#{(a, b, c, d, e)} |]

js6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Name -> a -> b -> c -> d -> e -> f -> JSFunc
js6 func a b c d e f = JSFunc [istr| #{func}#{(a, b, c, d, e, f)} |]

js7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Name -> a -> b -> c -> d -> e -> f -> g -> JSFunc
js7 func a b c d e f g = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g)} |]

js8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Name -> a -> b -> c -> d -> e -> f -> g -> h -> JSFunc
js8 func a b c d e f g h = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g, h)} |]

js9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Name -> a -> b -> c -> d -> e -> f -> g -> h -> i -> JSFunc
js9 func a b c d e f g h i = JSFunc [istr| #{func}#{(a, b, c, d, e, f, g, h, i)} |]

-- | lmao @ name 
-- | Make a set of functions which affect the same container given by DomID
-- | then you can disperse
-- <button onClick="document.body.innerHTML = '<div>New content inside a div!</div>'">Click Me</button>
-- | While this is crap rn,
-- | there is no reason we cant vary the "IO" strategy here
-- | because if we have an ID of the target to change to we can:
-- |  -> generate this and send in the JS files
-- |  -> have an associated handler for fetching HTML components 
type DomID = String 
staticDynamicDOM :: MonadIO m => DomID -> [StaticWidget' r x a] -> m [JSFunc]
staticDynamicDOM domId widgets = liftIO $ forM widgets $ \w -> do
  x <- runStaticWidget w
  let html = T.decodeUtf8 x 
  pure $ js2 "setChild" domId html

staticDynWithToggle :: MonadIO m => DomID -> [StaticWidget' r x a] -> m [JSFunc]
staticDynWithToggle domId widgets = liftIO $ forM (zip [(1::Int)..] widgets) $ \(n,widget) -> do
  html <- T.decodeUtf8 <$> runStaticWidget widget
  pure $ js3 "setChildToggle" domId n html 

type OnClick = JSFunc 
simpleButton :: DomBuilder t m => T.Text -> JSFunc -> m ()
simpleButton label (JSFunc onClick) = elAttr "button" ("onClick" =: T.pack onClick) $ text label

