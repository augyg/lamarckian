module Lamarckian.Types where

import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import qualified Data.Text as T

type GroupKey = String

type StaticDom = PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))

type StaticPackagePath = FilePath

-- | In theory we could make the r of ReaderT generic 
newtype TemplateT m a = TemplateT { runTemplateT :: ReaderT TemplateVars m a }
-- | In generic impl. that would mean `templateSlot :: (r -> a) -> TemplateT a ~~~ asks`
-- | Further, we could also have a templateFileSlot function 
type Name = String
type StaticWidget' r x a = SetRouteT (SpiderTimeline Global) (R r) (RouteToUrlT (R r) (StaticWidget x)) a
type TemplateVars = Map.Map String String
type SlotKey = String

-- | The key allows for arbitrary retrieval of a Template group  
type HTemplateVars k a = Map.Map k [(SlotKey, HtmlString, a)]
-- | for example, lets say we are writing an email with multiple sections which require non-escaped Raw Strings (ie html)
-- |
-- | do 
-- |   let tmplMap = fromList [( 1, sectionOneVars), (2, sectionTwoVars)]
-- |   renderSectionOne (Map.lookupWithDefault [] 1 tmplMap) 
-- |   renderSectionTwo (Map.lookupWithDefault [] 2 tmplMap)
-- |
-- | Thus, this data structure allows for an arbitrary number of DomBuilders 
type HTemplateRefs k a = Map.Map k [(SlotKey, a)]

-- | Once the DOM has been rendered, all we need are our globally unique IDs + vals
type HTemplateValues = Map.Map SlotKey (HtmlString) 

newtype HtmlString = HtmlString { getHtml :: T.Text } 


