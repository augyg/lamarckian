module Landing.Impl where

import Landing.Compiler
import Landing.Router
import Common.Route

import Language.Haskell.TH
import Obelisk.Route
import System.FilePath

getPageCompiled :: R LandingRoute -> Q Exp
getPageCompiled route = tryWritePageGetPath ("staticSite" </> "src" </> "html") route staticRouter

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

