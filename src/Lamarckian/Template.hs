module Lamarckian.Template where

import Lamarckian.Types 

import Reflex.Dom.Core 
import Scrappy.Find (streamEdit)
import Text.Parsec

import Control.Monad.IO.Class
import Data.Bifunctor
import Crypto.Hash (hashWith, SHA256(..))
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as BSC

renderStaticTemplate :: Map.Map Name' String -> StaticDom () -> IO String
renderStaticTemplate config dom = do 
  (_, html) <- renderStatic dom
  pure $ unTemplate config $ T.unpack . T.decodeUtf8 $ html
  
templateSlot :: DomBuilder t m => T.Text -> m ()
templateSlot name = text $ "{{::=" <> name <> "}}"

-- TODO: use template haskell and staticWhich to make this compile time so that it fails
-- if an undefined name does not exist in the Map and can also give warnings when some are not used 
-- Take a String (likely from a file) and replace templates where--  they exist 
unTemplate :: Map.Map String String -> String -> String
unTemplate tmplMap input = streamEdit templateParser mapLookup input
  where
    mapLookup :: Name' -> String
    mapLookup n = case Map.lookup n tmplMap of
      Just v -> v
      Nothing -> error $ "name {{::=" <> n <> "}}" <> " not defined" 


type Name' = String 
templateParser :: Stream s m Char => ParsecT s u m Name'
templateParser = 
  between1 (string "{{::=") (string "}}") validName
  where
    validName :: Stream s m Char => ParsecT s u m Name'
    validName = do
      first_ <- letter <|> (char '_') <|> (char '-') 
      rest <- many $ alphaNum <|> (char '_') <|> (char '-')
      pure $ first_ : rest

between1 :: ParsecT s u m open -> ParsecT s u m end -> ParsecT s u m a -> ParsecT s u m a 
between1 open end match = open *> match <* end


-- Helper func
readFileToTemplateKey' :: MonadIO m => String -> FilePath -> m TemplateVars
readFileToTemplateKey' key fp = do
  x <- liftIO $ readFile fp
  pure $ Map.fromList [ (key,  x)] 

-- for now I'll include Template logic in here

slotKey :: String
slotKey = "slot"

-- | prefixed with "h-" to ensure a valid name
hashString :: String -> String
hashString input =
  let bsInput = BSC.pack input
      digest  = hashWith SHA256 bsInput
  in "h-" <> show (digest) -- :: BS.ByteString)

-- | Standardized templating 
mkTmplValue :: T.Text -> (SlotKey, HtmlString)
mkTmplValue val = (hashString $ T.unpack val, HtmlString val)

-- | Templating with values to help render the template
mkTmplValueA :: T.Text -> a -> (SlotKey, HtmlString, a)
mkTmplValueA val x = (hashString $ T.unpack val, HtmlString val, x)

mkTmplValues :: GroupKey -> [T.Text] -> [(SlotKey, HtmlString)]
mkTmplValues gKey vals = first (gKey <>) . mkTmplValue <$> vals 

mkTmplValuesA :: GroupKey -> [(T.Text,a)] -> [(SlotKey, HtmlString, a)]
mkTmplValuesA _ [] = []
mkTmplValuesA gKey ((val,a):xs) = (gKey <> hashString (T.unpack val), HtmlString val, a) : mkTmplValuesA gKey xs
-- We want to allow for heterogenous inputs and also keep ordering
  -- mkSlotKey input1 : (mkOrderedSlotKeys inputs2) <> (mkOrderedSlotKeys inputs3) 

getHTemplateValues :: HTemplateVars k a -> HTemplateValues
getHTemplateValues = Map.fromList . fmap grab_1_2 . mconcat . Map.elems
  where
    grab_1_2 (_1, _2,_) = (_1,_2)
