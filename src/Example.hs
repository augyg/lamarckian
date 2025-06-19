{-# LANGUAGE QuasiQuotes #-}
module Landing where

import Landing.Impl

import Backend.DB.UserInfo
import Backend.DB (toAcctID)
import Common.Route
import Common.Types
import Backend.Config 
import Backend.Utils.Log
import Backend.Utils.HttpJson

import Obelisk.Route

import Snap
import Snap.Util.GZip
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS




-- import qualified Landing.Static as Landing
-- import Landing.Utils


-- | TODO: given that we are using Template Haskell here, there is really nothing stopping us from
-- | automatically uploading to github, which could be used for netlify or similar IO effects 
serveStaticSite :: MonadSnap m => R LandingRoute -> m () 
serveStaticSite = \case  
  LandingBase :/ _ -> do
    serveCompressedWithReport $(getPageCompiled (LandingBase :/ mempty))
  Pricing :/ () -> serveCompressedWithReport $(getPageCompiled (Pricing :/ ()))
  FAQ :/ () -> serveCompressedWithReport $(getPageCompiled (FAQ :/ ()))
  ForTalent :/ () -> serveCompressedWithReport $(getPageCompiled (ForTalent :/ ()))
  AboutUs :/ () -> serveCompressedWithReport $(getPageCompiled (AboutUs :/ ()))
  SolutionFor :/ solRoute -> case solRoute of 
    Bootcamps :/ () -> serveCompressedWithReport $(getPageCompiled (SolutionFor :/ Bootcamps :/ ()))
    Recruiters :/ () -> serveCompressedWithReport $(getPageCompiled (SolutionFor :/ Recruiters :/ ()))
    Communities :/ () ->  serveCompressedWithReport $(getPageCompiled (SolutionFor :/ Communities :/ ()))
    JobSeekers :/ () -> serveCompressedWithReport $(getPageCompiled (SolutionFor :/ JobSeekers :/ ()))
  
  Blog :/ blogRoute -> case blogRoute of
    BlogMain :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ BlogMain :/ ()))
    README :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ README :/ ())) 
    SetupInstructions :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ SetupInstructions :/ ())) 
    Introduction :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Introduction :/ ()))
    FirstProgram :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ FirstProgram :/ ()))
    Chapter0 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter0 :/ ()))
    Chapter1 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter1 :/ ()))
    Chapter2 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter2 :/ ())) 
    Chapter3 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter3 :/ ()))
    Chapter4 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter4 :/ ()))
    Chapter5 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter5 :/ ()))
    Chapter6 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter6 :/ ()))
    Chapter7 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter7 :/ ()))
    Chapter8 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter8 :/ ()))
    Chapter9 :/ () -> serveCompressedWithReport $(getPageCompiled (Blog :/ Chapter9 :/ ()))


serveCompressed :: MonadSnap m => FilePath -> m T.Text
serveCompressed fp = do
  withCompression $ do
    writeLBS =<< liftIO (LBS.readFile fp)
  let pageName = dropExtension . takeFileName $ fp
  pure $ T.pack pageName 

serveCompressedWithReport :: MonadSnap m => FilePath -> m ()
serveCompressedWithReport fp = do
  pageName <- serveCompressed fp
  let getAccountIdFromCookies = Left ""
  eithAcctId <- getAccountIdFromCookies
  case matchLesson pageName of
    Nothing -> reportLog False $ LandingPageRequested pageName
    Just lessonId  -> do
      case eithAcctId of
        Left _ -> reportLog False $ UserRequestedLesson Nothing lessonId
        Right aid -> do
          email <- withDbEnv $ getUsersEmail $ toAcctID aid
          reportLog False $ UserRequestedLesson (Email <$> email) lessonId 

  --writeLBS =<< findReadStaticFile fp
  --writeLBS =<< liftIO (LBS.readFile fp)
  

-- TODO(galen): make more self-maintained by using Show of the Route GADT
matchLesson :: T.Text -> Maybe LessonId
matchLesson pName
  | T.isInfixOf "README" pName = Just README_Main
  | T.isInfixOf "SetupInstructions" pName = Just LessonSetup 
  | T.isInfixOf "Introduction" pName = Just Intro
  | T.isInfixOf "Chapter0" pName = Just Lesson0
  | T.isInfixOf "Chapter1" pName = Just Lesson1
  | T.isInfixOf "Chapter2" pName = Just Lesson2
  | T.isInfixOf "Chapter3" pName = Just Lesson3
  | T.isInfixOf "Chapter4" pName = Just Lesson4
  | T.isInfixOf "Chapter5" pName = Just Lesson5
  | T.isInfixOf "Chapter6" pName = Just Lesson6
  | T.isInfixOf "Chapter7" pName = Just Lesson7
  | T.isInfixOf "Chapter8" pName = Just Lesson8
  | T.isInfixOf "Chapter9" pName = Just Lesson9
  | T.isInfixOf "Chapter10" pName = Just Lesson10
  | T.isInfixOf "Chapter11" pName = Just Lesson11
  | T.isInfixOf "Chapter12" pName = Just Lesson12
  | T.isInfixOf "Chapter13" pName = Just Lesson13
  | otherwise = Nothing

  
 
