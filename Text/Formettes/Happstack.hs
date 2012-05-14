{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{- |
Support for using Formettes with the Haskell Web Framework Happstack. <http://happstack.com/>
-}
module Text.Formettes.Happstack where

import Control.Applicative                 (Applicative((<*>)), Alternative, (<$>), (<|>), (*>), optional)
import Control.Monad                       (msum)
import Control.Monad.Trans                 (liftIO)
import Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Either                         (lefts, rights)
import Data.Maybe                          (mapMaybe)
import Data.Monoid                         (Monoid)
import System.Random                       (randomIO)
import Text.Formettes.Backend              (FormInput(..), FileType, CommonFormError(NoFileFound, MultiFilesFound), commonFormError)
import Text.Formettes.Core                 (IndexedApplicative(..), Environment(..), Form, Proved(..), Value(..), View(..), (++>), eitherForm, runForm, mapView, viewForm)
import Text.Formettes.Result               (Result(..), FormRange)
import Happstack.Server                    (Cookie(..), CookieLife(Session), ContentType, Happstack, Input(..), Method(GET, HEAD, POST), ToMessage(..), addCookie, forbidden, lookCookie, lookInputs, look, body, escape, method, mkCookie)

-- FIXME: we should really look at Content Type and check for non-UTF-8 encodings
instance FormInput [Input] where
    type FileType [Input] = (FilePath, FilePath, ContentType)
    getInputStrings inputs = map UTF8.toString $ rights $ map inputValue inputs
    getInputFile inputs =
        case [ (tmpFilePath, uploadName, contentType) | (Input (Left tmpFilePath) (Just uploadName) contentType) <- inputs ] of
          [(tmpFilePath, uploadName, contentType)] -> Right (tmpFilePath, uploadName, contentType)
          []   -> Left (commonFormError $ NoFileFound inputs)
          _    -> Left (commonFormError $ MultiFilesFound inputs)

-- | create an 'Environment' to be used with 'runForm'
environment :: (Happstack m) => Environment m [Input]
environment =
    Environment $ \formId ->
        do ins <- lookInputs (show formId)
           case ins of
             []  -> return $ Missing
             _   -> return $ Found ins
-- | an alias for, 'eitherForm environment'
happstackForm :: (Happstack m) =>
                 String                            -- ^ form prefix
              -> Form m [Input] error view proof a -- ^ Form to run
              -> m (Either view a)                 -- ^ Result
happstackForm = eitherForm environment


addCSRFCookie :: (Happstack m) => m String
addCSRFCookie =
    do i <- liftIO $ randomIO
       addCookie Session ((mkCookie "formettes-csrf" (show i)) { httpOnly = True })
       return (show (i :: Integer))

getCSRFCookie :: (Happstack m) => m String
getCSRFCookie = cookieValue <$> lookCookie "formettes-csrf"

checkCSRF :: (Happstack m) => m ()
checkCSRF =
    do mc <- optional $ getCSRFCookie
       mi <- optional $ look "formettes-csrf"
       case (mc, mi) of
         (Just c, Just c')
             | c == c' -> return ()
         _ -> escape $ forbidden (toResponse "CSRF check failed.")

-- | turn a formlet into XML+ServerPartT which can be embedded in a larger document
formette :: (ToMessage b, Happstack m, Alternative m, Monoid view) =>
            ((String, String) -> [(String, String)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
         -> String                                      -- ^ prefix
         -> (a -> m b)                                  -- ^ handler used when form validates
         -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ handler used when form does not validate
         -> Form m [Input] error view proof a           -- ^ the formlet
         -> m view
formette toForm prefix handleSuccess mHandleFailure form =
    msum [ do method [GET, HEAD]
              csrfToken <- addCSRFCookie
              toForm ("formettes-csrf", csrfToken) [] <$> viewForm prefix form

         , do method POST
              checkCSRF
              (v, mresult) <- runForm environment prefix form
              result <- mresult
              case result of
                (Ok a)    -> (escape . fmap toResponse) $ handleSuccess (unProved a)
                (Error errors) ->
                    do csrfToken <- addCSRFCookie
                       case mHandleFailure of
                         (Just handleFailure) ->
                             (escape . fmap toResponse) $
                               handleFailure errors (toForm ("formettes-csrf", csrfToken) [] (unView v errors))
                         Nothing ->
                             return $ toForm ("formettes-csrf", csrfToken) [] (unView v errors)
         ]

{-
multiFormette ::
  (EmbedAsChild m xml, EmbedAsAttr m (Attr String String), EmbedAsAttr m (Attr String Text), ToMessage b, Happstack m, Alternative m)
  => String -- ^ unique name for the formlet
  -> Text -- ^ url to POST form results to
  -> (a -> m b) -- ^ handler used when form validates
   -> Maybe ([(FormRange, e)] -> [XMLGenT m (XMLType m)] -> m b) -- ^ handler used when form does not validate
  -> Form m [Input] e xml a      -- ^ the formlet
  -> XMLGenT m (XMLType m)
multiFormette name action success failure form =
    guard name (formette name (action `Text.append` (Text.pack $ "?form=" ++ name)) success failure form)
    where
      guard :: (Happstack m) => String -> m a -> m a
      guard formName part =
          (do method POST
              submittedName <- getDataFn (look "form")
              if (submittedName == (Right formName))
               then part
               else localRq (\req -> req { rqMethod = GET }) part
          ) `mplus` part
-}