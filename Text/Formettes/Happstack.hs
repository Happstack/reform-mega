{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{- |
Support for using Formettes with the Haskell Web Framework Happstack. <http://happstack.com/>
-}
module Text.Formettes.Happstack where

import Control.Applicative                 (Applicative((<*>)), Alternative, (<$>), (<|>), (*>), optional)
import Control.Monad                       (msum, mplus)
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
import Happstack.Server                    (Cookie(..), CookieLife(Session), ContentType, Happstack, Input(..), Method(GET, HEAD, POST), ServerMonad(localRq), ToMessage(..), Request(rqMethod), addCookie, expireCookie, forbidden, lookCookie, lookInputs, look, body, escape, method, mkCookie, getDataFn)

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


addCSRFCookie :: (Happstack m) => String -> m String
addCSRFCookie name =
    do i <- liftIO $ randomIO
       addCookie Session ((mkCookie name (show i)) { httpOnly = True })
       return (show (i :: Integer))

getCSRFCookie :: (Happstack m) => String -> m String
getCSRFCookie name = cookieValue <$> lookCookie name

checkCSRF :: (Happstack m) => String -> m ()
checkCSRF name =
    do mc <- optional $ getCSRFCookie name
       mi <- optional $ look name
       case (mc, mi) of
         (Just c, Just c')
             | c == c' -> return ()
         _ -> escape $ forbidden (toResponse "CSRF check failed.")

-- | turn a formlet into XML+ServerPartT which can be embedded in a larger document
formetteSingle :: (ToMessage b, Happstack m, Alternative m, Monoid view) =>
                  ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a <form> tag
               -> String                                      -- ^ prefix
               -> (a -> m b)                                  -- ^ handler used when form validates
               -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ handler used when form does not validate
               -> Form m [Input] error view proof a           -- ^ the formlet
               -> m view
formetteSingle toForm prefix handleSuccess mHandleFailure form =
    let csrfName = "formettes-csrf-" ++ prefix in
    msum [ do method [GET, HEAD]
              csrfToken <- addCSRFCookie csrfName
              toForm [(csrfName, csrfToken)] <$> viewForm prefix form

         , do method POST
              checkCSRF csrfName
              (v, mresult) <- runForm environment prefix form
              result <- mresult
              case result of
                (Ok a)         ->
                    (escape . fmap toResponse) $ do expireCookie csrfName
                                                    handleSuccess (unProved a)
                (Error errors) ->
                    do csrfToken <- addCSRFCookie csrfName
                       case mHandleFailure of
                         (Just handleFailure) ->
                             (escape . fmap toResponse) $
                               handleFailure errors (toForm [(csrfName, csrfToken)] (unView v errors))
                         Nothing ->
                             return $ toForm [(csrfName, csrfToken)] (unView v errors)
         ]

formette :: (ToMessage b, Happstack m, Alternative m, Monoid view) =>
            ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a <form> tag
         -> String                                      -- ^ prefix
         -> (a -> m b)                                  -- ^ handler used when form validates
         -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ handler used when form does not validate
         -> Form m [Input] error view proof a           -- ^ the formlet
         -> m view

formette toForm prefix success failure form =
    guard prefix (formetteSingle toForm' prefix success failure form)
    where
      toForm' hidden view = toForm (("formname",prefix) : hidden) view
      guard :: (Happstack m) => String -> m a -> m a
      guard formName part =
          (do method POST
              submittedName <- getDataFn (look "formname")
              if (submittedName == (Right formName))
               then part
               else localRq (\req -> req { rqMethod = GET }) part
          ) `mplus` part
