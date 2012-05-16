{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
{- |
Support for using Reform with the Haskell Web Framework Happstack. <http://happstack.com/>
-}
module Text.Reform.Happstack where

import Control.Applicative                 (Applicative((<*>)), Alternative, (<$>), (<|>), (*>), optional)
import Control.Applicative.Indexed         (IndexedApplicative(..))
import Control.Monad                       (msum, mplus)
import Control.Monad.Trans                 (liftIO)
import Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Either                         (lefts, rights)
import Data.Maybe                          (mapMaybe)
import Data.Monoid                         (Monoid)
import System.Random                       (randomIO)
import Text.Reform.Backend              (FormInput(..), FileType, CommonFormError(NoFileFound, MultiFilesFound), commonFormError)
import Text.Reform.Core                 (Environment(..), Form, Proved(..), Value(..), View(..), (++>), eitherForm, runForm, mapView, viewForm)
import Text.Reform.Result               (Result(..), FormRange)
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
-- | alias for 'eitherForm environment'
happstackEitherForm :: (Happstack m) =>
                       String                            -- ^ form prefix
                    -> Form m [Input] error view proof a -- ^ Form to run
                    -> m (Either view a)                 -- ^ Result
happstackEitherForm = eitherForm environment

-- | similar to 'eitherForm environment' but includes double-submit
-- (Cross Site Request Forgery) CSRF protection.
--
-- The form must have been created using 'csrfViewForm'
--
-- see also: 'csrfViewForm'
csrfEitherForm :: (Happstack m) =>
                  ([(String, String)] -> view -> view) -- ^ wrap raw form html inside a <form> tag
               -> String                               -- ^ form prefix
               -> Form m [Input] error view proof a    -- ^ Form to run
               -> m (Either view a)                    -- ^ Result
csrfEitherForm toForm prefix frm =
    do checkCSRF csrfName
       -- expireCookie csrfName
       r <- eitherForm environment prefix frm
       case r of
         (Left view) -> Left <$> csrfView toForm prefix view
         (Right a)   -> return (Right a)

-- | similar to 'viewForm' but includes double-submit
-- (Cross Site Request Forgery) CSRF protection.
--
-- Must be used with 'csrfEitherForm'.
--
-- see also: 'csrfEitherForm'.
csrfViewForm :: (Happstack m) =>
                ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a @\<form\>@ tag
             -> String
             -> Form m input error view proof a
             -> m view
csrfViewForm toForm prefix frm =
    do formChildren <- viewForm prefix frm
       csrfView toForm prefix formChildren

-- | Utility Function: wrap the @view@ in a @\<form\>@ that includes
-- double-submit CSRF protection.
--
-- calls 'addCSRFCookie' to set the cookie and adds the token as a
-- hidden field.
--
-- see also: 'csrfViewForm', 'csrfEitherForm', 'checkCSRF'
csrfView :: (Happstack m) =>
            ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a @\<form\>@ tag
         -> String
         -> view
         -> m view
csrfView toForm prefix view =
    do csrfToken <- addCSRFCookie csrfName
       return (toForm [(csrfName, csrfToken)] view)

-- | Utility Function: add a cookie for CSRF protection
addCSRFCookie :: (Happstack m) =>
                 String    -- ^ name to use for the cookie
              -> m String
addCSRFCookie name =
    do mc <- optional $ lookCookie name
       case mc of
         Nothing ->
             do i <- liftIO $ randomIO
                addCookie Session ((mkCookie name (show i)) { httpOnly = True })
                return (show (i :: Integer))
         (Just c) ->
             return (cookieValue c)

-- | Utility Function: get CSRF protection cookie
getCSRFCookie :: (Happstack m) => String -> m String
getCSRFCookie name = cookieValue <$> lookCookie name

-- | Utility Function: check that the CSRF cookie and hidden field exist and are equal
--
-- If the check fails, this function will call:
--
-- > escape $ forbidden (toResponse "CSRF check failed.")
checkCSRF :: (Happstack m) => String -> m ()
checkCSRF name =
    do mc <- optional $ getCSRFCookie name
       mi <- optional $ look name
       case (mc, mi) of
         (Just c, Just c')
             | c == c' -> return ()
         _ -> escape $ forbidden (toResponse "CSRF check failed.")

-- | generate the name to use for the csrf cookie
--
-- Currently this returns the static cookie "reform-csrf". Using the prefix would allow 
csrfName :: String
csrfName = "reform-csrf"

-- | This function allows you to embed a a single 'Form' into a HTML page.
--
-- In general, you will want to use the 'reform' function instead,
-- which allows more than one 'Form' to be used on the same page.
--
-- see also: 'reform'
reformSingle :: (ToMessage b, Happstack m, Alternative m, Monoid view) =>
                  ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a <form> tag
               -> String                                      -- ^ prefix
               -> (a -> m b)                                  -- ^ handler used when form validates
               -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ handler used when form does not validate
               -> Form m [Input] error view proof a           -- ^ the formlet
               -> m view
reformSingle toForm prefix handleSuccess mHandleFailure form =
    msum [ do method [GET, HEAD]
              csrfToken <- addCSRFCookie csrfName
              toForm [(csrfName, csrfToken)] <$> viewForm prefix form

         , do method POST
              checkCSRF csrfName
              (v, mresult) <- runForm environment prefix form
              result <- mresult
              case result of
                (Ok a)         ->
                    (escape . fmap toResponse) $ do -- expireCookie csrfName
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

-- | this function embeds a 'Form' in an HTML page.
--
-- When the page is requested with a 'GET' request, the form view will
-- be rendered.
--
-- When the page is requested with a 'POST' request, the form data
-- will be extracted and validated.
--
-- If a value is successfully produced the success handler will be
-- called with the value.
--
-- On failure the failure handler will be called. If no failure
-- handler is provided, then the page will simply be redisplayed. The
-- form will be rendered with the errors and previous submit data shown.
--
-- The first argument to 'reform' is a function which generates the
-- @\<form\>@ tag. It should generally come from the template library
-- you are using, such as the @form@ function from @reform-hsp@.
--
-- The @[(String, String)]@ argument is a list of '(name, value)'
-- pairs for extra hidden fields that should be added to the
-- @\<form\>@ tag. These hidden fields are used to provide cross-site
-- request forgery (CSRF) protection, and to support multiple forms on
-- the same page.
reform :: (ToMessage b, Happstack m, Alternative m, Monoid view) =>
            ([(String, String)] -> view -> view)        -- ^ wrap raw form html inside a @\<form\>@ tag
         -> String                                      -- ^ prefix
         -> (a -> m b)                                  -- ^ success handler used when form validates
         -> Maybe ([(FormRange, error)] -> view -> m b) -- ^ failure handler used when form does not validate
         -> Form m [Input] error view proof a           -- ^ the formlet
         -> m view

reform toForm prefix success failure form =
    guard prefix (reformSingle toForm' prefix success failure form)
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

