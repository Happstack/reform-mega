{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, OverloadedStrings #-}
module Main where

import Blaze.ByteString.Builder (toLazyByteString)
import Control.Monad
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Text as Text
import Happstack.Server
import Text.Blaze ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Formettes
import Text.Formettes.Heist
import Text.Formettes.Happstack
import Text.Templating.Heist
import qualified Text.XmlHtml as X
import Text.Templating.Heist.TemplateDirectory (newTemplateDirectory')
import SharedForm

usernameForm :: (Monad m, FormInput input) =>
                     String
                  -> Form m input (DemoFormError input) RefMap NotNull Username
usernameForm initialValue =
    (Username <+$+> (inputText "username" initialValue) `prove` (notNullProof InvalidUsername))

emailForm :: (Monad m, FormInput input) =>
                  String
               -> Form m input (DemoFormError input) RefMap ValidEmail Email
emailForm initialValue    =
    (Email    <+$+> inputText "email" initialValue `prove` (validEmailProof InvalidEmail))

userForm :: (Monad m, FormInput input) =>
            String -- ^ initial username
         -> String -- ^ initial email
         -> Form m input (DemoFormError input) RefMap ValidUser User
userForm nm eml = mkUser <+*+> (usernameForm nm) <+*+> (emailForm eml)

formHandler :: (Show a, Show error) => HeistState (ServerPartT IO) -> Form (ServerPartT IO) [Input] error RefMap proof a -> ServerPart Response
formHandler ts form =
        msum [ do method GET
                  refMap <- viewForm "form" form
                  ok ()
                  renderHS (bindSplices (formettesSplices refMap []) ts) "form"

             , do method POST
                  (view', mr) <- runForm "form" environment form
                  r <- mr
                  case r of
                    (Ok a) -> ok $ toResponse $ show (unProved a)
                    (Error errs) ->
                           do let refMap   = unView view' errs
                                  textErrs = map (\(fr, e) -> (fr, Text.pack $ show e)) errs
                              ok ()
                              renderHS (bindSplices (formettesSplices refMap textErrs) ts) "form"
             ]

main :: IO ()
main =
    do let form = userForm "" ""
       ets <- loadTemplates "heist" (defaultHeistState :: HeistState (ServerPartT IO))
       case ets of
         (Left e) -> error e
         (Right ts) ->
             simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                      formHandler ts form


-- | render the specified template
renderHS :: (MonadPlus m, MonadIO m) =>
         HeistState m  -- ^ 'HeistState' handle
      -> C.ByteString           -- ^ template name
      -> m Response
renderHS ts template = do
    t     <- renderTemplate ts template
    flip (maybe mzero) t $ \(builder, mimeType) -> do
        return (toResponseBS mimeType (toLazyByteString builder))
