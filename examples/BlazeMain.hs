{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Text.Blaze ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Formettes
import Text.Formettes.Blaze
import Text.Formettes.Happstack
import Happstack.Server
import SharedForm

instance H.ToHtml (DemoFormError [Input]) where
    toHtml InvalidEmail    = "Email address must contain a @."
    toHtml InvalidUsername = "Username must not be blank."
    toHtml (CommonError (InputMissing fid))        = H.toHtml $ "Internal Error. Input missing: " ++ show fid
    toHtml (CommonError (NoStringFound input))     = H.toHtml $ "Internal Error. Could not extract a String from: " ++ show input
    toHtml (CommonError (MultiStringsFound input)) = H.toHtml $ "Internal Error. Found more than one String in: " ++ show input

usernameForm :: (Monad m, FormInput input, H.ToHtml (DemoFormError input)) =>
                     String
                  -> Form m input (DemoFormError input) Html NotNull Username
usernameForm initialValue =
    errorList ++> (label "username: " ++> (Username <+$+> inputString initialValue `prove` (notNullProof InvalidUsername)))

emailFormBlaze :: (Monad m, FormInput input, H.ToHtml (DemoFormError input)) =>
                  String
               -> Form m input (DemoFormError input) Html ValidEmail Email
emailFormBlaze initialValue    =
    errorList ++> (label "email: " ++> (Email    <+$+> inputString initialValue `prove` (validEmailProof InvalidEmail)))

userForm :: (Monad m, FormInput input, H.ToHtml (DemoFormError input)) =>
            String -- ^ initial username
         -> String -- ^ initial email
         -> Form m input (DemoFormError input) Html ValidUser User
userForm nm eml = mkUser <+*+> (usernameForm nm) <+*+> (emailFormBlaze eml)

blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html

blazeForm :: Html -> Html
blazeForm html =
    H.form ! A.action "/"
           ! A.method "POST"
           ! A.enctype "multipart/form-data" $
             do html
                H.input ! A.type_ "submit"

formHandler :: (H.ToHtml error, Show a) => Form (ServerPartT IO) [Input] error Html proof a -> ServerPart Response
formHandler form =
        msum [ do method GET
                  html <- viewForm "user" form
                  ok $ blazeResponse $ blazeForm html

             , do method POST
                  r <- eitherForm "user" environment form
                  case r of
                    (Right a) -> ok $ toResponse $ show a
                    (Left view) ->
                              ok $ blazeResponse $ blazeForm view

             ]

mainBlaze :: IO ()
mainBlaze =
    do let form = userForm "" ""
       simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                formHandler form

