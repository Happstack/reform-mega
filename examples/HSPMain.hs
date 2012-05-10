{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C
import Text.Blaze ((!), Html)
import Text.Formettes
import Text.Formettes.HSP.String
import Text.Formettes.Happstack
import HSP.ServerPartT
import Happstack.Server
import Happstack.Server.HSP.HTML
import SharedForm

instance (XMLGenerator x) => EmbedAsChild x (DemoFormError [Input]) where
    asChild InvalidEmail    = <%>Email address must contain a @.</%>
    asChild InvalidUsername = <%>Username must not be blank.</%>
    asChild (CommonError (InputMissing fid))        = <%>Internal Error. Input missing: <% show fid %></%>
    asChild (CommonError (NoStringFound input))     = <%>Internal Error. Could not extract a String from: <% show input %></%>
    asChild (CommonError (MultiStringsFound input)) = <%>Internal Error. Found more than one String in: <% show input %></%>

usernameForm :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), Monad m, FormInput input, EmbedAsChild x (DemoFormError input)) =>
                String
             -> Form m input (DemoFormError input) [XMLGenT x (XMLType x)] NotNull Username
usernameForm initialValue =
    errorList ++> (label "username: " ++> (Username <+$+> inputText initialValue `prove` (notNullProof InvalidUsername)))

emailForm :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), Monad m, FormInput input, EmbedAsChild x (DemoFormError input)) =>
             String
          -> Form m input (DemoFormError input) [XMLGenT x (XMLType x)] ValidEmail Email
emailForm initialValue    =
    errorList ++> (label "email: " ++> (Email    <+$+> inputText initialValue `prove` (validEmailProof InvalidEmail)))

userForm :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), Monad m, FormInput input, EmbedAsChild x (DemoFormError input)) =>
            String -- ^ initial username
         -> String -- ^ initial email
         -> Form m input (DemoFormError input) [XMLGenT x (XMLType x)] ValidUser User
userForm nm eml = mkUser <+*+> (usernameForm nm) <+*+> (emailForm eml)

hsxForm :: (XMLGenerator x) => [XMLGenT x (XMLType x)] -> [XMLGenT x (XMLType x)]
hsxForm html =
    [<form action="/" method="POST" enctype="multipart/form-data">
      <% html %>
      <input type="submit" />
     </form>]

formHandler :: (EmbedAsChild (ServerPartT IO) error, Show a) =>
               Form (ServerPartT IO) [Input] error [XMLGenT (ServerPartT IO) XML] proof a
            -> ServerPart Response
formHandler form =
        msum [ do method GET
                  formHtml <- viewForm "user" form
                  html <- unXMLGenT $ <html><% hsxForm $ formHtml %></html>
                  ok $ toResponse $ html

             , do method POST
                  r <- eitherForm "user" environment form
                  case r of
                    (Right a) -> ok $ toResponse $ show a
                    (Left view) ->
                        do html <- unXMLGenT $ <html><% hsxForm $ view %></html>
                           ok $ toResponse $ html

             ]

main :: IO ()
main =
    do let form = userForm "" ""
       simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                formHandler form



{-
    H.form ! A.action "/"
           ! A.method "POST"
           ! A.enctype "multipart/form-data" $
             do html
                H.input ! A.type_ "submit"
-}
{-
blazeResponse :: Html -> Response
blazeResponse html = toResponseBS (C.pack "text/html;charset=UTF-8") $ renderHtml html


main :: IO ()
main =
    do let form = userForm "" ""
       simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                formHandler form
-}