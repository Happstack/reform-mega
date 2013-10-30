{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
{-# OPTIONS_GHC -F -pgmFhsx2hs #-}
module Main where

import Control.Applicative.Indexed
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Text.Lazy (Text)
import Text.Blaze.Html ((!), Html)
import Text.Reform
import Text.Reform.HSP.String
import Text.Reform.Happstack
import HSP
import HSP.Monad
import Happstack.Server
import Happstack.Server.HSP.HTML
import SharedForm

type Server = HSPT XML (ServerPartT IO)
type ServerForm = Form Server [Input] (DemoFormError [Input]) [XMLGenT Server XML]


instance (XMLGenerator x) => EmbedAsChild x (DemoFormError [Input]) where
    asChild InvalidEmail    = <%>Email address must contain a @.</%>
    asChild InvalidUsername = <%>Username must not be blank.</%>
    asChild (CommonError (InputMissing fid))        = <%>Internal Error. Input missing: <% show fid %></%>
    asChild (CommonError (NoStringFound input))     = <%>Internal Error. Could not extract a String from: <% show input %></%>
    asChild (CommonError (MultiStringsFound input)) = <%>Internal Error. Found more than one String in: <% show input %></%>

instance EmbedAsAttr Server (Attr Text String)

usernameForm :: String
             -> ServerForm NotNull Username
usernameForm initialValue =
    errorList ++> (label "username: " ++> (Username <<$>> inputText initialValue `prove` (notNullProof InvalidUsername)))

emailForm :: String
          -> ServerForm ValidEmail Email
emailForm initialValue    =
    errorList ++> (label "email: " ++> (Email    <<$>> inputText initialValue `prove` (validEmailProof InvalidEmail)))

userForm :: String -- ^ initial username
         -> String -- ^ initial email
         -> ServerForm ValidUser User
userForm nm eml = mkUser <<*>> (usernameForm nm) <<*>> (emailForm eml)

hsxForm :: (XMLGenerator x, StringType x ~ Text) => [XMLGenT x (XMLType x)] -> [XMLGenT x (XMLType x)]
hsxForm html =
    [<form action="/" method="POST" enctype="multipart/form-data">
      <% html %>
      <input type="submit" />
     </form>]

formHandler :: ServerForm proof a
            -> Server Response
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
{-
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
-}-}