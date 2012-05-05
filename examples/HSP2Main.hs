{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Text.Blaze ((!), Html)
import Text.Formettes
import Text.Formettes.HSP
import Text.Formettes.Happstack
import HSP.ServerPartT
import Happstack.Server
import Happstack.Server.HSP.HTML

instance FormError String where
    type ErrorInputType String = [Input]
    commonFormError = show


data Greek = Greek
    { alpha :: Bool
    , beta  :: Bool
    , gamma :: Bool
    }
    deriving (Show)

greek :: Greek -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () Greek
greek Greek{..} =
    Greek <$> (label "α" ++> inputCheckbox alpha)
          <*> (label "β" ++> inputCheckbox beta)
          <*> (label "γ" ++> inputCheckbox gamma)

data Vice
    = Sex
    | Drugs
    | RockAndRoll
      deriving (Show, Eq, Enum)
{-
vices :: [Vice] -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () [Vice]
vices vs = inputCheckboxes $ map mkVice [Sex .. RockAndRoll]
    where
      mkVice v = (v, show v, v `elem` vs)
-}
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
    do let greekForm = (greek $ Greek False True False)
           -- viceForm  = vices [Sex, RockAndRoll]
       simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                formHandler greekForm
