{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances, RecordWildCards #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Text (Text)
import Text.Blaze ((!), Html)
import Text.Formettes
import Text.Formettes.HSP.String
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

vices :: [Vice] -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () [Vice]
vices vs = inputCheckboxes (map mkVice [Sex .. RockAndRoll]) (`elem` vs)
    where
      mkVice v = (v, show v)

vices2 :: [Vice] -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () [Vice]
vices2 vs = selectMultiple (map mkVice [Sex .. RockAndRoll]) (`elem` vs)
    where
      mkVice v = (v, show v)

data Stars
    = OneStar
    | TwoStars
    | ThreeStars
    | FourStars
    | FiveStars
      deriving (Eq, Enum)

instance Show Stars where
    show OneStar    = "★"
    show TwoStars   = "★★"
    show ThreeStars = "★★★"
    show FourStars  = "★★★★"
    show FiveStars  = "★★★★★"

stars :: Stars -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () Stars
stars def =
    inputRadio [(s, show s) | s <- [OneStar .. FiveStars]] (== def)

selectStars :: Stars -> Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () Stars
selectStars def =
    select [(s, show s) | s <- [OneStar .. FiveStars]] (== def)

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
                  (view, r) <- runForm' "user" environment form
                  case r of
                    (Just a) ->
                        do html <- unXMLGenT $ <html>
                                                 <% show a %>
                                                 <% hsxForm $ view %>
                                               </html>
                           ok $ toResponse $ html
                    Nothing ->
                        do html <- unXMLGenT $ <html><% hsxForm $ view %></html>
                           ok $ toResponse $ html

             ]

data BigForm = BigForm
    { text         :: String
    , pwd          :: String
    , hidden       :: String
    , speech       :: String
    , file         :: (FilePath, FilePath, ContentType)
    , check        :: Bool
    , viceChecks   :: [Vice]
    , viceMulti    :: [Vice]
    , starsRadio   :: Stars
    , starsSelect  :: Stars
    , intButton    :: Maybe String
    , submit       :: Maybe String
    }
    deriving Show

bigForm :: Form (ServerPartT IO) [Input] String [XMLGenT (ServerPartT IO) XML] () BigForm
bigForm =
    BigForm <$> (label "username:" ++> inputText "")              <* br
            <*> (label "password:" ++> inputPassword)             <* br
            <*> inputHidden "It's a secret to everybody."
            <*> (label "a little speech" ++> (textarea 80 10 "")) <* br
            <*> (label "a file, any file" ++> inputFile)          <* br
            <*> label "check this box if appropriate: " ++> inputCheckbox True <* br
            <*> vices [Sex, RockAndRoll]                          <* br
            <*> vices2 [Drugs, RockAndRoll]                       <* br
            <*> stars ThreeStars                                  <* br
            <*> selectStars ThreeStars                            <* br
            <*> buttonSubmit "123" <b>123</b>                     <* br
            <*> inputSubmit "submit!!"
            <* inputButton "little button that doesn't do anything"
            <* inputReset "reeeeset!"
            <* buttonReset <b>ReSeT!</b>
            <* button <span>Another button that does <i>nothing</i></span>


main :: IO ()
main =
    do let greekForm  = (greek $ Greek False True False)
           viceForm   = vices  [Sex, RockAndRoll]
           vice2Form  = vices2 [Sex, RockAndRoll]
           starsForm  = stars TwoStars
       simpleHTTP nullConf $ do decodeBody (defaultBodyPolicy "/tmp" 0 10000 10000)
                                formHandler bigForm
