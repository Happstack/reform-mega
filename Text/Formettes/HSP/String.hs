{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Formettes.HSP.String where


import HSP
import Text.Formettes
import qualified Text.Formettes.HSP.Common as C

inputText :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputText initialValue = C.inputText getInputString initialValue

inputPassword :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputPassword initialValue = C.inputPassword getInputString initialValue


inputSubmit :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputSubmit initialValue = C.inputSubmit getInputString initialValue

inputReset :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
              String
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset = C.inputReset

inputHidden :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputHidden initialValue = C.inputHidden getInputString initialValue

inputButton :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputButton initialValue = C.inputButton getInputString initialValue



