{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Formettes.HSP.String
    ( inputText
    , inputPassword
    , inputSubmit
    , inputReset
    , inputHidden
    , inputButton
    , textarea
    , C.inputFile
    , buttonSubmit
    , C.buttonReset
    , C.button
    , C.label
    , C.inputCheckbox
    , C.inputCheckboxes
    , C.inputRadio
    , C.select
    , C.selectMultiple
    , C.errorList
      -- * layout functions
    , C.br
    ) where

import HSP
import Text.Formettes
import qualified Text.Formettes.HSP.Common as C

inputText :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputText initialValue = C.inputText getInputString initialValue

inputPassword :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
                 Form m input error [XMLGenT x (XMLType x)] () String
inputPassword = C.inputPassword getInputString ""

inputSubmit :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () (Maybe String)
inputSubmit initialValue = C.inputSubmit getInputString initialValue

inputReset :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
              String
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset = C.inputReset

inputHidden :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputHidden initialValue = C.inputHidden getInputString initialValue

inputButton :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () ()
inputButton label = C.inputButton label

textarea :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
            Int
         -> Int
         -> String
         -> Form m input error [XMLGenT x (XMLType x)] () String
textarea rows cols initialValue = C.textarea getInputString rows cols initialValue

buttonSubmit :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, EmbedAsChild x children , EmbedAsAttr x (Attr String FormId)) =>
                String
             -> children
             -> Form m input error [XMLGenT x (XMLType x)] () (Maybe String)
buttonSubmit = C.buttonSubmit getInputString