{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{- |
This module provides functions creating Reform using blaze-html markup.

This module assumes that you wish for text based controls such as 'inputText' and 'textarea' to using 'String' values. If you prefer 'Data.Text.Text' see "Text.Reform.Blaze.Text".

-}
module Text.Reform.Blaze.String
    ( -- * \<input\> element
      inputText
    , inputPassword
    , inputSubmit
    , inputReset
    , inputHidden
    , inputButton
    , C.inputCheckbox
    , C.inputCheckboxes
    , C.inputRadio
    , C.inputFile
      -- * \<textarea\> element
    , textarea
      -- * \<button\> element
    , buttonSubmit
    , C.buttonReset
    , C.button
      -- * \<select\> element
    , C.select
    , C.selectMultiple
      -- * \<label\> element
    , C.label
      -- * errors
    , C.errorList
    , C.childErrorList
      -- * layout functions
    , C.br
    , C.fieldset
    , C.ol
    , C.ul
    , C.li
    , C.form
    ) where

import Text.Blaze.Html (Html, ToMarkup)
import Text.Reform
import qualified Text.Reform.Blaze.Common as C

-- | Create an @\<input type=\"text\"\>@ element
inputText :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               String -- ^ initial value
            -> Form m input error Html () String
inputText initialValue = C.inputText getInputString initialValue

-- | Create an @\<input type=\"password\"\>@ element
inputPassword :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
                 Form m input error Html () String
inputPassword = C.inputPassword getInputString ""

-- | Create an @\<input type=\"submit\"\>@ element
--
-- returns:
--
--   [@Just@ /value/] if this button was used to submit the form.
--
--   [@Nothing@]    if this button was not used to submit the form.
inputSubmit :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               String -- ^ @value@ attribute. Used for button label, and value if button is submitted.
            -> Form m input error Html () (Maybe String)
inputSubmit initialValue = C.inputSubmit getInputString initialValue

-- | Create an @\<input type=\"reset\"\>@ element
--
-- This element does not add any data to the form data set.
inputReset :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
              String -- ^ value attribute. Used only to label the button.
           -> Form m input error Html () ()
inputReset = C.inputReset

-- | Create an @\<input type=\"hidden\"\>@ element
inputHidden :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               String -- ^ value to store in the hidden element
            -> Form m input error Html () String
inputHidden initialValue = C.inputHidden getInputString initialValue

-- | Create an @\<input type=\"button\"\>@ element
--
-- The element is a push button with a text label. The button does nothing by default, but actions can be added using javascript. This element does not add any data to the form data set.
--
-- see also: 'C.button'
inputButton :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               String -- ^ value attribute. Used to label the button.
            -> Form m input error Html () ()
inputButton label = C.inputButton label

-- | Create a \<textarea\>\<\/textarea\> element
textarea :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
            Int    -- ^ cols
         -> Int    -- ^ rows
         -> String -- ^ initial contents
         -> Form m input error Html () String
textarea rows cols initialValue = C.textarea getInputString rows cols initialValue

-- | create a  @\<button type=\"submit\"\>\<\/button\>@ element
buttonSubmit :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, ToMarkup children) =>
                String -- ^ value attribute. Returned if this button submits the form.
             -> children -- ^ children to embed in the \<button\>
             -> Form m input error Html () (Maybe String)
buttonSubmit = C.buttonSubmit getInputString
