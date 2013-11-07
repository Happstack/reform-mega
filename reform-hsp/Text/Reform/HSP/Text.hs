{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{- |
This module provides functions creating Reform using HSP markup.

This module assumes that you wish for text based controls such as 'inputText' and 'textarea' to using 'Text' values. If you prefer 'String' see "Text.Reform.HSP.String".
-}
module Text.Reform.HSP.Text
    ( -- * \<input\> element
      inputText
    , inputPassword
    , inputSubmit
    , inputReset
    , inputHidden
    , inputButton
    , inputCheckbox
    , inputCheckboxes
    , inputRadio
    , inputRadioForms
    , inputFile
      -- * \<textarea\> element
    , textarea
      -- * \<button\> element
    , buttonSubmit
    , buttonReset
    , button
      -- * \<select\> element
    , select
    , selectMultiple
      -- * \<label\> element
    , label
    , labelText
      -- * errors
    , errorList
    , childErrorList
      -- * layout functions
    , br
    , fieldset
    , ol
    , ul
    , li
    , form
    , setAttrs
    ) where

import Data.Text (empty)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import HSP.XMLGenerator
import Text.Reform
import qualified Text.Reform.HSP.Common as C

-- | Create an @\<input type=\"text\"\>@ element
inputText :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text)) =>
               T.Text -- ^ initial value
            -> Form m input error [XMLGenT x (XMLType x)] () T.Text
inputText initialValue = C.inputText getInputText initialValue

-- | Create an @\<input type=\"password\"\>@ element
inputPassword :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text)) =>
                 Form m input error [XMLGenT x (XMLType x)] () T.Text
inputPassword = C.inputPassword getInputText empty

-- | Create an @\<input type=\"submit\"\>@ element
--
-- returns:
--
--   [@Just@ /value/] if this button was used to submit the form.
--
--   [@Nothing@]    if this button was not used to submit the form.
inputSubmit :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text)) =>
               T.Text -- ^ @value@ attribute. Used for button label, and value if button is submitted.
            -> Form m input error [XMLGenT x (XMLType x)] () (Maybe T.Text)
inputSubmit initialValue = C.inputSubmit getInputText initialValue

-- | Create an @\<input type=\"reset\"\>@ element
--
-- This element does not add any data to the form data set.
inputReset :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text)) =>
              T.Text -- ^ value attribute. Used only to label the button.
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset = C.inputReset

-- | Create an @\<input type=\"hidden\"\>@ element
inputHidden :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text)) =>
               T.Text -- ^ value to store in the hidden element
            -> Form m input error [XMLGenT x (XMLType x)] () T.Text
inputHidden initialValue = C.inputHidden getInputText initialValue

-- | Create an @\<input type=\"button\"\>@ element
--
-- The element is a push button with a text label. The button does nothing by default, but actions can be added using javascript. This element does not add any data to the form data set.
--
-- see also: 'C.button'
inputButton :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text Text)) =>
               Text -- ^ value attribute. Used to label the button.
            -> Form m input error [XMLGenT x (XMLType x)] () ()
inputButton label = C.inputButton label

-- | Create a \<textarea\>\<\/textarea\> element
textarea :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text T.Text), EmbedAsChild x T.Text) =>
            Int    -- ^ cols
         -> Int    -- ^ rows
         -> T.Text -- ^ initial contents
         -> Form m input error [XMLGenT x (XMLType x)] () T.Text
textarea rows cols initialValue = C.textarea getInputText rows cols initialValue

-- | create a  @\<button type=\"submit\"\>\<\/button\>@ element
buttonSubmit :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x  (Attr Text T.Text)) =>
                T.Text -- ^ value attribute. Returned if this button submits the form.
             -> children -- ^ children to embed in the \<button\>
             -> Form m input error [XMLGenT x (XMLType x)] () (Maybe T.Text)
buttonSubmit = C.buttonSubmit getInputText

--------------------------------------------------------------------------------
-- re-exports from .Common. In theory we could just put the docs in .Common,
-- but, currently HSX strips them out.

-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
inputCheckbox :: forall x error input m. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId)) =>
                   Bool  -- ^ initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () Bool
inputCheckbox = C.inputCheckbox

-- | Create a group of @\<input type=\"checkbox\"\>@ elements
--
inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                   [(a, lbl)]  -- ^ (value, label)
                -> (a -> Bool) -- ^ function which marks if a value should be checked (aka, selected) initially or not. Can match zero or more elements.
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes = C.inputCheckboxes

-- | Create a group of @\<input type=\"radio\"\>@ elements
inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
              [(a, lbl)]  -- ^ (value, label)
           -> (a -> Bool) -- ^ predicate which returns @True@ if @a@ should be initially checked. Must match exactly one value in the previous argument.
           -> Form m input error [XMLGenT x (XMLType x)] () a
inputRadio = C.inputRadio

-- | Create a group of radio buttons that select between sub-forms
inputRadioForms :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
              [(Form m input error [XMLGenT x (XMLType x)] proof a, lbl)]  -- ^ value, label, initially checked
           -> a -- ^ default
           -> Form m input error [XMLGenT x (XMLType x)] proof a
inputRadioForms = C.inputRadioForms

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be \"\" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId)) =>
             Form m input error [XMLGenT x (XMLType x)] () (FileType input)
inputFile = C.inputFile

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset :: ( Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId)
                ) =>
               children -- ^ children of the @<\/button\>@ element
             -> Form m input error [XMLGenT x (XMLType x)] () ()
buttonReset = C.buttonReset

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId)) =>
          children -- ^ children to embed in the \<button\>
       -> Form m input error [XMLGenT x (XMLType x)] () ()
button = C.button

-- | create @\<select\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- see also: 'selectMultiple'
select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
              [(a, lbl)]  -- ^ (value, label)
           -> (a -> Bool) -- ^ specifies which value is initially selected. Must match *exactly one* element in the list of choices
           -> Form m input error [XMLGenT x (XMLType x)] () a
select = C.select

-- | create @\<select multiple=\"multiple\"\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- This creates a @\<select\>@ element which allows more than one item to be selected.
selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                  [(a, lbl)]  -- ^ (value, label)
               -> (a -> Bool) -- ^ specifies which values are initially selected. Can match 0 or more elements.
               -> Form m input error [XMLGenT x (XMLType x)] () [a]
selectMultiple = C.selectMultiple

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
--
-- see also: 'labelText'
label :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsChild x c) =>
         c
      -> Form m input error [XMLGenT x (XMLType x)] () ()
label = C.label

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > labelText "some input field: " ++> inputText ""
--
-- This function is provided as an alternative to 'label' because when
-- the 'OverloadedStrings' extension is enabled, you will get
-- ambiguous type errors when attempting to apply 'label' to a string
-- literal. While the type error can be fixed using an explicit type
-- signature, calling 'labelText' looks nicer.
labelText :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsChild x Text) =>
         Text
      -> Form m input error [XMLGenT x (XMLType x)] () ()
labelText = C.label


-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
errorList :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x error) =>
             Form m input error [XMLGenT x (XMLType x)] () ()
errorList = C.errorList

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- Includes errors from children of the current form.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
childErrorList :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x error) =>
             Form m input error [XMLGenT x (XMLType x)] () ()
childErrorList = C.childErrorList

-- | create a @\<br\>@ tag.
br :: (Monad m, XMLGenerator x, StringType x ~ Text) => Form m input error [XMLGenT x (XMLType x)] () ()
br = C.br

-- | wrap a @\<fieldset class=\"reform\"\>@ around a 'Form'
--
fieldset :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
            Form m input error c proof a
         -> Form m input error [XMLGenT x (XMLType x)] proof a
fieldset = C.fieldset

-- | wrap an @\<ol class=\"reform\"\>@ around a 'Form'
ol :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ol = C.ol

-- | wrap a @\<ul class=\"reform\"\>@ around a 'Form'
ul :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ul = C.ul

-- | wrap a @\<li class=\"reform\"\>@ around a 'Form'
li :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
li = C.li

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text action)) =>
        action                  -- ^ action url
     -> [(Text,Text)]       -- ^ extra hidden fields to add to form
     -> [XMLGenT x (XMLType x)] -- ^ children
     -> [XMLGenT x (XMLType x)]
form = C.form

-- | set the attributes on the top-level elements of 'Form'
setAttrs :: (EmbedAsAttr x attr, XMLGenerator x, StringType x ~ Text, Monad m, Functor m) =>
            Form m input error [XMLGenT x (XMLType x)] proof a
         -> attr
         -> Form m input error [GenXML x] proof a
setAttrs = C.setAttrs
