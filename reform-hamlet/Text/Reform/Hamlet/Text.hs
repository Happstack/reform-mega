{-# LANGUAGE RankNTypes, TypeFamilies #-}
{- |
This module provides functions creating Reform using Hamlet markup.

This module assumes that you wish for text based controls such as 'inputText' and 'textarea' to using 'Text' values. If you prefer 'String' see "Text.Reform.Hamlet.String".
-}
module Text.Reform.Hamlet.Text
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
    ) where

import Data.Text (empty)
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Text.Blaze (ToMarkup(..))
import Text.Reform
import qualified Text.Reform.Hamlet.Common as C
import Text.Hamlet (HtmlUrl)

-- | Create an @\<input type=\"text\"\>@ element
inputText :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               T.Text -- ^ initial value
            -> Form m input error (HtmlUrl url) () T.Text
inputText initialValue = C.inputText getInputText initialValue

-- | Create an @\<input type=\"password\"\>@ element
inputPassword :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
                 Form m input error (HtmlUrl url) () T.Text
inputPassword = C.inputPassword getInputText empty

-- | Create an @\<input type=\"submit\"\>@ element
--
-- returns:
--
--   [@Just@ /value/] if this button was used to submit the form.
--
--   [@Nothing@]    if this button was not used to submit the form.
inputSubmit :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               T.Text -- ^ @value@ attribute. Used for button label, and value if button is submitted.
            -> Form m input error (HtmlUrl url) () (Maybe T.Text)
inputSubmit initialValue = C.inputSubmit getInputText initialValue

-- | Create an @\<input type=\"reset\"\>@ element
--
-- This element does not add any data to the form data set.
inputReset :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
              T.Text -- ^ value attribute. Used only to label the button.
           -> Form m input error (HtmlUrl url) () ()
inputReset = C.inputReset

-- | Create an @\<input type=\"hidden\"\>@ element
inputHidden :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               T.Text -- ^ value to store in the hidden element
            -> Form m input error (HtmlUrl url) () T.Text
inputHidden initialValue = C.inputHidden getInputText initialValue

-- | Create an @\<input type=\"button\"\>@ element
--
-- The element is a push button with a text label. The button does nothing by default, but actions can be added using javascript. This element does not add any data to the form data set.
--
-- see also: 'C.button'
inputButton :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
               Text -- ^ value attribute. Used to label the button.
            -> Form m input error (HtmlUrl url) () ()
inputButton label = C.inputButton label

-- | Create a \<textarea\>\<\/textarea\> element
textarea :: (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
            Int    -- ^ cols
         -> Int    -- ^ rows
         -> T.Text -- ^ initial contents
         -> Form m input error (HtmlUrl url) () T.Text
textarea rows cols initialValue = C.textarea getInputText rows cols initialValue

-- | create a  @\<button type=\"submit\"\>\<\/button\>@ element
buttonSubmit :: ( Monad m, FormError error, FormInput input, ErrorInputType error ~ input, ToMarkup children) =>
                T.Text -- ^ value attribute. Returned if this button submits the form.
             -> children -- ^ children to embed in the \<button\>
             -> Form m input error (HtmlUrl url) () (Maybe T.Text)
buttonSubmit = C.buttonSubmit getInputText

--------------------------------------------------------------------------------
-- re-exports from .Common. In theory we could just put the docs in .Common,
-- but, currently HSX strips them out.

-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
inputCheckbox :: forall x error input m url. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
                   Bool  -- ^ initially checked
                -> Form m input error (HtmlUrl url) () Bool
inputCheckbox = C.inputCheckbox

-- | Create a group of @\<input type=\"checkbox\"\>@ elements
--
inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
                   [(a, lbl)]  -- ^ (value, label)
                -> (a -> Bool) -- ^ function which marks if a value should be checked (aka, selected) initially or not. Can match zero or more elements.
                -> Form m input error (HtmlUrl url) () [a]
inputCheckboxes = C.inputCheckboxes

-- | Create a group of @\<input type=\"radio\"\>@ elements
inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ (value, label)
           -> (a -> Bool) -- ^ predicate which returns @True@ if @a@ should be initially checked. Must match exactly one value in the previous argument.
           -> Form m input error (HtmlUrl url) () a
inputRadio = C.inputRadio

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be \"\" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input) =>
             Form m input error (HtmlUrl url) () (FileType input)
inputFile = C.inputFile

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset :: (Monad m, FormError error, ToMarkup children) =>
               children -- ^ children of the @<\/button\>@ element
             -> Form m input error (HtmlUrl url) () ()
buttonReset = C.buttonReset

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, ToMarkup children) =>
          children -- ^ children to embed in the \<button\>
       -> Form m input error (HtmlUrl url) () ()
button = C.button

-- | create @\<select\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- see also: 'selectMultiple'
select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ (value, label)
           -> (a -> Bool) -- ^ specifies which value is initially selected. Must match *exactly one* element in the list of choices
           -> Form m input error (HtmlUrl url) () a
select = C.select

-- | create @\<select multiple=\"multiple\"\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- This creates a @\<select\>@ element which allows more than one item to be selected.
selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
                  [(a, lbl)]  -- ^ (value, label)
               -> (a -> Bool) -- ^ specifies which values are initially selected. Can match 0 or more elements.
               -> Form m input error (HtmlUrl url) () [a]
selectMultiple = C.selectMultiple

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
--
-- see also: 'labelText'
label :: (Monad m, ToMarkup c) =>
         c
      -> Form m input error (HtmlUrl url) () ()
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
labelText :: Monad m =>
         Text
      -> Form m input error (HtmlUrl url) () ()
labelText = C.label


-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
errorList :: (Monad m, ToMarkup error) =>
             Form m input error (HtmlUrl url) () ()
errorList = C.errorList

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- Includes errors from children of the current form.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
childErrorList :: (Monad m, ToMarkup error) =>
             Form m input error (HtmlUrl url) () ()
childErrorList = C.childErrorList

-- | create a @\<br\>@ tag.
br :: (Monad m) => Form m input error (HtmlUrl url) () ()
br = C.br

-- | wrap a @\<fieldset class=\"reform\"\>@ around a 'Form'
--
fieldset :: (Monad m, Functor m, ToMarkup c) =>
            Form m input error c proof a
         -> Form m input error (HtmlUrl url) proof a
fieldset = C.fieldset

-- | wrap an @\<ol class=\"reform\"\>@ around a 'Form'
ol :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
ol = C.ol

-- | wrap a @\<ul class=\"reform\"\>@ around a 'Form'
ul :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
ul = C.ul

-- | wrap a @\<li class=\"reform\"\>@ around a 'Form'
li :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
li = C.li

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: ToMarkup action =>
        action                  -- ^ action url
     -> [(Text,Text)]       -- ^ extra hidden fields to add to form
     -> (HtmlUrl url) -- ^ children
     -> (HtmlUrl url)
form = C.form
