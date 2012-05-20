{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeFamilies #-}
module Text.Reform.Blaze.Common where

import Data.Monoid (mconcat, mempty, (<>))
import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Generalized as G
import Text.Reform.Result (FormId, Result(Ok), unitRange)
import Text.Blaze.Html (Html, (!), toValue)
import qualified Text.Blaze.Html5  as H
import Text.Blaze.Html5.Attributes (type_, name, value)
import qualified Text.Blaze.Html5.Attributes as A

instance H.ToValue FormId where
    toValue fid = toValue (show fid)

inputText :: (Monad m, FormError error, H.ToValue text) =>
             (input -> Either error text)
          -> text
          -> Form m input error Html () text
inputText getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = H.input ! type_ "text" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)

inputPassword :: (Monad m, FormError error, H.ToValue text) =>
             (input -> Either error text)
          -> text
          -> Form m input error Html () text
inputPassword getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = H.input ! type_ "password" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)

inputSubmit :: (Monad m, FormError error, H.ToValue text) =>
             (input -> Either error text)
          -> text
          -> Form m input error Html () (Maybe text)
inputSubmit getInput initialValue = G.inputMaybe getInput inputField initialValue
    where
      inputField i a = H.input ! type_ "submit" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)

inputReset :: (Monad m, FormError error, H.ToValue text) =>
              text
           -> Form m input error Html () ()
inputReset lbl = G.inputNoData inputField lbl
    where
      inputField i a = H.input ! type_ "submit" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)

inputHidden :: (Monad m, FormError error, H.ToValue text) =>
             (input -> Either error text)
          -> text
          -> Form m input error Html () text
inputHidden getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = H.input ! type_ "hidden" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)

inputButton :: (Monad m, FormError error, H.ToValue text) =>
             text
          -> Form m input error Html () ()
inputButton label = G.inputNoData inputField label
    where
      inputField i a = H.input ! type_ "button" ! A.id (toValue i) ! name (toValue i) ! value (toValue a)


textarea :: (Monad m, FormError error, H.ToMarkup text) =>
            (input -> Either error text)
         -> Int    -- ^ cols
         -> Int    -- ^ rows
         -> text   -- ^ initial text
         -> Form m input error Html () text
textarea getInput cols rows initialValue = G.input getInput textareaView initialValue
    where
      textareaView i txt =
          H.textarea ! A.rows (toValue rows)
                     ! A.cols (toValue cols)
                     ! A.id   (toValue i)
                     ! A.name (toValue i) $
               H.toHtml txt


-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be \"\" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input) =>
             Form m input error Html () (FileType input)
inputFile = G.inputFile fileView
    where
      fileView i = H.input ! type_ "file" ! A.id (toValue i) ! name (toValue i)


-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit :: (Monad m, FormError error, H.ToValue text, H.ToMarkup children) =>
                (input -> Either error text)
             -> text
             -> children
             -> Form m input error Html () (Maybe text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField text
    where
      inputField i a = H.button ! type_ "submit" ! A.id (toValue i) ! name (toValue i) ! value (toValue a) $ H.toHtml c

-- | create a  @\<button type=\"reset\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
buttonReset :: (Monad m, FormError error, H.ToMarkup children) =>
               children
             -> Form m input error Html () ()
buttonReset c = G.inputNoData inputField Nothing
    where
      inputField i a = H.button ! type_ "reset" ! A.id (toValue i) ! name (toValue i) $ H.toHtml c

-- | create a  @\<button type=\"button\"\>\<\/button\>@ element
--
-- This element does not add any data to the form data set.
button :: (Monad m, FormError error, H.ToMarkup children) =>
          children
       -> Form m input error Html () ()
button c = G.inputNoData inputField Nothing
    where
      inputField i a = H.button ! type_ "button" ! A.id (toValue i) ! name (toValue i) $ H.toHtml c


-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
label :: (Monad m, H.ToMarkup children) =>
         children
      -> Form m input error Html () ()
label c = G.label mkLabel
    where
      mkLabel i = H.label ! A.for (toValue i) $ H.toHtml c



-- | Create a single @\<input type=\"checkbox\"\>@ element
--
-- returns a 'Bool' indicating if it was checked or not.
--
-- see also 'inputCheckboxes'
-- FIXME: Should this built on something in Generalized?
inputCheckbox :: forall x error input m. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
                   Bool  -- ^ initially checked
                -> Form m input error Html () Bool
inputCheckbox initiallyChecked =
    Form $
      do i <- getFormId
         v <- getFormInput' i
         case v of
           Default   -> mkCheckbox i initiallyChecked
           Missing   -> mkCheckbox i False -- checkboxes only appear in the submitted data when checked
           (Found input) ->
               case getInputString input of
                 (Right _) -> mkCheckbox i True
                 (Left  (e :: error) ) -> mkCheckbox i False
    where
      mkCheckbox i checked =
          let checkbox  = H.input ! type_ "checkbox" ! A.id (toValue i) ! name (toValue i) ! value (toValue i)
              checkbox' = if checked then checkbox ! A.checked "checked" else checkbox
          in
          return ( View $ const $  checkbox'
                 , return $ Ok (Proved { proofs   = ()
                                       , pos      = unitRange i
                                       , unProved = if checked then True else False
                                       })
                 )

-- | Create a group of @\<input type=\"checkbox\"\>@ elements
--
inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, H.ToMarkup lbl) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
                -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
                -> Form m input error Html () [a]
inputCheckboxes choices isChecked =
    G.inputMulti choices mkCheckboxes isChecked
    where
      mkCheckboxes nm choices' = mconcat $ concatMap (mkCheckbox nm) choices'
      mkCheckbox nm (i, val, lbl, checked) =
          [ ((if checked then (! A.checked "checked") else id) $
                     H.input ! type_ "checkbox" ! A.id (toValue i) ! name (toValue nm) ! value (toValue val))
                  ,  H.label ! A.for (toValue i) $ H.toHtml lbl
                  ]

-- | Create a group of @\<input type=\"radio\"\>@ elements
inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, H.ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error Html () a
inputRadio choices isDefault =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = mconcat $ concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
          [ ((if checked then (! A.checked "checked") else id) $
             H.input ! type_ "radio" ! A.id (toValue i) ! name (toValue nm) ! value (toValue val))
          ,  H.label ! A.for (toValue i) $ H.toHtml lbl
          ,  H.br
          ]

-- | create @\<select\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- see also: 'selectMultiple'
select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, H.ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label
           -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
           -> Form m input error Html () a
select choices isDefault  =
    G.inputChoice isDefault choices mkSelect
    where
      mkSelect nm choices' =
          H.select ! name (toValue nm) $
           mconcat $ map mkOption choices'

      mkOption (_, val, lbl, selected) =
          (if selected then (! A.selected "selected") else id)
             H.option ! value (toValue val) $ H.toHtml lbl

-- | create @\<select multiple=\"multiple\"\>\<\/select\>@ element plus its @\<option\>\<\/option\>@ children.
--
-- This creates a @\<select\>@ element which allows more than one item to be selected.
selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, H.ToMarkup lbl) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
               -> (a -> Bool)  -- ^ isSelected initially
               -> Form m input error Html () [a]
selectMultiple choices isSelected =
    G.inputMulti choices mkSelect isSelected
    where
      mkSelect nm choices' =
          H.select ! name (toValue nm) ! A.multiple "multiple" $
           mconcat $ map mkOption choices'

      mkOption (_, val, lbl, selected) =
        (if selected then (! A.selected "selected") else id)
             H.option ! value (toValue val) $ H.toHtml lbl

{-
inputMultiSelectOptGroup :: (Functor m, XMLGenerator x, EmbedAsChild x groupLbl, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(groupLbl, [(a, lbl, Bool)])]  -- ^ value, label, initially checked
                -> Form m input error Html () [a]
inputMultiSelectOptGroup choices =
    G.inputMulti choices mkSelect
    where
      mkSelect nm choices' =
          [<select name=nm multiple="multiple">
            <% mapM mkOptGroup choices' %>
           </select>
          ]
      mkOptGroup (grpLabel, options) =
          <optgroup label=grpLabel>
           <% mapM mkOption options %>
          </optgroup>
      mkOption (_, val, lbl, selected) =
          <option value=val (if selected then ["selected" := "selected"] else [])>
           <% lbl %>
          </option>
-}

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
errorList :: (Monad m, H.ToMarkup error) =>
             Form m input error Html () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = mempty
      mkErrors errs =
          H.ul ! A.class_ "reform-error-list" $
             mconcat $ map mkError errs
      mkError e     = H.li $ H.toHtml e

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- Includes errors from child forms.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
childErrorList :: (Monad m, H.ToMarkup error) =>
             Form m input error Html () ()
childErrorList = G.childErrors mkErrors
    where
      mkErrors []   = mempty
      mkErrors errs =
          H.ul ! A.class_ "reform-error-list" $
             mconcat $ map mkError errs
      mkError e     = H.li $ H.toHtml e


-- | create a @\<br\>@ tag.
br :: (Monad m) => Form m input error Html () ()
br = view H.br

-- | wrap a @\<fieldset class=\"reform\"\>@ around a 'Form'
--
fieldset :: (Monad m, Functor m) =>
            Form m input error Html proof a
         -> Form m input error Html proof a
fieldset frm = mapView (H.fieldset ! A.class_ "reform") frm

-- | wrap an @\<ol class=\"reform\"\>@ around a 'Form'
ol :: (Monad m, Functor m) =>
      Form m input error Html proof a
   -> Form m input error Html proof a
ol frm = mapView (H.ol ! A.class_ "reform") frm

-- | wrap a @\<ul class=\"reform\"\>@ around a 'Form'
ul :: (Monad m, Functor m) =>
      Form m input error Html proof a
   -> Form m input error Html proof a
ul frm = mapView (H.ul ! A.class_ "reform") frm

-- | wrap a @\<li class=\"reform\"\>@ around a 'Form'
li :: (Monad m, Functor m) =>
      Form m input error Html proof a
   -> Form m input error Html proof a
li frm = mapView (H.li ! A.class_ "reform") frm

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (H.ToValue action) =>
        action                  -- ^ action url
     -> [(String, String)]       -- ^ hidden fields to add to form
     -> Html -- ^ children
     -> Html
form action hidden children =
    H.form ! A.action (toValue action) ! A.method "POST" ! A.enctype "multipart/form-data" $
         ((mconcat $ map mkHidden hidden) <> children)
      where
        mkHidden (nm, val) =
            H.input ! type_ "hidden" ! name (toValue nm) ! value (toValue val)
