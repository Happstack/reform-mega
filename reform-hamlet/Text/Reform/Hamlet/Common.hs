{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TypeFamilies #-}

module Text.Reform.Hamlet.Common where

import Data.Text.Lazy (Text, pack)
import qualified Data.Text as T
import Text.Blaze (ToMarkup(..))
import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Generalized as G
import Text.Reform.Result (FormId, Result(Ok), unitRange)
import Text.Hamlet (hamlet, HtmlUrl)

instance ToMarkup FormId where
    toMarkup fid = toMarkup (show fid)

inputText :: (FormError error, Monad m, ToMarkup text) => (input -> Either error text) -> text -> Form m input error (HtmlUrl url) () text
inputText getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hamlet|<input type="text" id=#{i} name=#{i} value=#{a}>|]

inputPassword :: (Monad m, FormError error, ToMarkup text) =>
             (input -> Either error text)
          -> text
          -> Form m input error (HtmlUrl url) () text
inputPassword getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hamlet|<input type="password" id=#{i} name=#{i} value=#{a}>|]

inputSubmit :: (Monad m, FormError error, ToMarkup text) =>
             (input -> Either error text)
          -> text
          -> Form m input error (HtmlUrl url) () (Maybe text)
inputSubmit getInput initialValue = G.inputMaybe getInput inputField initialValue
    where
      inputField i a = [hamlet|<input type="submit" id=#{i} name=#{i} value=#{a}>|]

inputReset :: (Monad m, FormError error, ToMarkup text) =>
              text
           -> Form m input error (HtmlUrl url) () ()
inputReset lbl = G.inputNoData inputField lbl
    where
      inputField i a = [hamlet|<input type="reset" id=#{i} name=#{i} value=#{a}>|]

inputHidden :: (Monad m, FormError error, ToMarkup text) =>
             (input -> Either error text)
          -> text
          -> Form m input error (HtmlUrl url) () text
inputHidden getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hamlet|<input type="hidden" id=#{i} name=#{i} value=#{a}>|]

inputButton :: (Monad m, FormError error, ToMarkup text) =>
             text
          -> Form m input error (HtmlUrl url) () ()

inputButton label = G.inputNoData inputField label
    where
      inputField i a = [hamlet|<input type="button" id=#{i} name=#{i} value=#{a}>|]

textarea :: (Monad m, FormError error, ToMarkup text) =>
            (input -> Either error text)
         -> Int    -- ^ cols
         -> Int    -- ^ rows
         -> text   -- ^ initial text
         -> Form m input error (HtmlUrl url) () text
textarea getInput cols rows initialValue = G.input getInput textareaView initialValue
    where
      textareaView i txt = [hamlet|<textarea rows=#{rows} cols=#{cols} id=#{i} name=#{i}>#{txt}|]

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be "" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input) =>
             Form m input error (HtmlUrl url) () (FileType input)
inputFile = G.inputFile fileView
    where
      fileView i = [hamlet|<input type="file" name=#{i} id=#{i}>|]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit :: (Monad m, FormError error, ToMarkup text, ToMarkup children) =>
                (input -> Either error text)
             -> text
             -> children
             -> Form m input error (HtmlUrl url) () (Maybe text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField text
    where
      inputField i a = [hamlet|<button type="submit" id=#{i} name=#{i} value=#{a}>#{c}|]

buttonReset :: (Monad m, FormError error, ToMarkup children) =>
               children
             -> Form m input error (HtmlUrl url) () ()
buttonReset c = G.inputNoData inputField Nothing
    where
      inputField i a = [hamlet|<button type="reset" id=#{i} name=#{i}>#{c}|]

button :: (Monad m, FormError error, ToMarkup children) =>
               children
             -> Form m input error (HtmlUrl url) () ()
button c = G.inputNoData inputField Nothing
    where
      inputField i a = [hamlet|<button type="button" id=#{i} name=#{i}>#{c}|]

label :: (Monad m, ToMarkup c) =>
         c
      -> Form m input error (HtmlUrl url) () ()
label c = G.label mkLabel
    where
      mkLabel i = [hamlet|<label for=#{i}>#{c}|]

-- FIXME: should this use inputMaybe?
inputCheckbox :: forall x error input m url. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
                   Bool  -- ^ initially checked
                -> Form m input error (HtmlUrl url) () Bool
inputCheckbox initiallyChecked =
    Form $
      do i <- getFormId
         v <- getFormInput' i
         case v of
           Default   -> mkCheckbox i initiallyChecked
           Missing   -> mkCheckbox i False -- checkboxes only appear in the submitted data when checked
           (Found input) ->
               case getInputText input of
                 (Right _) -> mkCheckbox i True
                 (Left  (e :: error) ) -> mkCheckbox i False
    where
      mkCheckbox i checked =
          return ( View $ const $ [hamlet|
$if checked
  <input type="checkbox" id=#{i} name=#{i} value=#{i} checked="checked">
$else
  <input type="checkbox" id=#{i} name=#{i} value=#{i}>
|]
                 , return $ Ok (Proved { proofs   = ()
                                       , pos      = unitRange i
                                       , unProved = if checked then True else False
                                       })
                 )

inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
                -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
                -> Form m input error (HtmlUrl url) () [a]
inputCheckboxes choices isChecked =
    G.inputMulti choices mkCheckboxes isChecked
    where
      mkCheckboxes nm choices' = [hamlet|
$forall (i, val, lbl, checked) <- choices'
  $if checked
    <input type="checkbox" id=#{i} name=#{nm} value=#{show val} checked="checked">
  $else
    <input type="checkbox" id=#{i} name=#{nm} value=#{show val}>
  <label for=#{i}>#{lbl}
|]

inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error (HtmlUrl url) () a
inputRadio choices isDefault =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = [hamlet|
$forall (i, val, lbl, checked) <- choices'
  $if checked
    <input type="radio" id=#{i} name=#{nm} value=#{show val} checked="checked">
  $else
    <input type="radio" id=#{i} name=#{nm} value=#{show val}>
  <label for=#{i}>#{lbl}
  <br>
|]

select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
              [(a, lbl)]  -- ^ value, label
           -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
           -> Form m input error (HtmlUrl url) () a
select choices isDefault  =
    G.inputChoice isDefault choices mkSelect
    where
      mkSelect nm choices' = [hamlet|
<select name=#{nm}>
  $forall (_, val, lbl, selected) <- choices'
    $if selected
      <option value=#{val} selected="selected">#{lbl}
    $else
      <option value=#{val}>#{lbl}
|]

selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, ToMarkup lbl) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
               -> (a -> Bool)  -- ^ isSelected initially
               -> Form m input error (HtmlUrl url) () [a]
selectMultiple choices isSelected =
    G.inputMulti choices mkSelect isSelected
    where
      mkSelect nm choices' = [hamlet|
<select name=#{nm} multiple="multiple">
  $forall (_, val, lbl, selected) <- choices'
    $if selected
      <option value=#{val} selected="selected">#{lbl}
    $else
      <option value=#{val}>#{lbl}
|]

errorList :: (Monad m, ToMarkup error) =>
             Form m input error (HtmlUrl url) () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = [hamlet||]
      mkErrors errs = [hamlet|
<ul .reform-error-list>
  $forall e <- errs
    <li>#{e}
|]

childErrorList :: (Monad m, ToMarkup error) =>
             Form m input error (HtmlUrl url) () ()
childErrorList = G.childErrors mkErrors
    where
      mkErrors []   = [hamlet||]
      mkErrors errs = [hamlet|
<ul .reform-error-list>
  $forall e <- errs
    <li>#{e}
|]

br :: Monad m => Form m input error (HtmlUrl url) () ()
br = view [hamlet|<br>|]

fieldset :: (Monad m, Functor m, ToMarkup c) =>
            Form m input error c proof a
         -> Form m input error (HtmlUrl url) proof a
fieldset frm = mapView (\xml -> [hamlet|<fieldset .reform>#{xml}|]) frm

ol :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
ol frm = mapView (\xml -> [hamlet|<ol .reform>#{xml}|]) frm

ul :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
ul frm = mapView (\xml -> [hamlet|<ul .reform>#{xml}|]) frm

li :: (Monad m, Functor m, ToMarkup c) =>
      Form m input error c proof a
   -> Form m input error (HtmlUrl url) proof a
li frm = mapView (\xml -> [hamlet|<li .reform>#{xml}|]) frm

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: ToMarkup action =>
        action              -- ^ action url
     -> [(Text,Text)]       -- ^ hidden fields to add to form
     -> HtmlUrl url         -- ^ children
     -> HtmlUrl url
form action hidden children
    = [hamlet|
<form action=#{action} method="POST" enctype="multipart/form-data">
  $forall (name, value) <- hidden
    <input type="hidden" name=#{name} value=#{value}>
  ^{children}
|]
