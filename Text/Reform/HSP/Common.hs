{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Reform.HSP.Common where

import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Generalized as G
import Text.Reform.Result (FormId, Result(Ok), unitRange)
import HSP

instance (EmbedAsAttr m (Attr String String)) => (EmbedAsAttr m (Attr String FormId)) where
    asAttr (n := v) = asAttr (n := show v)

inputText :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputText getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="text" id=i name=i value=a />]

inputPassword :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputPassword getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="password" id=i name=i value=a />]

inputSubmit :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () (Maybe text)
inputSubmit getInput initialValue = G.inputMaybe getInput inputField initialValue
    where
      inputField i a = [<input type="submit" id=i name=i value=a />]

inputReset :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
              text
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset lbl = G.inputNoData inputField lbl
    where
      inputField i a = [<input type="reset" id=i name=i value=a />]

inputHidden :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputHidden getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="hidden" id=i name=i value=a />]

inputButton :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
             text
          -> Form m input error [XMLGenT x (XMLType x)] () ()
inputButton label = G.inputNoData inputField label
    where
      inputField i a = [<input type="button" id=i name=i value=a />]

textarea :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x text) =>
            (input -> Either error text)
         -> Int    -- ^ cols
         -> Int    -- ^ rows
         -> text   -- ^ initial text
         -> Form m input error [XMLGenT x (XMLType x)] () text
textarea getInput cols rows initialValue = G.input getInput textareaView initialValue
    where
      textareaView i txt = [<textarea rows=rows cols=cols id=i name=i><% txt %></textarea>]

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be "" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
             Form m input error [XMLGenT x (XMLType x)] () (FileType input)
inputFile = G.inputFile fileView
    where
      fileView i = [<input type="file" name=i id=i />]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit :: ( Monad m, FormError error, XMLGenerator x, EmbedAsChild x children , EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text)) =>
                (input -> Either error text)
             -> text
             -> children
             -> Form m input error [XMLGenT x (XMLType x)] () (Maybe text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField text
    where
      inputField i a = [<button type="submit" id=i name=i value=a><% c %></button>]

buttonReset :: ( Monad m, FormError error, XMLGenerator x, EmbedAsChild x children , EmbedAsAttr x (Attr String FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
buttonReset c = G.inputNoData inputField Nothing
    where
      inputField i a = [<button type="reset" id=i name=i><% c %></button>]

button :: ( Monad m, FormError error, XMLGenerator x, EmbedAsChild x children , EmbedAsAttr x (Attr String FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
button c = G.inputNoData inputField Nothing
    where
      inputField i a = [<button type="button" id=i name=i><% c %></button>]

label :: (Monad m, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x c) =>
         c
      -> Form m input error [XMLGenT x (XMLType x)] () ()
label c = G.label mkLabel
    where
      mkLabel i = [<label for=i><% c %></label>]

-- FIXME: should this use inputMaybe?
inputCheckbox :: forall x error input m. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) =>
                   Bool  -- ^ initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () Bool
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
          return ( View $ const $ [<input type="checkbox" id=i name=i value=i (if checked then [("checked" := "checked")] else []) />]
                 , return $ Ok (Proved { proofs   = ()
                                       , pos      = unitRange i
                                       , unProved = if checked then True else False
                                       })
                 )

inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId)) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
                -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes choices isChecked =
    G.inputMulti choices mkCheckboxes isChecked
    where
      mkCheckboxes nm choices' = concatMap (mkCheckbox nm) choices'
      mkCheckbox nm (i, val, lbl, checked) =
             [ <input type="checkbox" id=i name=nm value=(show val) (if checked then [("checked" := "checked")] else []) />
             , <label for=i><% lbl %></label>
             ]

inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId)) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error [XMLGenT x (XMLType x)] () a
inputRadio choices isDefault =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
             [ <input type="radio" id=i name=nm value=(show val) (if checked then [("checked" := "checked")] else []) />
             , <label for=i><% lbl %></label>
             , <br />
             ]

select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId)) =>
              [(a, lbl)]  -- ^ value, label
           -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
           -> Form m input error [XMLGenT x (XMLType x)] () a
select choices isDefault  =
    G.inputChoice isDefault choices mkSelect
    where
      mkSelect nm choices' =
          [<select name=nm>
            <% mapM mkOption choices' %>
           </select>
          ]

      mkOption (_, val, lbl, selected) =
          <option value=val (if selected then ["selected" := "selected"] else [])>
           <% lbl %>
          </option>

selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId)) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
               -> (a -> Bool)  -- ^ isSelected initially
               -> Form m input error [XMLGenT x (XMLType x)] () [a]
selectMultiple choices isSelected =
    G.inputMulti choices mkSelect isSelected
    where
      mkSelect nm choices' =
          [<select name=nm multiple="multiple">
            <% mapM mkOption choices' %>
           </select>
          ]
      mkOption (_, val, lbl, selected) =
          <option value=val (if selected then ["selected" := "selected"] else [])>
           <% lbl %>
          </option>
{-
inputMultiSelectOptGroup :: (Functor m, XMLGenerator x, EmbedAsChild x groupLbl, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(groupLbl, [(a, lbl, Bool)])]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
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

errorList :: (Monad m, XMLGenerator x, EmbedAsChild x error) =>
             Form m input error [XMLGenT x (XMLType x)] () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = []
      mkErrors errs = [<ul class="reform-error-list"><% mapM mkError errs %></ul>]
      mkError e     = <li><% e %></li>

br :: (Monad m, XMLGenerator x) => Form m input error [XMLGenT x (XMLType x)] () ()
br = view [<br />]

fieldset :: (Monad m, Functor m, XMLGenerator x, EmbedAsChild x c) =>
            Form m input error c proof a
         -> Form m input error [XMLGenT x (XMLType x)] proof a
fieldset frm = mapView (\xml -> [<fieldset class="reform"><% xml %></fieldset>]) frm

ol :: (Monad m, Functor m, XMLGenerator x, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ol frm = mapView (\xml -> [<ol class="reform"><% xml %></ol>]) frm

ul :: (Monad m, Functor m, XMLGenerator x, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ul frm = mapView (\xml -> [<ul class="reform"><% xml %></ul>]) frm

li :: (Monad m, Functor m, XMLGenerator x, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
li frm = mapView (\xml -> [<li class="reform"><% xml %></li>]) frm

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (XMLGenerator x, EmbedAsAttr x (Attr String action)) =>
        action                  -- ^ action url
     -> [(String,String)]       -- ^ hidden fields to add to form
     -> [XMLGenT x (XMLType x)] -- ^ childern
     -> [XMLGenT x (XMLType x)]
form action hidden children
    = [ <form action=action method="POST" enctype="multipart/form-data">
         <% mapM mkHidden hidden %>
         <% children %>
        </form>
      ]
    where
      mkHidden (name, value) =
          <input type="hidden" name=name value=value />

