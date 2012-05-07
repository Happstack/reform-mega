{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Formettes.HSP.Common where

import Text.Formettes.Backend
import Text.Formettes.Core
import Text.Formettes.Generalized as G
import Text.Formettes.Result (FormId, Result(Ok), unitRange)
import HSP

inputText :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputText getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="text" id=i name=i value=a />]

inputPassword :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputPassword getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="password" id=i name=i value=a />]

inputSubmit :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputSubmit getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="submit" id=i name=i value=a />]

inputReset :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
              text
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset lbl = G.inputReset inputField lbl
    where
      inputField i a = [<input type="reset" id=i name=i value=a />]

inputFile :: (Monad m, FormError error, XMLGenerator x, EmbedAsAttr x (Attr String FormId)) => Form m input error [XMLGenT x (XMLType x)] () (Maybe (FileType input))
inputFile = G.inputFile fileView
    where
      fileView i = [<input type="file" name=i id=i />]

inputHidden :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputHidden getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="hidden" id=i name=i value=a />]

inputButton :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsAttr x (Attr String text), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputButton getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [<input type="button" id=i name=i value=a />]


buttonSubmit :: ( Monad m, FormError error, EmbedAsChild x children , EmbedAsAttr x (Attr String String) , EmbedAsAttr x (Attr String FormId) , EmbedAsAttr x (Attr String a)
                ) =>
                (input -> Either error a)
             -> a
             -> children
             -> Form m input error [XMLGenT x (XMLType x)] () a
buttonSubmit getInput text c = G.input getInput inputField text
    where
      inputField i a = [<button type="submit" id=i name=i value=a><% c %></button>]

buttonReset :: ( Monad m, FormError error, EmbedAsChild x children , EmbedAsAttr x (Attr String String) , EmbedAsAttr x (Attr String FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
buttonReset c = G.inputReset inputField Nothing
    where
      inputField i a = [<button type="reset" id=i name=i><% c %></button>]

button :: ( Monad m, FormError error, EmbedAsChild x children , EmbedAsAttr x (Attr String String) , EmbedAsAttr x (Attr String FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
button c = G.inputReset inputField Nothing
    where
      inputField i a = [<button type="button" id=i name=i><% c %></button>]

label :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x c, Monad m) =>
         c
      -> Form m input error [XMLGenT x (XMLType x)] () ()
label c = G.label mkLabel
    where
      mkLabel i = [<label for=i><% c %></label>]

inputCheckbox :: forall x error input m. (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
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

inputCheckboxes :: (Functor m, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes choices =
    G.inputMulti choices mkCheckboxes
    where
      mkCheckboxes nm choices' = concatMap (mkCheckbox nm) choices'
      mkCheckbox nm (i, val, lbl, checked) =
             [ <input type="checkbox" id=i name=nm value=(show val) (if checked then [("checked" := "checked")] else []) />
             , <label for=i><% lbl %></label>
             ]

inputRadio :: (Functor m, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
              (a -> Bool) -- ^ isDefault
           -> [(a, lbl)]  -- ^ value, label, initially checked
           -> Form m input error [XMLGenT x (XMLType x)] () (Maybe a)
inputRadio isDefault choices =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) =
             [ <input type="radio" id=i name=nm value=(show val) (if checked then [("checked" := "checked")] else []) />
             , <label for=i><% lbl %></label>
             , <br />
             ]

inputMultiSelect :: (Functor m, XMLGenerator x, EmbedAsChild x lbl, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputMultiSelect choices =
    G.inputMulti choices mkSelect
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

errorList :: (Monad m, XMLGenerator x, EmbedAsChild x error) => Form m input error [XMLGenT x (XMLType x)] () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = []
      mkErrors errs = [<ul class="formettes-error-list"><% mapM mkError errs %></ul>]
      mkError e     = <li><% e %></li>
