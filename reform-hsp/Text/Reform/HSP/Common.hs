{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns, OverloadedStrings, QuasiQuotes #-}
module Text.Reform.HSP.Common where

import Data.List                (intercalate)
import Data.Monoid              ((<>), mconcat)
import Data.Text.Lazy           (Text, pack)
import qualified Data.Text      as T
import Text.Reform.Backend
import Text.Reform.Core
import Text.Reform.Generalized  as G
import Text.Reform.Result      (FormId, Result(Ok), unitRange)
import Language.Haskell.HSX.QQ (hsx)
import HSP.XMLGenerator
import HSP.XML

instance (EmbedAsAttr m (Attr Text Text)) => (EmbedAsAttr m (Attr Text FormId)) where
    asAttr (n := v) = asAttr (n := (pack $ show v))

inputText :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputText getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hsx| [<input type="text" id=i name=i value=a />] |]

inputPassword :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputPassword getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hsx| [<input type="password" id=i name=i value=a />] |]

inputSubmit :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () (Maybe text)
inputSubmit getInput initialValue = G.inputMaybe getInput inputField initialValue
    where
      inputField i a = [hsx| [<input type="submit" id=i name=i value=a />] |]

inputReset :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
              text
           -> Form m input error [XMLGenT x (XMLType x)] () ()
inputReset lbl = G.inputNoData inputField lbl
    where
      inputField i a = [hsx| [<input type="reset" id=i name=i value=a />] |]

inputHidden :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
             (input -> Either error text)
          -> text
          -> Form m input error [XMLGenT x (XMLType x)] () text
inputHidden getInput initialValue = G.input getInput inputField initialValue
    where
      inputField i a = [hsx| [<input type="hidden" id=i name=i value=a />] |]

inputButton :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
             text
          -> Form m input error [XMLGenT x (XMLType x)] () ()
inputButton label = G.inputNoData inputField label
    where
      inputField i a = [hsx| [<input type="button" id=i name=i value=a />] |]

textarea :: (Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsChild x text) =>
            (input -> Either error text)
         -> Int    -- ^ cols
         -> Int    -- ^ rows
         -> text   -- ^ initial text
         -> Form m input error [XMLGenT x (XMLType x)] () text
textarea getInput cols rows initialValue = G.input getInput textareaView initialValue
    where
      textareaView i txt = [hsx| [<textarea rows=rows cols=cols id=i name=i><% txt %></textarea>] |]

-- | Create an @\<input type=\"file\"\>@ element
--
-- This control may succeed even if the user does not actually select a file to upload. In that case the uploaded name will likely be "" and the file contents will be empty as well.
inputFile :: (Monad m, FormError error, FormInput input, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId)) =>
             Form m input error [XMLGenT x (XMLType x)] () (FileType input)
inputFile = G.inputFile fileView
    where
      fileView i = [hsx| [<input type="file" name=i id=i />] |]

-- | Create a @\<button type=\"submit\"\>@ element
buttonSubmit :: ( Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId), EmbedAsAttr x (Attr Text text)) =>
                (input -> Either error text)
             -> text
             -> children
             -> Form m input error [XMLGenT x (XMLType x)] () (Maybe text)
buttonSubmit getInput text c = G.inputMaybe getInput inputField text
    where
      inputField i a = [hsx| [<button type="submit" id=i name=i value=a><% c %></button>] |]

buttonReset :: ( Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
buttonReset c = G.inputNoData inputField Nothing
    where
      inputField i a = [hsx| [<button type="reset" id=i name=i><% c %></button>] |]

button :: ( Monad m, FormError error, XMLGenerator x, StringType x ~ Text, EmbedAsChild x children , EmbedAsAttr x (Attr Text FormId)
                ) =>
               children
             -> Form m input error [XMLGenT x (XMLType x)] () ()
button c = G.inputNoData inputField Nothing
    where
      inputField i a = [hsx| [<button type="button" id=i name=i><% c %></button>] |]

label :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId), EmbedAsChild x c) =>
         c
      -> Form m input error [XMLGenT x (XMLType x)] () ()
label c = G.label mkLabel
    where
      mkLabel i = [hsx| [<label for=i><% c %></label>] |]

-- FIXME: should this use inputMaybe?
inputCheckbox :: forall x error input m. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input, XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text FormId)) =>
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
               case getInputText input of
                 (Right _) -> mkCheckbox i True
                 (Left  (e :: error) ) -> mkCheckbox i False
    where
      mkCheckbox i checked =
          return ( View $ const $ [hsx| [<input type="checkbox" id=i name=i value=i (if checked then [("checked" := "checked") :: Attr Text Text] else []) />] |]
                 , return $ Ok (Proved { proofs   = ()
                                       , pos      = unitRange i
                                       , unProved = if checked then True else False
                                       })
                 )

inputCheckboxes :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
                -> (a -> Bool) -- ^ function which indicates if a value should be checked initially
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes choices isChecked =
    G.inputMulti choices mkCheckboxes isChecked
    where
      mkCheckboxes nm choices' = concatMap (mkCheckbox nm) choices'
      mkCheckbox nm (i, val, lbl, checked) = [hsx|
             [ <input type="checkbox" id=i name=nm value=(pack $ show val) (if checked then [("checked" := "checked") :: Attr Text Text] else []) />
             , <label for=i><% lbl %></label>
             ] |]

inputRadio :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
              [(a, lbl)]  -- ^ value, label, initially checked
           -> (a -> Bool) -- ^ isDefault
           -> Form m input error [XMLGenT x (XMLType x)] () a
inputRadio choices isDefault =
    G.inputChoice isDefault choices mkRadios
    where
      mkRadios nm choices' = concatMap (mkRadio nm) choices'
      mkRadio nm (i, val, lbl, checked) = [hsx|
             [ <input type="radio" id=i name=nm value=(pack $ show val) (if checked then [("checked" := "checked") :: Attr Text Text] else []) />
             , <label for=i><% lbl %></label>
             , <br />
             ] |]

inputRadioForms :: forall m x error input lbl proof a. (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                   [(Form m input error [XMLGenT x (XMLType x)] proof a, lbl)]  -- ^ value, label, initially checked
                 -> a -- ^ default
                 -> Form m input error [XMLGenT x (XMLType x)] proof a
inputRadioForms choices def =
    inputRadioForms' onclick choices def
    where
      formIdsJS :: [FormId] -> Text
      formIdsJS [] = "[]"
      formIdsJS ids =
          "['" <> (pack $ intercalate "', '" $ map show ids) <> "']"

      onclick :: FormId -> FormId -> [FormId] -> Text
      onclick nm iview iviews = mconcat
                [ "var views = " <> formIdsJS iviews <> ";"
                , "var iview = '" <> (pack $ show iview) <> "';"
                , "for (var i = 0; i < views.length; i++) {"
                , "  if (iview == views[i]) {"
                , "    document.getElementById(iview).style.display='block';"
                , "  } else {"
                , "    document.getElementById(views[i]).style.display='none';"
                , "  }"
                , "}"
                ]

inputRadioForms' :: forall m x error input lbl proof a. (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                    (FormId -> FormId -> [FormId] -> Text)
                 -> [(Form m input error [XMLGenT x (XMLType x)] proof a, lbl)]  -- ^ value, label, initially checked
                 -> a -- ^ default
                 -> Form m input error [XMLGenT x (XMLType x)] proof a
inputRadioForms' onclick choices def =
    G.inputChoiceForms def choices mkRadios
    where
      iviewsExtract :: [(FormId, Int, FormId, [XMLGenT x (XMLType x)], lbl, Bool)] -> [FormId]
      iviewsExtract = map (\(_,_, iv, _, _, _) -> iv)

      mkRadios :: FormId -> [(FormId, Int, FormId, [XMLGenT x (XMLType x)], lbl, Bool)] -> [XMLGenT x (XMLType x)]
      mkRadios nm choices' =
          let iviews = iviewsExtract choices' in
          (concatMap (mkRadio nm iviews) choices')

      mkRadio nm iviews (i, val, iview, view, lbl, checked) = [hsx|
             [ <div>
                <input type="radio" onclick=(onclick nm iview iviews) id=i name=nm value=(pack $ show val) (if checked then [("checked" := "checked") :: Attr Text Text] else []) />
               <label for=i><% lbl %></label>
               <div id=iview (if checked then [] else [("style" := "display:none;") :: Attr Text Text])><% view %></div>
              </div>
             ] |]

select :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
              [(a, lbl)]  -- ^ value, label
           -> (a -> Bool) -- ^ isDefault, must match *exactly one* element in the list of choices
           -> Form m input error [XMLGenT x (XMLType x)] () a
select choices isDefault  =
    G.inputChoice isDefault choices mkSelect
    where
      mkSelect nm choices' = [hsx|
          [<select name=nm>
            <% mapM mkOption choices' %>
           </select>
          ] |]

      mkOption (_, val, lbl, selected) = [hsx|
          <option value=val (if selected then [("selected" := "selected") :: Attr Text Text] else []) >
           <% lbl %>
          </option> |]

selectMultiple :: (Functor m, Monad m, FormError error, ErrorInputType error ~ input, FormInput input, XMLGenerator x, StringType x ~ Text, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId)) =>
                  [(a, lbl)]  -- ^ value, label, initially checked
               -> (a -> Bool)  -- ^ isSelected initially
               -> Form m input error [XMLGenT x (XMLType x)] () [a]
selectMultiple choices isSelected =
    G.inputMulti choices mkSelect isSelected
    where
      mkSelect nm choices' = [hsx|
          [<select name=nm multiple="multiple">
            <% mapM mkOption choices' %>
           </select>
          ]  |]
      mkOption (_, val, lbl, selected) = [hsx|
          <option value=val (if selected then [("selected" := "selected") :: Attr Text Text] else [])>
           <% lbl %>
          </option> |]
{-
inputMultiSelectOptGroup :: (Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x groupLbl, EmbedAsChild x lbl, EmbedAsAttr x (Attr Text FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
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

errorList :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x error) =>
             Form m input error [XMLGenT x (XMLType x)] () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = []
      mkErrors errs = [hsx| [<ul class="reform-error-list"><% mapM mkError errs %></ul>] |]
      mkError e     = [hsx| <li><% e %></li> |]

childErrorList :: (Monad m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x error) =>
             Form m input error [XMLGenT x (XMLType x)] () ()
childErrorList = G.childErrors mkErrors
    where
      mkErrors []   = []
      mkErrors errs = [hsx| [<ul class="reform-error-list"><% mapM mkError errs %></ul>] |]
      mkError e     = [hsx| <li><% e %></li> |]


br :: (Monad m, XMLGenerator x, StringType x ~ Text) => Form m input error [XMLGenT x (XMLType x)] () ()
br = view [hsx| [<br />] |]

fieldset :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
            Form m input error c proof a
         -> Form m input error [XMLGenT x (XMLType x)] proof a
fieldset frm = mapView (\xml -> [hsx| [<fieldset class="reform"><% xml %></fieldset>] |]) frm

ol :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ol frm = mapView (\xml -> [hsx| [<ol class="reform"><% xml %></ol>] |]) frm

ul :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
ul frm = mapView (\xml -> [hsx| [<ul class="reform"><% xml %></ul>] |]) frm

li :: (Monad m, Functor m, XMLGenerator x, StringType x ~ Text, EmbedAsChild x c) =>
      Form m input error c proof a
   -> Form m input error [XMLGenT x (XMLType x)] proof a
li frm = mapView (\xml -> [hsx| [<li class="reform"><% xml %></li>] |]) frm

-- | create @\<form action=action method=\"POST\" enctype=\"multipart/form-data\"\>@
form :: (XMLGenerator x, StringType x ~ Text, EmbedAsAttr x (Attr Text action)) =>
        action                  -- ^ action url
     -> [(Text,Text)]       -- ^ hidden fields to add to form
     -> [XMLGenT x (XMLType x)] -- ^ children
     -> [XMLGenT x (XMLType x)]
form action hidden children
    = [hsx|
      [ <form action=action method="POST" enctype="multipart/form-data">
         <% mapM mkHidden hidden %>
         <% children %>
        </form>
      ] |]
    where
      mkHidden (name, value) =
          [hsx| <input type="hidden" name=name value=value /> |]

setAttrs :: (EmbedAsAttr x attr, XMLGenerator x, StringType x ~ Text, Monad m, Functor m) =>
            Form m input error [GenXML x] proof a
         -> attr
         -> Form m input error [GenXML x] proof a
setAttrs form attrs = mapView (map (`set` attrs)) form
