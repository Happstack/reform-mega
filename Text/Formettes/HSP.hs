{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeFamilies, UndecidableInstances, ViewPatterns #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Text.Formettes.HSP where

import qualified Data.Traversable as T
import Data.Maybe
import Text.Formettes
import qualified Text.Formettes.Generalized as G
import HSX.XMLGenerator

instance (EmbedAsAttr m (Attr String String)) => (EmbedAsAttr m (Attr String FormId)) where
    asAttr (n := v) = asAttr (n := show v)

-- instance EmbedAsAttr m (Attr String String) => EmbedAsAttr m (Attr String FormId) where
--    asAttr (n := fid) = asAttr (n := (show fid))

label :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x c, Monad m) =>
         c
      -> Form m input error [XMLGenT x (XMLType x)] () ()
label c = G.label mkLabel
    where
      mkLabel i = [<label for=i><% c %></label>]

inputString :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               String
            -> Form m input error [XMLGenT x (XMLType x)] () String
inputString initialValue = G.input getInputString inputField initialValue
    where
      inputField i mv def =
          let s = case mv of
                    (Found s') -> s'
                    _          -> def
          in [<input type="text" id=i name=i value=s />]

-- | inputCheckbox is useful when you want a checkbox that just returns a True/False value

inputCheckbox :: forall x error input m. (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   (Bool)  -- ^ initially checked
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

{-
inputCheckboxes :: (Functor m, XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x lbl, FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes values = inputCheckboxes' $ map mkLbl values
    where
      mkLbl (a, lbl, initiallyChecked) = (a, \x -> x <++ label lbl, initiallyChecked)

inputCheckboxes' :: (Functor m, XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, Form m input error [XMLGenT x (XMLType x)] () (Maybe a) -> Form m input error [XMLGenT x (XMLType x)] () (Maybe a), Bool)]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () [a]
inputCheckboxes' values =
    Form $ do i <- getFormId
              v <- getFormInput'
-}
-- fmap catMaybes $ T.sequenceA $ map inputCheckbox' values
{-
-- FIXME: all the checkboxes should have the same name, but different values
inputCheckbox' :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   (a, Form m input error [XMLGenT x (XMLType x)] () (Maybe a) -> Form m input error [XMLGenT x (XMLType x)] () (Maybe a), Bool)  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () (Maybe a)
inputCheckbox' (a, addlabel, initiallyChecked) =
  addlabel $
    Form $
      do i <- getFormId
         v <- getFormInput' i
         case v of
           Default   -> mkCheckbox i initiallyChecked
           Missing   -> mkCheckbox i False -- checkboxes only appear in the submitted data when checked
           (Found _) -> mkCheckbox i True
    where
      mkCheckbox i checked =
          return ( View $ const $ [<input type="checkbox" id=i name=i value=i (if checked then [("checked" := "checked")] else []) />]
                 , return $ Ok (Proved { proofs   = ()
                                       , pos      = unitRange i
                                       , unProved = if checked then (Just a) else Nothing
                                       }))
-}
{-
inputCheckboxes :: (XMLGenerator x, EmbedAsAttr x (Attr String FormId), EmbedAsChild x lbl, FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]  -- ^ value, label, initially checked
                -> Form m input error [XMLGenT x (XMLType x)] () a
inputCheckboxes values = undefined
--     mapA inputCheckbox values
-}


{-
errors :: (Monad m) => ([error] -> Html) -> Form m input error Html () ()
errors toHtml = G.errors toHtml

childErrors :: Monad m =>
               ([error] -> Html)
            -> Form m input error Html () ()
childErrors toHtml = G.errors toHtml
-}
errorList :: (Monad m, XMLGenerator x, EmbedAsChild x error) => Form m input error [XMLGenT x (XMLType x)] () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = []
      mkErrors errs = [<ul class="formettes-error-list"><% mapM mkError errs %></ul>]
      mkError e     = <li><% e %></li>


