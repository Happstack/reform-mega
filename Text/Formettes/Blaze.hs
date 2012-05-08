{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Text.Formettes.Blaze where

import Data.Monoid (mempty)
import Text.Formettes.Backend
import Text.Formettes.Core
import qualified Text.Formettes.Generalized as G
import Text.Formettes.Result
import Text.Blaze ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

instance (H.ToValue FormId) where
    toValue = H.toValue . show

inputString :: (FormError error, ErrorInputType error ~ input, FormInput input, Monad m) => String -> Form m input error Html () String
inputString initialValue = G.input getInputString inputField initialValue
    where
      inputField i v = H.input ! A.id (H.toValue i) ! A.name (H.toValue i) ! A.value (H.toValue v)

label :: (Monad m) => Html -> Form m input error Html () ()
label c = G.label mkLabel
    where
      mkLabel formId = H.label ! A.for (H.toValue formId) $ c

errors :: (Monad m) => ([error] -> Html) -> Form m input error Html () ()
errors toHtml = G.errors toHtml

childErrors :: Monad m =>
               ([error] -> Html)
            -> Form m input error Html () ()
childErrors toHtml = G.errors toHtml

errorList :: (Monad m, H.ToHtml error) => Form m input error Html () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = mempty
      mkErrors errs = H.ul ! A.class_ (H.toValue "formettes-error-list") $ mapM_ mkError errs
      mkError e     = H.li (H.toHtml e)

{-

inputString :: (FormError error, FormInput input, Monad m) => String -> Form m input error Html () String
inputString initialValue =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ H.input ! A.id (H.toValue i) ! A.name (H.toValue i) ! A.value (H.toValue initialValue)
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = initialValue
                                                 }))
                (Found (getInputString -> (Just str))) ->
                    return ( View $ const $ H.input ! A.id (H.toValue i) ! A.name (H.toValue i) ! A.value (H.toValue str)
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = str
                                                 }))
                _ ->
                    return ( View $ const $ H.input ! A.type_ (H.toValue "text") ! A.id (H.toValue i) ! A.name (H.toValue i) ! A.value (H.toValue initialValue)
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )


-}