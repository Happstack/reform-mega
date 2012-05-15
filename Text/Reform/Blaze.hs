{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Text.Reform.Blaze where

import Data.Monoid (mempty)
import Text.Reform.Backend
import Text.Reform.Core
import qualified Text.Reform.Generalized as G
import Text.Reform.Result
import Text.Blaze ((!), Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

instance (H.ToValue FormId) where
    toValue = H.toValue . show

-- | Create an @\<input type=\"text\"\>@ element
inputText :: (FormError error, ErrorInputType error ~ input, FormInput input, Monad m) => String -> Form m input error Html () String
inputText initialValue = G.input getInputString inputField initialValue
    where
      inputField i v = H.input ! A.id (H.toValue i) ! A.name (H.toValue i) ! A.value (H.toValue v)

-- | create a @\<label\>@ element.
--
-- Use this with <++ or ++> to ensure that the @for@ attribute references the correct @id@.
--
-- > label "some input field: " ++> inputText ""
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

-- | create a @\<ul\>@ which contains all the errors related to the 'Form'.
--
-- The @<\ul\>@ will have the attribute @class=\"reform-error-list\"@.
errorList :: (Monad m, H.ToHtml error) => Form m input error Html () ()
errorList = G.errors mkErrors
    where
      mkErrors []   = mempty
      mkErrors errs = H.ul ! A.class_ (H.toValue "reform-error-list") $ mapM_ mkError errs
      mkError e     = H.li (H.toHtml e)
