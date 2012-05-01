{-# LANGUAGE ViewPatterns #-}
module Text.Formettes.Generalized where

import Text.Formettes.Backend
import Text.Formettes.Core
import Text.Formettes.Result

input :: (Monad m, FormError error) =>
         (input -> Either error a)
      -> (FormId -> Value a -> a -> v)
      -> a
      -> Form m input error v () a
input fromInput toView initialValue =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ toView i Default initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = initialValue
                                                 }))
                (Found (fromInput -> (Right str))) ->
                    return ( View $ const $ toView i (Found str) initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = str
                                                 }))
                (Found (fromInput -> (Left error))) ->
                    return ( View $ const $ toView i Missing initialValue
                           , return $ Error [(unitRange i, error)]
                           )
                _ ->
                    return ( View $ const $ toView i Missing initialValue
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )

label :: Monad m =>
         (FormId -> view)
      -> Form m input error view () ()
label f = Form $ do
    id' <- getFormId
    return (View (const $ f id'), return (Ok $ Proved { proofs   = ()
                                                      , pos      = unitRange id'
                                                      , unProved = ()
                                                      }))

errors :: Monad m =>
          ([error] -> view)
       -> Form m input error view () ()
errors f = Form $ do
    range <- getFormRange
    return (View (f . retainErrors range), return (Ok $ Proved { proofs   = ()
                                                               , pos      = range
                                                               , unProved = ()
                                                               }))

childErrors :: Monad m =>
               ([error] -> view)
            -> Form m input error view () ()
childErrors f = Form $ do
    range <- getFormRange
    return (View (f . retainChildErrors range), return (Ok $ Proved { proofs   = ()
                                                               , pos      = range
                                                               , unProved = ()
                                                               }))
