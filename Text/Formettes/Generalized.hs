{-# LANGUAGE TypeFamilies, ViewPatterns #-}
module Text.Formettes.Generalized where

import Control.Applicative    ((<$>))
import qualified Data.IntMap  as I
import Data.Maybe             (mapMaybe)
import Numeric                (readDec)
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


inputMulti :: (Functor m, FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]                                -- ^ value, label, initially checked
                -> (FormId -> [(FormId, Int, lbl, Bool)] -> view)  -- ^ function which generates the view
                -> Form m input error view () [a]
inputMulti choices mkView =
    Form $ do i <- getFormId
              inp <- getFormInput' i
              view <- mkView i <$> augmentChoices choices
              case inp of
                Default ->
                    do let vals = mapMaybe (\(a,_,checked) -> if checked then Just a else Nothing) choices
                       mkRet i view vals
                Missing -> -- can happen if no choices where checked
                     do mkRet i view []
                (Found v) ->
                    do let readDec' str = case readDec str of
                                            [(n,[])] -> n
                           keys   = map readDec' $ getInputStrings v
                           intMap = I.fromAscList $ zipWith (\i (a, _,_) -> (i, a)) [0..] choices
                           vals   = mapMaybe (\i -> I.lookup i intMap) keys
                       mkRet i view vals

    where
      augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
      augmentChoices choices = mapM augmentChoice (zip [0..] choices)

      augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
      augmentChoice (vl, (a, lbl, checked)) =
          do i <- getFormId
             return (i, vl, lbl, checked)
{-
      mkCheckbox :: (Monad m, XMLGenerator x, EmbedAsChild x lbl) => FormId -> (Integer, (a, lbl, Bool)) -> FormState m input [XMLGenT x (XMLType x)]
      mkCheckbox nm (vl, (_,lbl,checked)) =
          do i' <- getFormId
             return $ [ <input type="checkbox" id=i' name=nm value=(show vl) (if checked then [("checked" := "checked")] else []) />
                      , <label for=i'><% lbl %></label>
                      ]
-}

{-
                    do let vals = mapMaybe (\(a,_,checked) -> if checked then Just a else Nothing) choices
                       buttons  <- mkCheckboxes i choices
                       return ( View $ const $ buttons
                              , return $ Ok (Proved { proofs   = ()
                                                    , pos      = unitRange i
                                                    , unProved = vals
                                       })
                              )
                Missing -> -- can happen if no choices where checked
                     do buttons <- mkCheckboxes i choices
                        return ( View $ const $ buttons
                              , return $ Ok (Proved { proofs   = ()
                                                    , pos      = unitRange i
                                                    , unProved = []
                                                    })
                               )
                (Found v) ->
                    do buttons <- mkCheckboxes i choices
                       let readDec' str = case readDec str of
                                            [(n,[])] -> n
                           keys   = map readDec' $ getInputStrings v
                           intMap = I.fromAscList $ zipWith (\i (a, _,_) -> (i, a)) [0..] choices
                           vals   = mapMaybe (\i -> I.lookup i intMap) keys
                       return ( View $ const $ buttons
                              , return $ Ok (Proved { proofs   = ()
                                                    , pos      = unitRange i
                                                    , unProved = vals
                                                    })
                              )
    where
      mkCheckboxes i choices = fmap concat $ mapM (mkCheckbox i) (zip [0..] choices)

      mkCheckbox :: (Monad m, XMLGenerator x, EmbedAsChild x lbl) => FormId -> (Integer, (a, lbl, Bool)) -> FormState m input [XMLGenT x (XMLType x)]
      mkCheckbox nm (vl, (_,lbl,checked)) =
          do i' <- getFormId
             return $ [ <input type="checkbox" id=i' name=nm value=(show vl) (if checked then [("checked" := "checked")] else []) />
                      , <label for=i'><% lbl %></label>
                      ]
-}



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
