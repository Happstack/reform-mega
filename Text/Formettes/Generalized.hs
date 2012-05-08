{-# LANGUAGE ScopedTypeVariables, TypeFamilies, ViewPatterns #-}
module Text.Formettes.Generalized where

import Control.Applicative    ((<$>))
import qualified Data.IntSet  as IS
import Data.List              (find)
import Data.Maybe             (mapMaybe)
import Numeric                (readDec)
import Text.Formettes.Backend
import Text.Formettes.Core
import Text.Formettes.Result

input :: (Monad m, FormError error) =>
         (input -> Either error a)
      -> (FormId -> a -> view)
      -> a
      -> Form m input error view () a
input fromInput toView initialValue =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ toView i initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = initialValue
                                                 }))
                (Found (fromInput -> (Right a))) ->
                    return ( View $ const $ toView i a
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = a
                                                 }))
                (Found (fromInput -> (Left error))) ->
                    return ( View $ const $ toView i initialValue
                           , return $ Error [(unitRange i, error)]
                           )
                Missing ->
                    return ( View $ const $ toView i initialValue
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )

inputMaybe :: (Monad m, FormError error) =>
         (input -> Either error a)
      -> (FormId -> a -> view)
      -> a
      -> Form m input error view () (Maybe a)
inputMaybe fromInput toView initialValue =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ toView i initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = Just initialValue
                                                 }))
                (Found (fromInput -> (Right a))) ->
                    return ( View $ const $ toView i a
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = (Just a)
                                                 }))
                (Found (fromInput -> (Left error))) ->
                    return ( View $ const $ toView i initialValue
                           , return $ Error [(unitRange i, error)]
                           )
                Missing ->
                    return ( View $ const $ toView i initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = Nothing
                                                 })
                           )

input' :: (Monad m, FormError error) =>
         (input -> Either error a)
      -> (FormId -> Value a -> a -> v)
      -> a
      -> Form m input error v () a
input' fromInput toView initialValue =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ toView i Default initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = initialValue
                                                 }))
                (Found (fromInput -> (Right a))) ->
                    return ( View $ const $ toView i (Found a) initialValue
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = a
                                                 }))
                (Found (fromInput -> (Left error))) ->
                    return ( View $ const $ toView i Missing initialValue
                           , return $ Error [(unitRange i, error)]
                           )
                _ ->
                    return ( View $ const $ toView i Missing initialValue
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )

inputReset :: (Monad m) =>
              (FormId -> a -> view)
           -> a
           -> Form m input error view () ()
inputReset toView a =
    Form $ do i <- getFormId
              return ( View $ const $ toView i a
                     , return $ Ok (Proved { proofs   = ()
                                           , pos      = unitRange i
                                           , unProved = ()
                                           })
                     )

inputFile :: forall m input error view. (Monad m, FormInput input, FormError error, ErrorInputType error ~ input) =>
             (FormId -> view)
          -> Form m input error view () (FileType input)
inputFile toView =
    Form $ do i <- getFormId
              v <- getFormInput' i
              case v of
                Default ->
                    return ( View $ const $ toView i
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )

                (Found (getInputFile' -> (Right a))) ->
                    return ( View $ const $ toView i
                           , return $ Ok (Proved { proofs   = ()
                                                 , pos      = unitRange i
                                                 , unProved = a
                                                 }))

                (Found (getInputFile' -> (Left error))) ->
                    return ( View $ const $ toView i
                           , return $ Error [(unitRange i, error)]
                           )
                Missing ->
                    return ( View $ const $ toView i
                           , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                           )
        where
          -- just here for the type-signature to make the type-checker happy
          getInputFile' :: (FormError error, ErrorInputType error ~ input) => input -> Either error (FileType input)
          getInputFile' = getInputFile

-- | checkboxes, multi-select boxes
inputMulti :: (Functor m, FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
                   [(a, lbl, Bool)]                                -- ^ value, label, initially checked
                -> (FormId -> [(FormId, Int, lbl, Bool)] -> view)  -- ^ function which generates the view
                -> Form m input error view () [a]
inputMulti choices mkView =
    Form $ do i <- getFormId
              inp <- getFormInput' i
              case inp of
                Default ->
                    do view <- mkView i <$> augmentChoices choices
                       let vals = mapMaybe (\(a,_,checked) -> if checked then Just a else Nothing) choices
                       mkRet i view vals
                Missing -> -- can happen if no choices where checked
                     do view <- mkView i <$> augmentChoices choices
                        mkRet i view []
                (Found v) ->
                    do let readDec' str = case readDec str of
                                            [(n,[])] -> n
                                            _ -> (-1) -- FIXME: should probably return an internal error?
                           keys   = IS.fromList $ map readDec' $ getInputStrings v
                           (choices', vals) =
                               foldr (\(i, (a,lbl,_)) (c,v) ->
                                          if IS.member i keys
                                          then ((a,lbl,True) : c, a : v)
                                          else ((a,lbl,False): c,     v)) ([],[]) $
                                 zip [0..] choices
                       view <- mkView i <$> augmentChoices choices'
                       mkRet i view vals

    where
      augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
      augmentChoices choices = mapM augmentChoice (zip [0..] choices)

      augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
      augmentChoice (vl, (a, lbl, checked)) =
          do i <- getFormId
             return (i, vl, lbl, checked)


-- | radio buttons, single-select boxes
inputChoice :: forall a m error input lbl view. (Functor m, FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               (a -> Bool)                                     -- ^ is default
            -> [(a, lbl)]                                      -- ^ value, label
            -> (FormId -> [(FormId, Int, lbl, Bool)] -> view)  -- ^ function which generates the view
            -> Form m input error view () a
inputChoice isDefault choices mkView =
    Form $ do i <- getFormId
              inp <- getFormInput' i

              case inp of
                Default ->
                    do let (choices', def) = markSelected choices
                       view <- mkView i <$> augmentChoices choices'
                       mkRet' i view def

                Missing -> -- can happen if no choices where checked
                    do let (choices', def) = markSelected choices
                       view <- mkView i <$> augmentChoices choices'
                       mkRet' i view def

                (Found v) ->
                    do let readDec' str = case readDec str of
                                            [(n,[])] -> n
                                            _ -> (-1) -- FIXME: should probably return an internal error?
                           (Right str) = getInputString v :: Either error String -- FIXME
                           key = readDec' str
                           (choices', mval) =
                               foldr (\(i, (a, lbl)) (c, v) ->
                                          if i == key
                                          then ((a,lbl,True) : c, Just a)
                                          else ((a,lbl,False): c,     v))
                                     ([], Nothing) $
                                     zip [0..] choices
                       view <- mkView i <$> augmentChoices choices'
                       case mval of
                         Nothing ->
                             return ( View $ const $ view
                                    , return $ Error [(unitRange i, commonFormError (InputMissing i))]
                                    )
                         (Just val) -> mkRet i view val

    where
      mkRet' i view (Just val) = mkRet i view val
      mkRet' i view Nothing =
          return ( View $ const $ view
                 , return $ Error [(unitRange i, commonFormError MissingDefaultValue)]
                 )

      markSelected :: [(a,lbl)] -> ([(a, lbl, Bool)], Maybe a)
      markSelected cs = foldr (\(a,lbl) (vs, ma) ->
                                   if isDefault a
                                      then ((a,lbl,True):vs , Just a)
                                      else ((a,lbl,False):vs, ma))
                         ([], Nothing)
                         cs

      augmentChoices :: (Monad m) => [(a, lbl, Bool)] -> FormState m input [(FormId, Int, lbl, Bool)]
      augmentChoices choices = mapM augmentChoice (zip [0..] choices)

      augmentChoice :: (Monad m) => (Int, (a, lbl, Bool)) -> FormState m input (FormId, Int, lbl, Bool)
      augmentChoice (vl, (a, lbl,selected)) =
          do i <- getFormId
             return (i, vl, lbl, selected)

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
