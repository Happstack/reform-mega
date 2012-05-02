{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving #-}
module Text.Formettes.Core where

import Control.Applicative    (Applicative(pure, (<*>)))
import Control.Arrow          (first, second)
import Control.Monad.Reader   (MonadReader(ask), ReaderT, runReaderT)
import Control.Monad.State    (MonadState(get,put), StateT, evalStateT)
import Control.Monad.Trans    (lift)
import Data.Monoid            (Monoid(mempty, mappend))
import Text.Formettes.Result

class IndexedFunctor f where
    imap :: (x -> y) -> (a -> b) -> f x a -> f y b

class (IndexedFunctor f) => IndexedApplicative f where
    ipure   :: x -> a -> f x a
    (<+*+>) :: f (x -> y) (a -> b) -> f x a -> f y b

infixl 4 <+*+>

data Proved proofs a =
    Proved { proofs   :: proofs
           , pos      :: FormRange
           , unProved :: a
           }

instance Functor (Proved ()) where
    fmap f (Proved () pos a) = Proved () pos (f a)

unitProved :: FormId -> Proved () ()
unitProved formId =
    Proved { proofs   = ()
           , pos      = unitRange formId
           , unProved = ()
           }

type FormState m input = ReaderT (Environment m input) (StateT FormRange m)

data Value a
    = Missing
    | Found a
    | Default

-- | Utility function: Get the current input
--
getFormInput :: Monad m => FormState m i (Value i)
getFormInput = getFormId >>= getFormInput'

-- | Gets the input of an arbitrary FormId.
--
getFormInput' :: Monad m => FormId -> FormState m input (Value input)
getFormInput' id' = do
    env <- ask
    case env of
      NoEnvironment -> return Default
      Environment f ->
          lift $ lift $ f id'

-- | Utility function: Get the current range
--
getFormRange :: Monad m => FormState m i FormRange
getFormRange = get


-- | The environment is where you get the actual input per form. The environment
-- itself is optional
--
data Environment m input
    = Environment (FormId -> m (Value input))
    | NoEnvironment

-- | Not quite sure when this is useful and so hard to say if the rules for combining things with Missing/Default are correct
instance (Monoid input, Monad m) => Monoid (Environment m input) where
    mempty = NoEnvironment
    NoEnvironment `mappend` x = x
    x `mappend` NoEnvironment = x
    (Environment env1) `mappend` (Environment env2) =
        Environment $ \id' ->
            do r1 <- (env1 id')
               r2 <- (env2 id')
               case (r1, r2) of
                 (Missing, Missing) -> return Missing
                 (Default, Missing) -> return Default
                 (Missing, Default) -> return Default
                 (Found x, Found y) -> return $ Found (x `mappend` y)
                 (Found x, _      ) -> return $ Found x
                 (_      , Found y) -> return $ Found y

-- | Utility function: returns the current 'FormId'. This will only make sense
-- if the form is not composed
--
getFormId :: Monad m => FormState m i FormId
getFormId = do
    FormRange x _ <- get
    return x

incFormId :: Monad m => FormState m i ()
incFormId = do
        FormRange _ endF1 <- get
        put $ unitRange endF1

-- | A view represents a visual representation of a form. It is composed of a
-- function which takes a list of all errors and then produces a new view
--
newtype View error v = View
    { unView :: [(FormRange, error)] -> v
    } deriving (Monoid)

instance Functor (View e) where
    fmap f (View g) = View $ f . g

newtype Form m input error view proof a = Form { unForm :: FormState m input (View error view, m (Result error (Proved proof a))) }

instance (Monad m) => IndexedFunctor (Form m input view error) where
    imap f g (Form frm) =
        Form $ do (view, mval) <- frm
                  val <- lift $ lift $ mval
                  case val of
                    (Ok (Proved p pos a)) -> return (view, return $ Ok (Proved (f p) pos (g a)))
                    (Error errs)          -> return (view, return $ Error errs)

(<+$+>) :: IndexedFunctor f => (a -> b) -> f y a -> f y b
(<+$+>) = imap id

infixl 4 <+$+>

instance (Monoid view, Monad m) => IndexedApplicative (Form m input error view) where
    ipure p a = Form $ do i <- getFormId
                          return (mempty, return $ Ok (Proved p (unitRange i) a))

    (Form frmF) <+*+> (Form frmA) =
        Form $ do (xml1, mfok) <- frmF
                  incFormId
                  (xml2, maok) <- frmA
                  fok <- lift $ lift $ mfok
                  aok <- lift $ lift $ maok
                  case (fok, aok) of
                     (Error errs1, Error errs2) -> return (xml1 `mappend` xml2, return $ Error $ errs1 ++ errs2)
                     (Error errs1, _)           -> return (xml1 `mappend` xml2, return $ Error $ errs1)
                     (_          , Error errs2) -> return (xml1 `mappend` xml2, return $ Error $ errs2)
                     (Ok (Proved p (FormRange x _) f), Ok (Proved q (FormRange _ y) a)) ->
                         return (xml1 `mappend` xml2, return $ Ok $ Proved { proofs   = p q
                                                                           , pos      = FormRange x y
                                                                           , unProved = f a
                                                                           })

instance (Functor m) => Functor (Form m input error view ()) where
    fmap f form =
        Form $ fmap (second (fmap (fmap (fmap f)))) (unForm form)


instance (Functor m, Monoid view, Monad m) => Applicative (Form m input error view ()) where
    pure a =
      Form $
        do i <- getFormId
           return (View $ const $ mempty, return $ Ok $ Proved { proofs    = ()
                                                               , pos       = FormRange i i
                                                               , unProved  = a
                                                               })
    -- this coud be defined in terms of <+*+> if we just changed the proof of frmF to (() -> ())
    (Form frmF) <*> (Form frmA) =
       Form $
         do (xml1, mfok) <- frmF
            incFormId
            (xml2, maok) <- frmA
            fok <- lift $ lift $ mfok
            aok <- lift $ lift $ maok
            case (fok, aok) of
              (Error errs1, Error errs2) -> return (xml1 `mappend` xml2, return $ Error $ errs1 ++ errs2)
              (Error errs1, _)           -> return (xml1 `mappend` xml2, return $ Error $ errs1)
              (_          , Error errs2) -> return (xml1 `mappend` xml2, return $ Error $ errs2)
              (Ok (Proved p (FormRange x _) f), Ok (Proved q (FormRange _ y) a)) ->
                  return (xml1 `mappend` xml2, return $ Ok $ Proved { proofs   = ()
                                                                    , pos      = FormRange x y
                                                                    , unProved = f a
                                                                    })

-- * Ways to evaluate a Form

-- | Run a form
--
runForm :: (Monad m) =>
           String
        -> Environment m input
        -> Form m input error view proof a
        -> m (View error view, m (Result error (Proved proof a)))
runForm prefix env form =
    evalStateT (runReaderT (unForm form) env) (unitRange (zeroId prefix))


-- | Just evaluate the form to a view. This usually maps to a GET request in the
-- browser.
--
viewForm :: (Monad m) => String -> Form m input error view proof a -> m view
viewForm prefix form =
    do (v, _) <- runForm prefix NoEnvironment form
       return (unView v [])

-- | Evaluate a form to it's view if it fails
--
eitherForm :: Monad m
           => String                          -- ^ Identifier for the form
           -> Environment m input             -- ^ Input environment
           -> Form m input error view proof a -- ^ Form to run
           -> m (Either view a)               -- ^ Result
eitherForm form id' env = do
    (view', mresult) <- runForm form id' env
    result <- mresult
    return $ case result of
        Error e  -> Left $ unView view' e
        Ok x     -> Right (unProved x)

-- | Insert a view into the functor
--
view :: Monad m
     => view                        -- ^ View to insert
     -> Form m input error view () ()  -- ^ Resulting form
view view' =
  Form $
    do i <- getFormId
       return (View (const view'), return (Ok (Proved { proofs   = ()
                                                             , pos      = FormRange i i
                                                             , unProved = ()
                                                             })))

-- | Append a unit form to the left. This is useful for adding labels or error
-- fields
--
(++>) :: (Monad m, Monoid view)
      => Form m input error view () ()
      -> Form m input error view proof a
      -> Form m input error view proof a
f1 ++> f2 = Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v2, r) <- unForm f2
    (v1, _) <- unForm f1
    return (v1 `mappend` v2, r)

infixl 6 ++>

-- | Append a unit form to the right. See '++>'.
--
(<++) :: (Monad m, Monoid view)
      => Form m input error view proof a
      -> Form m input error view () ()
      -> Form m input error view proof a
f1 <++ f2 = Form $ do
    -- Evaluate the form that matters first, so we have a correct range set
    (v1, r) <- unForm f1
    (v2, _) <- unForm f2
    return (v1 `mappend` v2, r)

infixr 5 <++

-- | Change the view of a form using a simple function
--
mapView :: (Monad m, Functor m)
        => (view -> view')        -- ^ Manipulator
        -> Form m input error view  proof a  -- ^ Initial form
        -> Form m input error view' proof a  -- ^ Resulting form
mapView f = Form . fmap (first $ fmap f) . unForm
