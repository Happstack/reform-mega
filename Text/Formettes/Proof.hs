module Text.Formettes.Proof where

import Control.Monad.Trans   (lift)
import Numeric               (readDec, readFloat, readSigned)
import Text.Formettes.Result (FormRange, Result(..))
import Text.Formettes.Core   (IndexedFunctor(imap), Form(..), Proved(..))

data Proof m error proof a b
    = Proof { proofName     :: proof
            , proofFunction :: a -> m (Either error b)
            }

prove :: (Monad m) => Form m input error view q a -> Proof m error proof a b -> Form m input error view proof b
prove (Form frm) (Proof p f) =
    Form $ do (xml, mval) <- frm
              val <- lift $ lift $ mval
              case val of
                (Error errs)            -> return (xml, return $ Error errs)
                (Ok (Proved _ pos a)) ->
                    do r <- lift $ lift $ f a
                       case r of
                         (Left err) -> return (xml, return $ Error [(pos, err)])
                         (Right b)  ->
                             return (xml, return $ Ok (Proved { proofs   = p
                                                              , pos      = pos
                                                              , unProved = b
                                                              }))

-- * transformations (proofs minus the proof)


transform :: (Monad m) => Form m input error view anyProof a -> Proof m error proof a b -> Form m input error view () b
transform frm proof = imap (const ()) id (frm `prove` proof)

transformEitherM :: (Monad m) => Form m input error view anyProof a -> (a -> m (Either error b)) -> Form m input error view () b
transformEitherM frm func = frm `transform` (Proof () func)

transformEither :: (Monad m) => Form m input error view anyProof a -> (a -> Either error b) -> Form m input error view () b
transformEither frm func = transformEitherM frm (return . func)

-- * Various Proofs

data NotNull = NotNull

notNullProof :: (Monad m) => error -> Proof m error NotNull [a] [a]
notNullProof errorMsg = Proof NotNull (return . check)
    where
    check list =
        if null list
          then (Left errorMsg)
          else (Right list)

data Decimal        = Decimal
data RealFractional = RealFractional
data Signed a       = Signed a

-- | read an unsigned number in decimal notation
decimal :: (Monad m, Eq i, Num i) => (String -> error) -> Proof m error Decimal String i
decimal mkError = Proof Decimal (return . toDecimal)
    where
      toDecimal str =
          case readDec str of
            [(d,[])] -> (Right d)
            _        -> (Left $ mkError str)

-- | read signed decimal number
signedDecimal :: (Monad m, Eq i, Real i) => (String -> error) -> Proof m error (Signed Decimal) String i
signedDecimal mkError = Proof (Signed Decimal) (return . toDecimal)
    where
      toDecimal str =
          case (readSigned readDec) str of
            [(d,[])] -> (Right d)
            _        -> (Left $ mkError str)

realFrac :: (Monad m, RealFrac a) => (String -> error) -> Proof m error RealFractional String a
realFrac mkError = Proof RealFractional (return . toRealFrac)
    where
      toRealFrac str =
          case readFloat str of
            [(f,[])] -> (Right f)
            _        -> (Left $ mkError str)

realFracSigned :: (Monad m, RealFrac a) => (String -> error) -> Proof m error (Signed RealFractional) String a
realFracSigned mkError = Proof (Signed RealFractional) (return . toRealFrac)
    where
      toRealFrac str =
          case (readSigned readFloat) str of
            [(f,[])] -> (Right f)
            _        -> (Left $ mkError str)

