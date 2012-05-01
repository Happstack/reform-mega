module Text.Formettes.Proof where

import Control.Monad.Trans   (lift)
import Text.Formettes.Result (FormRange, Result(..))
import Text.Formettes.Core   (Form(..), Proved(..))

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

data NotNull = NotNull

notNullProof :: (Monad m) => error -> Proof m error NotNull [a] [a]
notNullProof errorMsg = Proof NotNull (return . check)
    where
    check list =
        if null list
          then (Left errorMsg)
          else (Right list)

