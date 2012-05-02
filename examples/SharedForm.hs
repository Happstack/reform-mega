{-# LANGUAGE TypeFamilies #-}
module SharedForm where

import Text.Formettes

data DemoFormError input
    = CommonError (CommonFormError input)
    | InvalidUsername
    | InvalidEmail
      deriving (Show)

instance FormError (DemoFormError input) where
    type ErrorInputType (DemoFormError input) = input
    commonFormError = CommonError

data Username = Username String deriving (Eq, Ord, Read, Show)

mkUsername :: (Monad m) => Form m input error view proof String -> Form m input error view proof Username
mkUsername = imap id Username

data Email      = Email String deriving (Eq, Ord, Read, Show)
data ValidEmail = ValidEmail

validEmailProof :: (Monad m) => error -> Proof m error ValidEmail String String
validEmailProof errorMsg = Proof ValidEmail (return . check)
    where
    check str =
        if '@' `elem` str
        then (Right str)
        else (Left errorMsg)

data User = User { username :: Username
                 , email    :: Email
                 } deriving (Eq, Ord, Read, Show)
data ValidUser = ValidUser

mkUser :: (Monad m, Monoid view) => Form m inptu error view (NotNull -> ValidEmail -> ValidUser) (Username -> Email -> User)
mkUser = ipure (\NotNull ValidEmail -> ValidUser) User
