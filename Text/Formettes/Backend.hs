{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Text.Formettes.Backend where

import Data.Maybe            (listToMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import Text.Formettes.Result (FormId)

-- | Class which all backends should implement. @i@ is here the type that is
-- used to represent a value uploaded by the client in the request
--
class FormInput input where
    type FileType input
    -- | Parse the input into a string. This is used for simple text fields
    -- among other things
    --
    getInputString :: (FormError e, ErrorInputType e ~ input) => input -> Either e String
    getInputString input =
           case getInputStrings input of
             []  -> Left (commonFormError $ NoStringFound input)
             [s] -> Right s
             _   -> Left (commonFormError $ MultiStringsFound input)

    -- | Should be implemented
    --
    getInputStrings :: input -> [String]

    -- | Parse the input value into 'Text'
    --
    getInputText :: input -> Maybe Text
    getInputText = listToMaybe . getInputTexts

    -- | Can be overriden for efficiency concerns
    --
    getInputTexts :: input -> [Text]
    getInputTexts = map T.pack . getInputStrings

    -- | Get a file descriptor for an uploaded file
    --
    getInputFile :: input -> Maybe (FileType input)

data CommonFormError input
    = InputMissing FormId
    | NoStringFound input
    | MultiStringsFound input
      deriving (Eq, Ord, Show)

class FormError e where
    type ErrorInputType e
    commonFormError :: (CommonFormError (ErrorInputType e)) -> e