{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Text.Formettes.Backend where

import Data.Maybe            (listToMaybe)
import           Data.Text   (Text)
import qualified Data.Text   as T
import Text.Formettes.Result (FormId)

data CommonFormError input
    = InputMissing FormId
    | NoStringFound input
    | NoFileFound input
    | MultiFilesFound input
    | MultiStringsFound input
      deriving (Eq, Ord, Show)

class FormError e where
    type ErrorInputType e
    commonFormError :: (CommonFormError (ErrorInputType e)) -> e

-- | Class which all backends should implement. @i@ is here the type that is
-- used to represent a value uploaded by the client in the request
--
class FormInput input where
    type FileType input
    -- | Parse the input into a string. This is used for simple text fields
    -- among other things
    --
    getInputString :: (FormError error, ErrorInputType error ~ input) => input -> Either error String
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
    getInputText :: (FormError error, ErrorInputType error ~ input) => input -> Either error Text
    getInputText input =
           case getInputTexts input of
             []  -> Left (commonFormError $ NoStringFound input)
             [s] -> Right s
             _   -> Left (commonFormError $ MultiStringsFound input)


    -- | Can be overriden for efficiency concerns
    --
    getInputTexts :: input -> [Text]
    getInputTexts = map T.pack . getInputStrings

    -- | Get a file descriptor for an uploaded file
    --
    getInputFile :: (FormError error, ErrorInputType error ~ input) => input -> Either error (FileType input)
