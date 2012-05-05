{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Text.Formettes.Happstack where

import Control.Applicative                 (optional)
import Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Either                         (lefts, rights)
import Data.Maybe                          (mapMaybe)
import Text.Formettes.Backend              (FormInput(..), FileType, CommonFormError(NoFileFound, MultiFilesFound), commonFormError)
import Text.Formettes.Core                 (Environment(..), Value(..))
import Text.Formettes.Result               (Result(..))
import Happstack.Server                    (Happstack, Input(..), lookInputs)

-- FIXME: we should really look at Content Type and check for non-UTF-8 encodings
instance FormInput [Input] where
    type FileType [Input] = FilePath
    getInputStrings inputs = map UTF8.toString $ rights $ map inputValue inputs
    getInputFile inputs =
        case lefts (map inputValue inputs) of
          []   -> Left (commonFormError $ NoFileFound inputs)
          [fp] -> Right fp
          _    -> Left (commonFormError $ MultiFilesFound inputs)

environment :: (Happstack m) => Environment m [Input]
environment =
    Environment $ \formId ->
        do ins <- lookInputs (show formId)
           case ins of
             []  -> return $ Missing
             _   -> return $ Found ins
