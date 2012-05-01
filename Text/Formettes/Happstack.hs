{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Text.Formettes.Happstack where

import Control.Applicative                 (optional)
import Data.ByteString.Lazy                (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Maybe                          (mapMaybe)
import Text.Formettes.Backend              (FormInput(..), FileType)
import Text.Formettes.Core                 (Environment(..), Value(..))
import Text.Formettes.Result               (Result(..))
import Happstack.Server                    (Happstack, Input(..), lookInputs)

-- FIXME: we should really look at Content Type and check for non-UTF-8 encodings
instance FormInput [Input] where
    type FileType [Input] = FilePath
    getInputStrings inputs = mapMaybe (asString . inputValue) inputs
        where
          asString :: Either FilePath ByteString -> Maybe String
          asString (Left _)   = Nothing
          asString (Right bs) = Just $ UTF8.toString bs
    getInputFile [i] =
        case inputValue i of
          (Left fp) -> (Just fp)
          _         -> Nothing
    getInputFile _ = Nothing

environment :: (Happstack m) => Environment m [Input]
environment =
    Environment $ \formId ->
        do mv <- optional $ lookInputs (show formId)
           case mv of
             Nothing  -> return $ Missing
             (Just a) -> return $ Found a