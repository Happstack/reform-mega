{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}
module Text.Formettes.Heist where

import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Map as Map
import           Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Formettes.Backend
import Text.Formettes.Core
import qualified Text.Formettes.Generalized as G
import Text.Formettes.Result
import Text.Templating.Heist
import qualified Text.XmlHtml as X

newtype RefMap = RefMap
    { inputMap :: Map Ref (FormId, String)
--    , errorMap :: Map Ref [(FormId, error)]
    } deriving (Monoid)


formettesSplices :: Monad m => RefMap -> [(FormRange, Text)] -> [(Text, Splice m)]
formettesSplices refMap errors =
    [ ("fmInputString",      fmInputString refMap)
    , ("fmErrorList",        fmErrorList refMap errors)
    ]

attr :: Bool -> (Text, Text) -> [(Text, Text)] -> [(Text, Text)]
attr False _ = id
attr True  a = (a :)

makeElement :: Text -> [X.Node] -> [(Text, Text)] -> [X.Node]
makeElement name nodes attrs = [X.Element name attrs nodes]

getRefAttributes :: Monad m => HeistT m (Text, [(Text, Text)])
getRefAttributes = do
    node <- getParamNode
    return $ case node of
        X.Element _ as _ ->
            let ref = fromMaybe (error $ show node ++ ": missing ref") $
                        lookup "ref" as
            in (ref, filter ((/= "ref") . fst) as)
        _                -> (error "Wrong type of node!", [])

type Ref = Text

inputString :: (FormError error, ErrorInputType error ~ input, FormInput input, Monad m) =>
               Ref
            -> String
            -> Form m input error RefMap () String
inputString ref initialValue = G.input getInputString mkInput initialValue
    where
      mkInput i v =
          RefMap { inputMap = Map.singleton ref (i, v)
  --                  , errorMap = Map.empty
                    }

-- | Generate a text input field. Example:
--
-- > <dfInputText ref="user.name" />
fmInputString :: Monad m => RefMap -> Splice m
fmInputString refMap = do
    (ref, attrs) <- getRefAttributes
    case Map.lookup ref (inputMap refMap) of
      (Just (i, s)) ->
            return $ makeElement "input" [] $
                             ("type", "text") : ("id", T.pack $ show i) :
                             ("name", T.pack $ show i) : ("value", T.pack s) : attrs



errorList :: [Text] -> [(Text, Text)] -> [X.Node]
errorList []   _     = []
errorList errs attrs = [X.Element "ul" attrs $ map makeError errs]
  where
    makeError e = X.Element "li" [] [X.TextNode e]


-- | Display the list of errors for a certain field. Example:
--
-- > <dfErrorList ref="user.name" />
-- > <dfInputText ref="user.name" />
fmErrorList :: Monad m => RefMap -> [(FormRange, Text)] -> Splice m
fmErrorList refMap errors =
    do (ref, attrs) <- getRefAttributes
       case Map.lookup ref (inputMap refMap) of
         (Just (formId, _)) ->
             case filter (\((FormRange x _), _) -> x == formId) errors of
               [] -> return []
               errs -> return $ errorList (map snd errs) attrs


--     return $ errorList (errors ref view) attrs
