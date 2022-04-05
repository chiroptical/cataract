module AesonUtils where

import           Data.Aeson.TH
import           Data.Aeson.Types    (camelTo2)
import           Import.NoFoundation

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

jsonDeriveSnakeCaseDropPrefix :: Text -> Options
jsonDeriveSnakeCaseDropPrefix t =
  defaultOptions
    { fieldLabelModifier = camelToSnake . drop (length t)
    , omitNothingFields = True
    , constructorTagModifier = camelToSnake . drop (length t)
    }
