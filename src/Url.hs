{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Url where

import qualified Data.Text as T
import Network.URI.Encode (encodeText)

data Url = Url
  { host :: T.Text,
    params :: [(T.Text, T.Text)]
  }

urlEncode :: Url -> T.Text
urlEncode Url {..} =
  if null params
    then host
    else
      host
        <> "?"
        <> T.intercalate "&" (map (\(k, v) -> k <> "=" <> encodeText v) params)
