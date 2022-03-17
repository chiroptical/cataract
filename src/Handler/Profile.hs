{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
  (_, user) <- requireAuthPair
  defaultLayout $ do
    setTitle . toHtml $ twitchUserIdent user <> "'s User page"
    $(widgetFile "profile")
