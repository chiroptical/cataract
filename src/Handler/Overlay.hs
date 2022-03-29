{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Overlay where

import Import

emptyLayout :: Yesod site => WidgetFor site () -> HandlerFor site Html
emptyLayout w = do
    p <- widgetToPageContent w
    withUrlRenderer [hamlet|
        $newline never
        $doctype 5
        <html>
          <head>
            <title>#{pageTitle p}
            ^{pageHead p}
          <body>
            ^{pageBody p}
        |]

-- | Notes
--
-- - example overlay: https://overlay.player.me/qfRbSKTkajToTzfO
-- - svg.js: https://svgjs.dev/docs/3.1
getOverlayR :: Handler Html
getOverlayR = emptyLayout $ do
  setTitle "Overlay"
  $(widgetFile "overlay")
