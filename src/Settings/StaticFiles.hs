{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Settings.StaticFiles where

import Settings (appDevelopment, appStaticDir, compileTimeAppSettings)
import Yesod.EmbeddedStatic (embedDir, mkEmbeddedStatic)

-- | Documentation: https://hackage.haskell.org/package/yesod-static-1.6.1.0/docs/Yesod-EmbeddedStatic.html#v:mkEmbeddedStatic
mkEmbeddedStatic
  (appDevelopment compileTimeAppSettings)
  "myStatic"
  [embedDir (appStaticDir compileTimeAppSettings)]
