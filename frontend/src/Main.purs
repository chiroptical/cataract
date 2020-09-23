module Main where

import Prelude
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Overlay as Overlay

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI Overlay.component unit body
