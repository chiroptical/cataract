module Overlay where

import Prelude
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

-- First `Unit` is the input type
-- Second `Unit` is the output type
component :: forall m a. MonadAff m => H.Component HH.HTML a Unit Unit m
component =
  H.mkComponent
    { initialState: const unit
    , render: render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: forall b c. b -> H.ComponentHTML Unit c m
  render _ =
    HH.div
      [ HP.class_ (ClassName "overlay") ]
      [ HH.h1_ [ HH.text "Hello, Haskell from Purescript :)" ] ]
