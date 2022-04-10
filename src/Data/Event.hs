module Data.Event where

import Data.Aeson
import Data.Either
import Data.Monoid
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql (..),
                                        PersistValue (..), SqlType (..))
import GHC.Generics

data Event =
    NewFollower
  | NewSubscriber
  | NewCheer
  | NewRaid
  deriving Generic

instance ToJSON Event

instance PersistField Event where
  toPersistValue = \case
    NewFollower   -> PersistText "NewFollower"
    NewSubscriber -> PersistText "NewSubscriber"
    NewCheer      -> PersistText "NewCheer"
    NewRaid       -> PersistText "NewRaid"

  fromPersistValue = \case
    PersistText eventType ->
      case eventType of
        "NewFollower"   -> Right NewFollower
        "NewSubscriber" -> Right NewSubscriber
        "NewCheer"      -> Right NewCheer
        "NewRaid"       -> Right NewRaid
        et              -> Left ("Unrecognized event type: " <> et)
    _ -> Left "Invalid PersistField"

instance PersistFieldSql Event where
  sqlType _ = SqlString
