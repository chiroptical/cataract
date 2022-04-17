module Data.EventKind where

import Data.Aeson
import Data.Either
import Data.Monoid
import Database.Esqueleto.Experimental (PersistField (..), PersistFieldSql (..),
                                        PersistValue (..), SqlType (..))
import GHC.Generics
import Prelude                         (Show)

data EventKind =
    PingKind
  | NewFollowerKind
  | NewSubscriberKind
  | NewCheerKind
  | NewRaidKind
  deriving (Generic, Show)

instance ToJSON EventKind

instance PersistField EventKind where
  toPersistValue = \case
    PingKind          -> PersistText "Ping"
    NewFollowerKind   -> PersistText "NewFollower"
    NewSubscriberKind -> PersistText "NewSubscriber"
    NewCheerKind      -> PersistText "NewCheer"
    NewRaidKind       -> PersistText "NewRaid"

  fromPersistValue = \case
    PersistText eventType ->
      case eventType of
        "Ping"          -> Right PingKind
        "NewFollower"   -> Right NewFollowerKind
        "NewSubscriber" -> Right NewSubscriberKind
        "NewCheer"      -> Right NewCheerKind
        "NewRaid"       -> Right NewRaidKind
        et              -> Left ("Unrecognized event type: " <> et)
    _ -> Left "Invalid PersistField"

instance PersistFieldSql EventKind where
  sqlType _ = SqlString
