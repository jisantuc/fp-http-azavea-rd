module StacLinkType where

import Prelude
import Data.Argonaut (class DecodeJson, JsonDecodeError(..), toString)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

data StacLinkType
  = Self
  | StacRoot
  | Parent
  | Child
  | Item
  | Items
  | Source
  | Collection
  | License
  | Alternate
  | DescribedBy
  | Next
  | Prev
  | ServiceDesc
  | ServiceDoc
  | Conformance
  | Data
  | LatestVersion
  | PredecessorVersion
  | SuccessorVersion
  | DerivedFrom
  | VendorLinkType String

instance decodeStacLinkType :: DecodeJson StacLinkType where
  decodeJson js = case toString js of
    Just "self" -> Right Self
    Just "root" -> Right StacRoot
    Just "parent" -> Right Parent
    Just "child" -> Right Child
    Just "item" -> Right Item
    Just "items" -> Right Items
    Just "source" -> Right Source
    Just "collection" -> Right Collection
    Just "license" -> Right License
    Just "alternate" -> Right Alternate
    Just "describedBy" -> Right DescribedBy
    Just "next" -> Right Next
    Just "prev" -> Right Prev
    Just "service-desc" -> Right ServiceDesc
    Just "service-doc" -> Right ServiceDoc
    Just "conformance" -> Right Conformance
    Just "data" -> Right Data
    Just "latest-version" -> Right LatestVersion
    Just "predecessor-version" -> Right PredecessorVersion
    Just "successor-version" -> Right SuccessorVersion
    Just "derived_from" -> Right DerivedFrom
    Just s -> (Right <<< VendorLinkType) s
    Nothing -> (Left <<< UnexpectedValue) js
