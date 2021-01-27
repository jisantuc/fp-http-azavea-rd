module Model where

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), toString)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Refined (Refined, RefinedError(..), SizeEqualTo)
import Data.Refined.Predicate (class Predicate)
import Data.String (toLower)
import Data.Typelevel.Num (D4)
import Prelude (($), (<$>))

-- predicate requiring at least one non-Nothing item in a list of two items
-- implies SizeEqualTo D2, but I don't know how to tell the compiler that
data OneOrBoth a

instance predicateOneOrBoth :: Predicate (OneOrBoth p) (Array (Maybe x)) where
  validate _ arr =
    case arr of
      [Just _, _] -> Right arr
      [_, Just _] -> Right arr
      _ -> Left NotError

data StacProviderRole =
  Licensor
  | Producer
  | Processor
  | Host

instance decodeStacProviderRole :: DecodeJson StacProviderRole where
  decodeJson js = case toLower <$> toString js of
    Just "licensor" -> Right Licensor
    Just "producer" -> Right Producer
    Just "processor" -> Right Processor
    Just "host" -> Right Host
    Just _ -> Left $ UnexpectedValue js
    Nothing -> Left $ TypeMismatch ("Expected a JSON String")

type TwoDimBbox = Refined (SizeEqualTo D4) (Array Number)

type SpatialExtent = {
    bbox :: Array TwoDimBbox
}

type TemporalExtent = Refined (OneOrBoth JsonDate) (Array (Maybe JsonDate))

type Interval = {
    interval :: Array TemporalExtent
}

type StacExtent = {
    spatial :: SpatialExtent,
    temporal :: Interval
}

type StacProvider = {
    name :: String,
    description :: Maybe String,
    roles :: Array StacProviderRole,
    url :: Maybe String
}

data StacLinkType =
  Self                                   
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

type StacLink = {
  href :: String,
  rel :: StacLinkType,
  _type :: Maybe String,
  title :: Maybe String,
  extensionFields :: Json
}

type StacCollection = {
    stacVersion :: String,
    stacExtensions :: Array String,
    id :: String,
    title :: Maybe String,
    description :: String,
    keywords :: Array String,
    license :: String,
    providers :: Array StacProvider,
    extent :: StacExtent,
    summaries :: Json,
    properties :: Json,
    links :: Array StacLink,
    extensionFields :: Json
}

