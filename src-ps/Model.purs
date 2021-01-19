module Model where

import Data.Argonaut (Json)
import Data.Date (Month(..))
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Refined (Refined, RefinedError(..), SizeEqualTo, refine)
import Data.Refined.Predicate (class Predicate)
import Data.Typelevel.Num (D4)
import Prelude ((<$>), (<*>))

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

type TwoDimBbox = Refined (SizeEqualTo D4) (Array Number)

type SpatialExtent = {
    bbox :: Array TwoDimBbox
}

type TemporalExtent = Refined (OneOrBoth DateTime) (Array (Maybe DateTime))

aDateTime :: Maybe DateTime
aDateTime = 
  let
    date = canonicalDate <$> toEnum 2021 <*> Just January <*> toEnum 1
    time = Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
  in
     DateTime <$> date <*> time

extent :: Either (RefinedError (Array (Maybe DateTime))) TemporalExtent
extent = refine [Nothing, aDateTime]

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

