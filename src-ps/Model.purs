module Model where

import Control.Apply (lift2)
import Control.Monad.Gen (elements)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, stringify, toObject, toString, (.:), (:=), (~>))
import Data.Array.NonEmpty (cons', toNonEmpty)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.Refined (class Predicate, Refined, RefinedError(..), unsafeRefine)
import Data.Set as Set
import Data.String (toLower)
import Foreign.Object (Object, filterKeys)
import Foreign.Object.Gen (genForeignObject)
import JsonDate (JsonDate)
import Prelude (class Eq, class Show, apply, bind, map, not, pure, show, ($), (+), (-), (<$>), (<<<), (<>), (>>=))
import StacLinkType (StacLinkType)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen, arrayOf, oneOf)

-- predicate requiring at least one non-Nothing item in a list of two items
-- implies SizeEqualTo D2, but I don't know how to tell the compiler that
data OneOrBoth a

instance predicateOneOrBoth :: Predicate (OneOrBoth p) (Array (Maybe x)) where
  validate _ arr = case arr of
    [ Just _, _ ] -> Right arr
    [ _, Just _ ] -> Right arr
    _ -> Left NotError

data StacProviderRole
  = Licensor
  | Producer
  | Processor
  | Host

derive instance eqStacProviderRole :: Eq StacProviderRole

instance showStacProviderRole :: Show StacProviderRole where
  show role = case role of
    Licensor -> "licensor"
    Producer -> "producer"
    Processor -> "processor"
    Host -> "host"

instance decodeStacProviderRole :: DecodeJson StacProviderRole where
  decodeJson js = case toLower <$> toString js of
    Just "licensor" -> Right Licensor
    Just "producer" -> Right Producer
    Just "processor" -> Right Processor
    Just "host" -> Right Host
    Just _ -> Left $ UnexpectedValue js
    Nothing -> Left $ TypeMismatch ("Expected a JSON String")

instance encodeStacProviderRole :: EncodeJson StacProviderRole where
  encodeJson = encodeJson <<< show

instance arbitraryStacProviderRole :: Arbitrary StacProviderRole where
  arbitrary =
    elements
      $ toNonEmpty
          ( Licensor
              `cons'`
                [ Producer
                , Processor
                , Host
                ]
          )

alphaStringGen :: Int -> Gen String
alphaStringGen 0 = pure ""

alphaStringGen n =
  let
    choice =
      elements
        $ toNonEmpty
            ( "a" `cons'` [ "b", "c", "d", "e", "f", "g" ]
            )
  in
    lift2 (<>) choice (alphaStringGen (n - 1))

jsObjectGen :: Gen (Object Json)
jsObjectGen = genForeignObject (alphaStringGen 12) (encodeJson <$> (arbitrary :: Gen Number))

maybe :: forall a. Gen a -> Gen (Maybe a)
maybe g = oneOf $ toNonEmpty $ pure Nothing `cons'` [ Just <$> g ]

newtype TwoDimBbox
  = TwoDimBbox
  { llx :: Number
  , lly :: Number
  , urx :: Number
  , ury :: Number
  }

derive instance eqTwoDimBbox :: Eq TwoDimBbox

instance showTwoDimBbox :: Show TwoDimBbox where
  show (TwoDimBbox { llx, lly, urx, ury }) = show [ llx, lly, urx, ury ]

instance decodeTwoDimBbox :: DecodeJson TwoDimBbox where
  decodeJson js =
    decodeJson js
      >>= ( \x -> case x of
            [ llx, lly, urx, ury ] -> Right $ TwoDimBbox { llx, lly, urx, ury }
            _ -> Left $ UnexpectedValue js
        )

instance encodeTwoDimBbox :: EncodeJson TwoDimBbox where
  encodeJson (TwoDimBbox { llx, lly, urx, ury }) = encodeJson [ llx, lly, urx, ury ]

instance arbitraryTwoDimBbox :: Arbitrary TwoDimBbox where
  arbitrary = do
    llx <- arbitrary
    lly <- arbitrary
    let
      urx = llx + 5.0

      ury = lly + 5.0
    pure $ TwoDimBbox { llx, lly, urx, ury }

type SpatialExtent
  = { bbox :: Array TwoDimBbox
    }

-- TODO: handwrite json and arbitrary instances, derive eq and show
newtype TemporalExtent
  = TemporalExtent (Refined (OneOrBoth JsonDate) (Array (Maybe JsonDate)))

derive newtype instance eqTemporalExtent :: Eq TemporalExtent

derive newtype instance showTemporalExtent :: Show TemporalExtent

instance decodeJsonTemporalExtent :: DecodeJson TemporalExtent where
  decodeJson js = TemporalExtent <$> decodeJson js

derive newtype instance encodeJsonTemporalExtent :: EncodeJson TemporalExtent

instance arbitraryTemporalExtent :: Arbitrary TemporalExtent where
  arbitrary = oneOf $ toNonEmpty $ emptyStartGen `cons'` [ emptyEndGen, bothEndpoints ]
    where
    emptyStartGen = do
      start <- pure Nothing
      end <- Just <$> arbitrary
      pure $ TemporalExtent (unsafeRefine [ start, end ])

    emptyEndGen = do
      start <- Just <$> arbitrary
      end <- pure Nothing
      pure $ TemporalExtent (unsafeRefine [ start, end ])

    bothEndpoints = do
      start <- Just <$> arbitrary
      end <- Just <$> arbitrary
      pure $ TemporalExtent (unsafeRefine [ start, end ])

type Interval
  = { interval :: Array TemporalExtent
    }

type StacExtent
  = { spatial :: SpatialExtent
    , temporal :: Interval
    }

type StacProvider
  = { name :: String
    , description :: Maybe String
    , roles :: Array StacProviderRole
    , url :: Maybe String
    }

newtype StacLink
  = StacLink
  { href :: String
  , rel :: StacLinkType
  , _type :: Maybe String
  , title :: Maybe String
  , extensionFields :: Object Json
  }

derive newtype instance eqStacLink :: Eq StacLink

instance showStacLink :: Show StacLink where
  show = stringify <<< encodeJson

-- what's going wrong here?
-- map decoder expects an array of k v pairs, while I'm encoding as top-level json properties
-- what an adventure! i'll need to write a custom decoding function here
instance decodeStacLink :: DecodeJson StacLink where
  decodeJson js = case toObject js of
    Just obj ->
      let
        fields = Set.fromFoldable [ "href", "rel", "type", "title" ]
      in
        do
          href <- obj .: "href"
          rel <- obj .: "rel"
          _type <- obj .: "type"
          title <- obj .: "title"
          extensionFields <- filterKeys (\key -> not $ elem key fields) <$> decodeJson js
          pure $ StacLink { href, rel, _type, title, extensionFields }
    Nothing -> Left $ TypeMismatch "Expected a JSON object"

instance encodeJsonStacLink :: EncodeJson StacLink where
  encodeJson (StacLink { href, rel, _type, title, extensionFields }) =
    "href" := href
      ~> "rel"
      := rel
      ~> "type"
      := _type
      ~> "title"
      := title
      ~> encodeJson extensionFields

instance arbitraryStacLink :: Arbitrary StacLink where
  arbitrary = ado
    href <- arbitrary
    rel <- arbitrary
    _type <- arbitrary
    title <- arbitrary
    extensionFields <- jsObjectGen
    in StacLink { href, rel, _type, title, extensionFields }

newtype StacCollection
  = StacCollection
  { stacVersion :: String
  , stacExtensions :: Array String
  , id :: String
  , title :: Maybe String
  , description :: String
  , keywords :: Array String
  , license :: String
  , providers :: Array StacProvider
  , extent :: StacExtent
  , summaries :: Json
  , properties :: Json
  , links :: Array StacLink
  , extensionFields :: Object Json
  }

instance decodeJsonStacCollection :: DecodeJson StacCollection where
  decodeJson js =
    let
      fields =
        Set.fromFoldable
          [ "stac_version"
          , "stac_extensions"
          , "id"
          , "title"
          , "description"
          , "keywords"
          , "license"
          , "providers"
          , "extent"
          , "summaries"
          , "properties"
          , "links"
          ]
    in
      case toObject js of
        Just jsObject -> do
          stacVersion <- jsObject .: "stac_version"
          stacExtensions <- jsObject .: "stac_extensions"
          id <- jsObject .: "id"
          title <- jsObject .: "title"
          description <- jsObject .: "description"
          keywords <- jsObject .: "keywords"
          license <- jsObject .: "license"
          providers <- jsObject .: "providers"
          extent <- jsObject .: "extent"
          summaries <- jsObject .: "summaries"
          properties <- jsObject .: "properties"
          links <- jsObject .: "links"
          extensionFields <- filterKeys (\key -> not $ elem key fields) <$> decodeJson js
          pure
            $ StacCollection
                { stacVersion
                , stacExtensions
                , id
                , title
                , description
                , keywords
                , license
                , providers
                , extent
                , summaries
                , properties
                , links
                , extensionFields
                }
        Nothing -> Left $ UnexpectedValue js

instance encodeJsonStacCollection :: EncodeJson StacCollection where
  encodeJson ( StacCollection
      { stacVersion
    , stacExtensions
    , id
    , title
    , description
    , keywords
    , license
    , providers
    , extent
    , summaries
    , properties
    , links
    , extensionFields
    }
  ) =
    "stac_version" := stacVersion
      ~> "stac_extensions"
      := stacExtensions
      ~> "id"
      := id
      ~> "title"
      := title
      ~> "description"
      := description
      ~> "keywords"
      := keywords
      ~> "license"
      := license
      ~> "providers"
      := providers
      ~> "extent"
      := extent
      ~> "summaries"
      := summaries
      ~> "properties"
      := properties
      ~> "links"
      := links
      ~> encodeJson extensionFields

instance arbitraryStacCollection :: Arbitrary StacCollection where
  arbitrary = do
    stacVersion <- pure $ "1.0.0-beta.2"
    stacExtensions <- pure $ []
    id <- alphaStringGen 12
    title <- maybe (alphaStringGen 12)
    description <- alphaStringGen 12
    keywords <- arrayOf (alphaStringGen 12)
    license <- alphaStringGen 12
    providers <- arrayOf arbitrary
    extent <- (arbitrary :: Gen StacExtent)
    summaries <- encodeJson <$> jsObjectGen
    properties <- encodeJson <$> jsObjectGen
    links <- arrayOf arbitrary
    extensionFields <- jsObjectGen
    pure
      $ StacCollection
          { stacVersion
          , stacExtensions
          , id
          , title
          , description
          , keywords
          , license
          , providers
          , extent
          , summaries
          , properties
          , links
          , extensionFields
          }

derive newtype instance eqStacCollection :: Eq StacCollection

instance showStacCollection :: Show StacCollection where
  show = stringify <<< encodeJson
