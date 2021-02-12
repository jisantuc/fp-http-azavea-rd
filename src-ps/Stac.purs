module Stac where

import Affjax (Error(..), Response, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (Json, JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Model.Collection (StacCollection)
import Prelude (bind, pure, ($), (<<<), (<>))

adaptError :: JsonDecodeError -> Response Json -> Error
adaptError jsErr resp =
  RequestContentError
    ( "Request failed to produce a meaningful response: " <> printJsonDecodeError jsErr
    )

getCollections :: String -> Aff (Either Error (Array StacCollection))
getCollections apiHost = do
  resp <-
    AX.request
      $ defaultRequest { url = apiHost <> "/collections", responseFormat = ResponseFormat.json }
  case resp of
    Left e -> pure $ Left e
    Right success -> pure $ lmap (\err -> adaptError err success) $ (decodeCollection <<< _.body) success
  where
  decodeCollection js = decodeJson js :: Either JsonDecodeError (Array StacCollection)
