module Stac where

import Affjax (Error, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (JsonDecodeError, decodeJson)
import Data.Either (Either)
import Data.Typelevel.Undefined (undefined)
import Effect.Aff (Aff)
import Model.Collection (StacCollection)
import Prelude (bind, ($), (<$>), (<<<), (<>))

getCollections :: String -> Aff (Either Error (Array StacCollection))
getCollections apiHost = do
  resp <-
    AX.request
      $ defaultRequest { url = apiHost <> "/collections", responseFormat = ResponseFormat.json }
  let
    result = (decodeCollection <<< _.body) <$> resp
  undefined
  where
  decodeCollection js = decodeJson js :: Either JsonDecodeError (Array StacCollection)
