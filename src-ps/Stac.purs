module Stac where

import Affjax (Error, defaultRequest)
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Either (Either)
import Data.Typelevel.Undefined (undefined)
import Effect.Aff (Aff)
import Model (StacCollection)
import Prelude (bind, pure, ($), (<>))

getCollections :: String -> Aff (Either Error (Array StacCollection))
getCollections apiHost = do
  resp <-
    AX.request
      $ defaultRequest { url = apiHost <> "/collections", responseFormat = ResponseFormat.json }
  -- pure $ toStacCollection resp.body
  undefined
