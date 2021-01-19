module Test.Main where

import Prelude

import Data.Date (Month(..))
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either)
import Data.Either as Either
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..))
import Data.Refined (RefinedError, refine)
import Effect (Effect)
import Model (TemporalExtent)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = do
  runTest do
    suite "Temporal extent refinement" do
      test "Two nothings -- not ok" $
        Assert.assert "" $ Either.isLeft (refineTemporalExtent [Nothing, Nothing])
      test "Less than two items even if a Just -- not ok" $
        Assert.assert "" $ Either.isLeft (refineTemporalExtent [dateTime])
      test "More than two items even if a Just -- not ok" $
        Assert.assert "" $ Either.isLeft (refineTemporalExtent [dateTime, dateTime, dateTime])
      test "Two items, both Just -- ok" $
        Assert.assert "" $ Either.isRight (refineTemporalExtent [dateTime, dateTime])
      test "Two items, Just in front -- ok" $
        Assert.assert "" $ Either.isRight (refineTemporalExtent [dateTime, Nothing])
      test "Two items, Just in back -- ok" $
        Assert.assert "" $ Either.isRight (refineTemporalExtent [Nothing, dateTime])


refineTemporalExtent :: Array (Maybe DateTime) -> Either (RefinedError (Array (Maybe DateTime))) TemporalExtent
refineTemporalExtent = refine

dateTime :: Maybe DateTime
dateTime =
  let
    date = canonicalDate <$> toEnum 2021 <*> Just January <*> toEnum 1
    time = Time <$> toEnum 0 <*> toEnum 0 <*> toEnum 0 <*> toEnum 0
  in
     DateTime <$> date <*> time