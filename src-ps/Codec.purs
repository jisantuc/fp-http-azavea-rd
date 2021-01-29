module Codec where

import Data.Either (Either(..))

leftMap :: forall e a b. (e -> b) -> Either e a -> Either b a
leftMap f (Right r) = Right r

leftMap f (Left e) = Left (f e)
