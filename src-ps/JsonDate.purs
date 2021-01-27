module JsonDate where

import Data.DateTime (DateTime)

newtype JsonDate = JsonDate DateTime

unJsonDate :: JsonDate -> DateTime
unJsonDate (JsonDate dt) = dt

