{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ps-stac"
, dependencies =
  [ "affjax"
  , "argonaut"
  , "console"
  , "datetime"
  , "effect"
  , "formatters"
  , "psci-support"
  , "quickcheck"
  , "refined"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src-ps/**/*.purs", "test-ps/**/*.purs" ]
}
