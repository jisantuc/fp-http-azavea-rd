{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "ps-stac"
, dependencies = [ "console"
                 , "effect"
                 , "psci-support"
                 , "affjax"
                 , "argonaut"
                 , "datetime"
                 , "formatters"
                 , "refined"
                 , "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src-ps/**/*.purs", "test-ps/**/*.purs" ]
}
