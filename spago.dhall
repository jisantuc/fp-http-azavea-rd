{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support", "affjax", "argonaut" ]
, packages = ./packages.dhall
, sources = [ "src-ps/**/*.purs", "test-ps/**/*.purs" ]
}
