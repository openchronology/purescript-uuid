{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "uuid"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-generic"
  , "psci-support"
  , "sized-vectors"
  , "spec"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
