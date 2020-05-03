{ sources = [ "src/**/*.purs" ]
, name = "trout"
, repo = "https://github.com/jmars/purescript-trout.git"
, version = "headers"
, dependencies =
  [ "argonaut"
  , "media-types"
  , "prelude"
  , "spec"
  , "spec-discovery"
  , "uri"
  ]
, packages = ./packages.dhall
}
