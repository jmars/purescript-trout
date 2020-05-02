{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "trout"
, repo = "https://github.com/jmars/purescript-trout.git"
, version = "headers"
, dependencies =
  [ "argonaut"
  , "media-types"
  , "prelude"
  , "smolder"
  , "spec"
  , "spec-discovery"
  , "uri"
  ]
, packages = ./packages.dhall
}
