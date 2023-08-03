{ name = "halogen-vdom"
, dependencies =
  [ "arrays"
  , "bifunctors"
  , "effect"
  , "foreign"
  , "foreign-object"
  , "functions"
  , "maybe"
  , "newtype"
  , "nullable"
  , "prelude"
  , "record"
  , "refs"
  , "tuples"
  , "unsafe-coerce"
  , "web-dom"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
