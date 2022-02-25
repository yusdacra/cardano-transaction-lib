{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "affjax"
  , "argonaut"
  , "arraybuffer-types"
  , "arrays"
  , "assert"
  , "bifunctors"
  , "bigints"
  , "console"
  , "const"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "gen"
  , "identity"
  , "lattice"
  , "lists"
  , "maybe"
  , "medea"
  , "mote"
  , "newtype"
  , "node-buffer"
  , "node-fs-aff"
  , "node-path"
  , "nonempty"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "properties"
  , "psci-support"
  , "quickcheck"
  , "quickcheck-laws"
  , "rationals"
  , "refs"
  , "spec"
  , "strings"
  , "these"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-prelude"
  , "uint"
  , "undefined"
  , "unordered-collections"
  , "unsafe-coerce"
  , "untagged-union"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs", "examples/**/*.purs" ]
}
