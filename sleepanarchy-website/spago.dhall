{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "argonaut"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "datetime-parsing"
  , "effect"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "foreign"
  , "halogen"
  , "html-parser-halogen"
  , "integers"
  , "lists"
  , "markdown-it"
  , "maybe"
  , "media-types"
  , "newtype"
  , "now"
  , "options"
  , "orders"
  , "parsing"
  , "prelude"
  , "profunctor"
  , "routing"
  , "semirings"
  , "strings"
  , "transformers"
  , "tuples"
  , "validation"
  , "web-events"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
