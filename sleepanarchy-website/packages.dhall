{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.9-20230629/packages.dhall
        sha256:f91d36c7e4793fe4d7e042c57fef362ff3f9e9ba88454cd38686701e30bf545a

in  upstream
  with markdown-it =
    { dependencies =
      [ "effect"
      , "prelude"
      , "psci-support"
      , "foldable-traversable"
      , "foreign"
      , "tuples"
      , "jest"
      , "node-process"
      , "options"
      ]
    , repo = "https://github.com/nonbili/purescript-markdown-it.git"
    , version = "f6e8ee91298f2fc13c4277e75a19e0538de5f7a2"
    }
  with jest =
    { dependencies = [ "effect", "aff", "aff-promise" ]
    , repo = "https://github.com/nonbili/purescript-jest.git"
    , version = "v1.0.0"
    }
  with html-parser-halogen =
    { dependencies =
      [ "arrays"
      , "control"
      , "dom-indexed"
      , "foldable-traversable"
      , "effect"
      , "halogen"
      , "maybe"
      , "prelude"
      , "psci-support"
      , "jest"
      ]
    , repo = "https://github.com/rnons/purescript-html-parser-halogen.git"
    , version = "035a51d02ba9f8b70c3ffd9fe31a3f5bed19941c"
    }
