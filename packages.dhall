let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.14.4-20210826/packages.dhall sha256:eee0765aa98e0da8fc414768870ad588e7cada060f9f7c23c37385c169f74d9f

in  upstream
  with sequences =
    { dependencies =
      [ "arrays"
      , "assert"
      , "console"
      , "effect"
      , "lazy"
      , "maybe"
      , "newtype"
      , "nonempty"
      , "partial"
      , "prelude"
      , "profunctor"
      , "psci-support"
      , "tuples"
      , "unfoldable"
      , "unsafe-coerce"
      ]
    , repo = "https://github.com/hdgarrood/purescript-sequences.git"
    , version = "1f1d828ef30070569c812d0af23eb7253bb1e990"
    }
  with grid-reactors =
    { dependencies =
      [ "aff"
      , "arrays"
      , "canvas-action"
      , "colors"
      , "console"
      , "effect"
      , "exceptions"
      , "foldable-traversable"
      , "free"
      , "halogen"
      , "halogen-hooks"
      , "halogen-subscriptions"
      , "heterogeneous"
      , "integers"
      , "maybe"
      , "partial"
      , "prelude"
      , "psci-support"
      , "random"
      , "st"
      , "tailrec"
      , "transformers"
      , "tuples"
      , "web-events"
      , "web-html"
      , "web-uievents"
      , "foldable-traversable"
      , "lists"
      , "random"
      ]
    , repo = "https://github.com/Eugleo/purescript-grid-reactors.git"
    , version = "88dacae0f1d8976fde9b762df9fc1164839526a0"
    }
  with canvas-action =
    { dependencies =
      [ "aff"
      , "arrays"
      , "canvas"
      , "colors"
      , "effect"
      , "either"
      , "exceptions"
      , "foldable-traversable"
      , "math"
      , "maybe"
      , "numbers"
      , "polymorphic-vectors"
      , "prelude"
      , "refs"
      , "run"
      , "transformers"
      , "tuples"
      , "type-equality"
      , "typelevel-prelude"
      , "unsafe-coerce"
      , "web-dom"
      , "web-events"
      , "web-html"
      ]
    , repo = "https://github.com/artemisSystem/purescript-canvas-action.git"
    , version = "43de19ee369d1ff9fe7eff1e583b828809fd9e36"
    }
    with neon = 
    {dependencies =
    [ "arrays"
    , "foldable-traversable"
    , "bifunctors"
    , "control"
    , "prelude"
    , "newtype"
    , "maybe"
    , "monoid"
    , "invariant"
    , "st"
    , "eff"
    , "tailrec"
    , "identity"
    , "tuples"
    , "distributive"
    , "unfoldable"
    , "unsafe-coerce"
    , "console"
    , "either"
    , "enums"
    , "exceptions"
    , "lists"
    , "lazy"
    , "nonempty"
    , "random"
    , "integers"
    , "globals"
    , "strings"
    , "transformers"
    , "proxy"
    , "functions"
    , "aff"
    , "parallel"
    , "refs"
    , "functors"
    , "const"
    , "contravariant"
    , "datetime"
    , "generics"
    , "maps"
    , "catenable-lists"
    , "exists"
    , "free"
    , "nullable"
    , "debug"
    , "profunctor"
    , "sets"
    , "lens"
    , "assert"
    , "folds"
    ]
    , repo = "https://github.com/tfausak/purescript-neon.git"
    , version = "8342de2481f769c095ccf7918285127c18dd38ff"
    }