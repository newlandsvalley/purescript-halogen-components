{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-components"
, dependencies =
  [ "aff"
  , "arrays"
  , "css"
  , "datetime"
  , "dom-indexed"
  , "effect"
  , "halogen"
  , "halogen-css"
  , "integers"
  , "js-fileio"
  , "lists"
  , "maybe"
  , "nonempty"
  , "prelude"
  , "soundfonts"
  , "transformers"
  , "web-dom"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
