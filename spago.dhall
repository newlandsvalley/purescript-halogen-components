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
  , "prelude"
  , "soundfonts"
  , "transformers"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
