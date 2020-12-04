{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-components"
, dependencies = [ "console"
                 , "css"
                 , "effect"
                 , "js-fileio"
                 , "halogen"
                 , "halogen-css"
                 , "soundfonts"]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
