{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "player"
, dependencies = [ "abc-parser"
                 , "abc-melody"
                 , "console"
                 , "effect"
                 , "halogen-components"
                 ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
