{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "thumbnail-player"
, dependencies = [ "abc-parser"
                 , "abc-scores"
                 , "console"
                 , "debug"
                 , "effect"
                 , "halogen-components"
                ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
