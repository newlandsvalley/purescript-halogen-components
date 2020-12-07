let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/thumbnail-player/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "console"
                                     , "debug"
                                     , "effect"
                                     ],
  packages = ./thumbnail-player-packages.dhall
}
