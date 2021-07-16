let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/thumbnail-player/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-scores"
                                     , "debug"
                                     , "either"
                                     , "effect"
                                     , "foldable-traversable"
                                     , "midi"
                                     , "partial"
                                     , "tuples"
                                     ],
  packages = ./thumbnail-player-packages.dhall
}
