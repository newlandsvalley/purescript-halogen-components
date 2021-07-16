let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/player/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "abc-parser"
                                     , "abc-melody"
                                     , "effect"
                                     , "either"
                                     , "midi"
                                     ],
  packages = ./player-packages.dhall
}
