let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/multiple-select/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "console"
                                     , "foldable-traversable"
                                     , "halogen-subscriptions"
                                     ]
}
