let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/fileio/src/**/*.purs" ],
  dependencies = conf.dependencies # [ "console", "halogen-subscriptions", "media-types" ]
}
