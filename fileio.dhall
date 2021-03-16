let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/fileio/src/**/*.purs" ]
}
