let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/multiple-select/src/**/*.purs" ]
}
