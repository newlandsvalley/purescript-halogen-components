let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "examples/button/src/**/*.purs" ]
}
