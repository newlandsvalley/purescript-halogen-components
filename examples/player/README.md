player component example
========================

This is a Halogen component that is a player for a Melody (i.e. a melody composed of a succession of phrases of MidiNote). purescript-soundfonts provides the basic means of playing an individual phrase. However, if we were to re-render after every individual note, the playback would not be paced properly. Instead we group notes into phrases which are the units of interruptibility. This means the buttons are slightly unresponsive because they must wait for the phrase to end before they take effect.

The player materialises the melody only when the play button is first pressed. The idea is to allow the player to be rendered even though the user may not wish immediately to play the melody.

The player defines a typeclass Playable which allows any music source to be played if an instance can be defined. The player itself implements an ABC instance.

to build
--------

     npm run build

and then navigate using a browser to dist/index.html
