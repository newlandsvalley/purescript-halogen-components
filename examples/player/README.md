Soundfont player component example
==================================

This is a Halogen component that displays a player widget that will play any melody that conforms to the __Playable__ typeclass (representing melodies composed of a succession of phrases of __MidiNote__). The typeclass is defined in [purescript-soundfonts](https://github.com/newlandsvalley/purescript-soundfonts) and instances currently exist for MIDI, ABC and PSoM.  The example player chooses a MIDI instance.

The player materialises the melody only when the play button is first pressed. The idea is to
allow the player to be rendered even though the user may not wish immediately to play the melody.  Playable melodies group the notes into a succession of phrases which are the units of interruptibility. This design represents a compromise between allowing the playback to be paced properly and allowing it to be interrupted without waiting too long for this to happen.

