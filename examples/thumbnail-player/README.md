thumbnail player component example
==================================

This is a Halogen component with no UI that again plays a __Melody__ composed of a succession of phrases of __MidiNote__ (as defined in [purescript-soundfonts](https://github.com/newlandsvalley/purescript-soundfonts)).

The intention is for it to be used to play thumbnail images of the start of tunes (i.e. the first two or three bars only).  It cannot be paused and resumed (as can the full player) but can only be started or stopped by instruction from the parent.

This example uses ABC tunes curtailed by the __thumbnail__ function and then converted to a melody in the normal way.

