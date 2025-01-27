halogen-components
==================

This module consists of a heterogeneous group of UI components, chosen because they are of use in building various music player or editor applications.  Currently we have:

  * Soundfont melody player (with UI)
  * Thumbnail melody player (no UI)  
  * File input select
  * Multiple select dropdown
  * Trivial button (deprecated)

The workspace contains libraries which are required to support every example.  If you only need a subset, the dependencies may lessen.  The thumbnal player requires all of them, the player requires all but abc-scores.  Others are simpler.  See the individual spago.yaml for each example.

to build
--------

     npm run build build

to build the examples:
----------------------

     npm run player 

     npm run thumbnail-player

     npm run button

     npm run fileio

     npm run multiple-select

To run an example
-----------------

Navigate using a browser to the _index.html_ file in the appropriate _dist_ directory (e.g. examples/button/dist).
