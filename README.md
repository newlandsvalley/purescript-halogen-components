halogen-components
==================

This module consists of a heterogeneous group of UI components, chosen because they are of use in building various music player or editor applications.  Currently we have:

  * Soundfont melody player (with UI)
  * Thumbnail melody player (no UI)  
  * File input select
  * Multiple select dropdown
  * Trivial button (deprecated)

to build
--------

     spago install
     spago build

or

     bower install
     pulp build


to build the player or thumbnail player examples:
-------------------------------------------------

     cd to examples/player or  examples/thumbnail-player as appropriate

     bower install

     npm run build

to build the other examples
---------------------------

(after bower install):

     npm run button

     npm run fileio

     npm run multiple-select

To run an example
-----------------

Navigate using a browser to the _index.html_ file in the appropriate _dist_ directory (e.g. examples/button/dist).
