halogen-components
==================

This module consists of a heterogeneous group of UI components, chosen because they are of use in building various music player or editor applications.  Currently we have:

  * Soundfont melody player (with UI)
  * Thumbnail melody player (no UI)
  * File input select
  * Multiple select dropdown
  * Trivial button

to build
--------

     bower install

     pulp build

to build the examples
---------------------

(after bower install):

     npm run button

     npm run fileio

     npm run multiple-select

     npm run player

To run an example
-----------------

Navigate using a browser to the _index.html_ file in the appropriate _dist_ directory (e.g. examples/button/dist).
