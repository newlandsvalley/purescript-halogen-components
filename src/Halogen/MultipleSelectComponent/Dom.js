"use strict";

export function resetDefaultSelected() {
    var selectionMenu = document.getElementById("selection-menu");
    selectionMenu.options[0].selected = true;
  }
