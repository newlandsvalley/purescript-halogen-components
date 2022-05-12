"use strict";

export function resetInputValue(componentId) {
    return function () {
      var fileInput = document.getElementById(componentId);
      fileInput.value = "";
    }
  }
