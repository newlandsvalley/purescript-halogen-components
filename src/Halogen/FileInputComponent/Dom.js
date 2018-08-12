"use strict";

exports.resetInputValue = function (componentId) {
    return function () {
      var fileInput = document.getElementById(componentId);
      fileInput.value = "";
    }
  };
