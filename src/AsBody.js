"use strict";

// module AsBody

exports.asBody = function asBody(html) {
  return function() {
    onload = function() {
      document.body.innerHTML = html;
    };
  };
};
