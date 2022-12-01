// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Caml_obj = require("rescript/lib/js/caml_obj.js");
var Day1Input = require("./Day1Input.bs.js");
var Belt_Array = require("rescript/lib/js/belt_Array.js");

function sum(sum$1, item) {
  return sum$1 + item;
}

function largest(largest$1, item) {
  if (Caml_obj.greaterthan(item, largest$1)) {
    return item;
  } else {
    return largest$1;
  }
}

function reversedNumberSorter(a, b) {
  if (Caml_obj.greaterthan(a, b)) {
    return -1;
  } else if (Caml_obj.greaterthan(b, a)) {
    return 1;
  } else {
    return 0;
  }
}

var start = performance.now();

var elfCalories = Belt_Array.map(Day1Input.data.split("\n\n"), (function (elfData) {
        return Belt_Array.reduce(Belt_Array.map(elfData.split("\n"), (function (prim) {
                          return Number(prim);
                        })), 0.0, sum);
      }));

var top3sum = Belt_Array.reduce(elfCalories.sort(reversedNumberSorter).slice(0, 3), 0.0, sum);

var end = performance.now();

console.log("Execution time:", (end - start).toFixed(3), "ms");

console.log("Top 3 sum", top3sum);

var largest$1 = Belt_Array.reduce(elfCalories, 0.0, largest);

console.log("Largest", largest$1);

exports.sum = sum;
exports.reversedNumberSorter = reversedNumberSorter;
exports.start = start;
exports.elfCalories = elfCalories;
exports.top3sum = top3sum;
exports.end = end;
exports.largest = largest$1;
/* start Not a pure module */
