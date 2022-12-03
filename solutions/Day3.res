// Part 1

// Split the input by lines
let backpacks = Js.String2.split(Day3Input.data, "\n")
// Find the amount of the items (line length) and split it in the middle
let backpacksByCompartments = Js.Array2.map(backpacks, backpack => {
  let items = Js.String2.split(backpack, "")
  let itemCount = Js.Array2.length(items)
  let left = Js.Array2.slice(items, ~start=0, ~end_=itemCount / 2)
  let right = Js.Array.slice(items, ~start=itemCount / 2, ~end_=itemCount)
  (left, right)
})
// Create a set of each side items and return the intersecting items
let compartmentIntersections = Js.Array2.map(backpacksByCompartments, ((left, right)) => {
  let leftSet = Belt.Set.String.fromArray(left)
  let rightSet = Belt.Set.String.fromArray(right)
  Belt.Set.String.intersect(leftSet, rightSet)->Belt.Set.String.toArray
})
// Flatten the list to a long list of intersections
let intersections = Js.Array2.concatMany([], compartmentIntersections)
// Calculate the priority of each intersection
let getPriority = char =>
  Js.String2.indexOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", char) + 1
let priorities = Js.Array2.map(intersections, getPriority)
// Get the solution by reducing the list to its sum
let result = Js.Array2.reduce(priorities, (sum, item) => sum + item, 0)

Js.log(result)
