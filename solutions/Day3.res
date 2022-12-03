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

// Part 2
// Split input by lines
let matchEveryThreeLines = %re("/(?:^.*$\n?){1,3}/mg")
let groups =
  Js.String2.match_(Day3Input.data, matchEveryThreeLines)
  ->Belt.Option.getWithDefault([])
  ->Belt.Array.keepMap(a => a)
  
let groupBadges = Js.Array2.map(groups, group => {
  let (a, b, c) = switch Js.String2.split(group, "\n") {
  | [a, b, c] => (a, b, c) // The last match doesn't have an ending newline
  | [a, b, c, _] => (a, b, c) // All other matches have and empty last match
  | _ => failwith(`Unexpected group`) // Won't happen
  }

  Belt.Set.String.intersect(
    Js.String2.split(a, "")->Belt.Set.String.fromArray,
    Js.String2.split(b, "")->Belt.Set.String.fromArray,
  )
  ->Belt.Set.String.intersect(Js.String2.split(c, "")->Belt.Set.String.fromArray)
  ->Belt.Set.String.toArray
})
let badges = Js.Array2.concatMany([], groupBadges)
let priorities = Js.Array2.map(badges, getPriority)
let sum = Js.Array2.reduce(priorities, (sum, item) => sum + item, 0)

Js.log(sum)
// Group every 3 lines together
// For each group, create a set of each item and find their intersection
// For the intersection, calculate the priority
// Sum the priorities
