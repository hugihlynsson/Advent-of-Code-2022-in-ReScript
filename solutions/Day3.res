let getPriority = char =>
  Js.String2.indexOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", char) + 1

let sum = (sum, item) => sum + item

// Part 1
let start = Performance.now()

let part1Result =
  Js.String2.split(Day3Input.data, "\n")
  ->Js.Array2.map(backpack => {
    let items = Js.String2.split(backpack, "")
    let itemCount = Js.Array2.length(items)
    let left = Js.Array2.slice(items, ~start=0, ~end_=itemCount / 2)
    let right = Js.Array.slice(items, ~start=itemCount / 2, ~end_=itemCount)
    Belt.Set.String.fromArray(left)
    ->Belt.Set.String.intersect(Belt.Set.String.fromArray(right))
    ->Belt.Set.String.toArray
  })
  ->Js.Array2.concatMany([], _)
  ->Js.Array2.map(getPriority)
  ->Js.Array2.reduce(sum, 0)

let end = Performance.now()
let executionTime = Js.Float.toFixed((end -. start) *. 1000.0)

Js.log(`Part 1: ${Belt.Int.toString(part1Result)}, took ${executionTime} ns`)

// Part 2
let start = Performance.now()

let matchEveryThreeLines = %re("/(?:^.*$\n?){1,3}/mg")

let parseGroup = groupString =>
  // Each group includes three lines
  switch Js.String2.split(groupString, "\n") {
  | [a, b, c] => (a, b, c) // The last match doesn't have an ending newline
  | [a, b, c, _] => (a, b, c) // All other matches have an empty last match
  | _ => failwith(`Unexpected group`) // Shouldn't happen
  }

let part2Result =
  Js.String2.match_(Day3Input.data, matchEveryThreeLines)
  ->Belt.Option.getWithDefault([])
  ->Belt.Array.keepMap(a => a)
  ->Js.Array2.map(groupString => {
    let (a, b, c) = parseGroup(groupString)

    // Compute the intersecting character between the three groups
    Js.String2.split(a, "")
    ->Belt.Set.String.fromArray
    ->Belt.Set.String.intersect(Js.String2.split(b, "")->Belt.Set.String.fromArray)
    ->Belt.Set.String.intersect(Js.String2.split(c, "")->Belt.Set.String.fromArray)
    ->Belt.Set.String.toArray
  })
  ->Js.Array2.concatMany([], _)
  ->Js.Array2.map(getPriority)
  ->Js.Array2.reduce(sum, 0)

let end = Performance.now()
let executionTime = Js.Float.toFixed((end -. start) *. 1000.0)

Js.log(`Part 2: ${Belt.Int.toString(part2Result)}, took ${executionTime} ns`)
