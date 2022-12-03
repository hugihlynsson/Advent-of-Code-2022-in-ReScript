let getPriority = char =>
  Js.String2.indexOf("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ", char) + 1

// Part 1
let part1Result =
  Js.String2.split(Day3Input.data, "\n")
  ->Js.Array2.map(backpack => {
    let items = Js.String2.split(backpack, "")
    let itemCount = Js.Array2.length(items)
    let left = Js.Array2.slice(items, ~start=0, ~end_=itemCount / 2)
    let right = Js.Array.slice(items, ~start=itemCount / 2, ~end_=itemCount)
    (left, right)
  })
  ->Js.Array2.map(((left, right)) => {
    let leftSet = Belt.Set.String.fromArray(left)
    let rightSet = Belt.Set.String.fromArray(right)
    Belt.Set.String.intersect(leftSet, rightSet)->Belt.Set.String.toArray
  })
  ->Js.Array2.concatMany([], _)
  ->Js.Array2.map(getPriority)
  ->Js.Array2.reduce((sum, item) => sum + item, 0)

Js.log(part1Result)

// Part 2
let matchEveryThreeLines = %re("/(?:^.*$\n?){1,3}/mg")

let part2Result =
  Js.String2.match_(Day3Input.data, matchEveryThreeLines) 
  ->Belt.Option.getWithDefault([])
  ->Belt.Array.keepMap(a => a)
  ->Js.Array2.map(group => {
    let (a, b, c) = switch Js.String2.split(group, "\n") {
    | [a, b, c] => (a, b, c) // The last match doesn't have an ending newline
    | [a, b, c, _] => (a, b, c) // All other matches have an empty last match
    | _ => failwith(`Unexpected group`) // Won't happen
    }

    Belt.Set.String.intersect(
      Js.String2.split(a, "")->Belt.Set.String.fromArray,
      Js.String2.split(b, "")->Belt.Set.String.fromArray,
    )
    ->Belt.Set.String.intersect(Js.String2.split(c, "")->Belt.Set.String.fromArray)
    ->Belt.Set.String.toArray
  })
  ->Js.Array2.concatMany([], _)
  ->Js.Array2.map(getPriority)
  ->Js.Array2.reduce((sum, item) => sum + item, 0)

Js.log(part2Result)