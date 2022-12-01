let sum = (sum, item) => sum +. item
let largest = (largest, item) => item > largest ? item : largest
let reversedNumberSorter = (a, b) =>
  switch (a, b) {
  | (a, b) if a > b => -1
  | (a, b) if b > a => 1
  | _ => 0
  }

let start = Performance.now()

let caloriesPerElf =
  Js.String2.split(Day1Input.data, "\n\n")->Belt.Array.map(elfData =>
    Js.String2.split(elfData, "\n")
    ->Belt.Array.map(Js.Float.fromString)
    ->Belt.Array.reduce(0.0, sum)
  )

let top3sum =
  Js.Array2.sortInPlaceWith(caloriesPerElf, reversedNumberSorter)
  ->Js.Array2.slice(~start=0, ~end_=3)
  ->Belt.Array.reduce(0.0, sum)

let end = Performance.now()

Js.log3("Execution time:", Js.Float.toFixedWithPrecision(end -. start, ~digits=3), "ms")

let largest = Belt.Array.reduce(caloriesPerElf, 0.0, largest)

Js.log2("Part 1, largest:", largest)
Js.log2("Part 2, top 3 sum:", top3sum)


