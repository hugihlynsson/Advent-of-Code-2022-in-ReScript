let sum = (sum, item) => sum +. item

// Part 1:
let start = Performance.now()

let largest =
  Js.String2.split(Day1Input.data, "\n\n")
  ->Belt.Array.map(elfData =>
    Js.String2.split(elfData, "\n")
    ->Belt.Array.map(Js.Float.fromString)
    ->Belt.Array.reduce(0.0, sum)
  )
  ->Belt.Array.reduce(0.0, Js.Math.max_float)

let end = Performance.now()
let executionTime = Js.Float.toFixedWithPrecision(end -. start, ~digits=3)
Js.log(`Part 1, largest: ${Belt.Float.toString(largest)}, took ${executionTime} ms`)

// Part 2:
let start = Performance.now()

let reversedNumberSorter = (a, b) =>
  switch (a, b) {
  | (a, b) if a > b => -1
  | (a, b) if b > a => 1
  | _ => 0
  }

let top3sum =
  Js.String2.split(Day1Input.data, "\n\n")
  ->Belt.Array.map(elfData =>
    Js.String2.split(elfData, "\n")
    ->Belt.Array.map(Js.Float.fromString)
    ->Belt.Array.reduce(0.0, sum)
  )
  ->Js.Array2.sortInPlaceWith(reversedNumberSorter)
  ->Js.Array2.slice(~start=0, ~end_=3)
  ->Belt.Array.reduce(0.0, sum)

let end = Performance.now()

let executionTime = Js.Float.toFixedWithPrecision(end -. start, ~digits=3)
Js.log(`Part 2, top 3 sum: ${Belt.Float.toString(top3sum)}, took ${executionTime} ms`)
