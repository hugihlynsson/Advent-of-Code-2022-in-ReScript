open Belt

let parseInput = input =>
  Js.String2.split(input, "\n")
  ->Array.map(line =>
    switch Js.String2.split(line, " ") {
    | ["addx", value] => [0, Js.Float.fromString(value)->Int.fromFloat]
    | ["noop"] => [0]
    | input =>
      Js.log2("Unknown line", input)
      failwith("Unknown line")
    }
  )
  ->Belt.Array.concatMany

let smallExampleCommands = parseInput(Day10Input.smallExample)
let largeExampleComands = parseInput(Day10Input.bigExample)

let applyValues = values =>
  Array.reduce(values, list{1}, (computed, value) =>
    List.add(computed, List.headExn(computed) + value)
  )
  ->List.reverse
  ->List.toArray

let smallExampleApplied = applyValues(smallExampleCommands)
let largeExampleApplied = applyValues(largeExampleComands)

// Js.log2("Small example commands", smallExampleCommands)
// Js.log2("Small example applied", smallExampleApplied)

Js.log2("Large example commands", largeExampleComands)
Js.log2("Large example applied", largeExampleApplied)

let getSignalsComputations = computations =>
  Array.mapWithIndex(computations, (index, computation) => (computation, index))
  ->Array.keep(((_, index)) => mod(index - 20, 40) == 0)
  ->Array.map(((computation, index)) => {
    Js.log(computation)
    computation * index})

let bigExampleSignals = getSignalsComputations(largeExampleApplied)

Js.log2("bigExampleSignals", bigExampleSignals)

let sum = (a, b) => a + b
let computeTotalSignal = (signals) => Array.reduce(signals, 0, sum)

Js.log2("Total signal", computeTotalSignal(bigExampleSignals))
