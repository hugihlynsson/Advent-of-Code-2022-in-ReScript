// A for Rock, B for Paper, and C for Scissors
// 1 for Rock, 2 for Paper, and 3 for Scissors
// 0 if you lost, 3 if the round was a draw, and 6 if you won

// Part 1
// X for Rock, Y for Paper, and Z for Scissors
let start = Performance.now()

let scoreMap = Js.Dict.fromArray([
  ("A X", 4), // draw + rock = 3 + 1
  ("A Y", 8), // win + paper = 6 + 2
  ("A Z", 3), // loss + scissors = 0 + 3
  ("B X", 1), // loss + rock = 0 + 1
  ("B Y", 5), // draw + paper = 3 + 2
  ("B Z", 9), // win + scissors = 6 + 3
  ("C X", 7), // draw + rock = 6 + 1
  ("C Y", 2), // loss + paper = 0 + 2
  ("C Z", 6), // draw + scissors = 3 + 3
])

let totalScore =
  Js.String2.split(Day2Input.data, "\n")->Js.Array2.reduce(
    (sum, game) => sum + Js.Dict.unsafeGet(scoreMap, game), // We know scoreMap is exhaustive so unsafe is fine
    0,
  )

let end = Performance.now()
let executionTime = Js.Float.toFixedWithPrecision(end -. start, ~digits=3)

Js.log(`Part 1, total score: ${Belt.Int.toString(totalScore)}, took ${executionTime} ms`)

// Part 2
// X means loose, Y means draw, and Z means win
let start = Performance.now()

let scoreMap = Js.Dict.fromArray([
  ("A X", 3), // rock and loose (scissors) = 0 + 3
  ("A Y", 4), // rock and draw (rock) = 3 + 1
  ("A Z", 8), // rock and win (paper) = 6 + 2
  ("B X", 1), // paper and loose (rock) = 0 + 1
  ("B Y", 5), // paper and draw (paper) = 3 + 2
  ("B Z", 9), // paper and win (scissors) = 6 + 3
  ("C X", 2), // scissors and loose (paper) = 0 + 2
  ("C Y", 6), // scissors and draw (scissors) = 3 + 3
  ("C Z", 7), // scissors and win (rock) = 6 + 1
])

let totalScore =
  Js.String2.split(Day2Input.data, "\n")->Js.Array2.reduce(
    (sum, game) => sum + Js.Dict.unsafeGet(scoreMap, game), // We know scoreMap is exhaustive so unsafe is fine
    0,
  )

let end = Performance.now()
let executionTime = Js.Float.toFixedWithPrecision(end -. start, ~digits=3)

Js.log(`Part 2, total score: ${Belt.Int.toString(totalScore)}, took ${executionTime} ms`)
