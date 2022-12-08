open Belt

let demoInput = `30373
25512
65332
33549
35390`

let map =
  Js.String2.split(Day8Input.data, "\n")->Array.map(line =>
    Js.String2.split(line, "")->Array.map(Js.Float.fromString)
  )

let getPointValue = (map, (x, y)) => Array.getExn(map, x)->Array.getExn(y)

let isVisible = (map, x, y) => {
  let isLineLower = (map, points, height) =>
    Array.map(points, point => getPointValue(map, point))->Array.every(h => h < height)

  let upPoints = Array.range(0, y - 1)->Array.map(y => (x, y))
  let rightPoints =
    Array.range(x + 1, Array.getExn(map, 0)->Array.length - 1)->Array.map(x => (x, y))
  let downPoints = Array.range(y + 1, Array.length(map) - 1)->Array.map(y => (x, y))
  let leftPoints = Array.range(0, x - 1)->Array.map(x => (x, y))

  let height = getPointValue(map, (x, y))

  isLineLower(map, upPoints, height) ||
  isLineLower(map, rightPoints, height) ||
  isLineLower(map, downPoints, height) ||
  isLineLower(map, leftPoints, height)
}

// Part 1 solution
Array.mapWithIndex(map, (y, row) => Array.mapWithIndex(row, (x, _) => isVisible(map, x, y)))
->Array.concatMany
->Array.keep(isVisible => isVisible)
->Array.length
->Js.log

let getScenicScore = (map, x, y) => {
  let treesVisible = (map, points, height) =>
    Array.map(points, point => getPointValue(map, point))
    ->Array.reduce((0.0, true), ((sum, counting), current) =>
      counting ? (sum +. 1.0, current < height) : (sum, false)
    )
    ->fst

  let upPoints = Array.range(0, y - 1)->Array.map(y => (x, y))->Array.reverse
  let rightPoints =
    Array.range(x + 1, Array.getExn(map, 0)->Array.length - 1)->Array.map(x => (x, y))
  let downPoints = Array.range(y + 1, Array.length(map) - 1)->Array.map(y => (x, y))
  let leftPoints = Array.range(0, x - 1)->Array.map(x => (x, y))->Array.reverse

  let height = getPointValue(map, (x, y))

  treesVisible(map, upPoints, height) *.
  treesVisible(map, rightPoints, height) *.
  treesVisible(map, downPoints, height) *.
  treesVisible(map, leftPoints, height)
}

// Part 2 solution
Array.mapWithIndex(map, (y, row) => Array.mapWithIndex(row, (x, _) => getScenicScore(map, x, y)))
->Array.concatMany
->Array.reduce(0.0, Js.Math.max_float)
->Js.log
