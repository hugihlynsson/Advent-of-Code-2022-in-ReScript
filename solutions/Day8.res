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

let getPaths = (mapWidth, mapHeight, x, y) => {
  let upPoints = Array.range(0, y - 1)->Array.map(y => (x, y))->Array.reverse
  let rightPoints = Array.range(x + 1, mapWidth - 1)->Array.map(x => (x, y))
  let downPoints = Array.range(y + 1, mapHeight - 1)->Array.map(y => (x, y))
  let leftPoints = Array.range(0, x - 1)->Array.map(x => (x, y))->Array.reverse

  [upPoints, rightPoints, downPoints, leftPoints]
}

let isVisible = (map, x, y) => {
  let isLineLower = (map, points, height) =>
    Array.map(points, point => getPointValue(map, point))->Array.every(h => h < height)

  let paths = getPaths(Array.getExn(map, 0)->Array.length, Array.length(map), x, y)

  let height = getPointValue(map, (x, y))

  Array.some(paths, path => isLineLower(map, path, height))
}

// Part 1 solution
Array.mapWithIndex(map, (y, row) => Array.mapWithIndex(row, (x, _) => isVisible(map, x, y)))
->Array.concatMany
->Array.keep(isVisible => isVisible)
->Array.length
->Js.log

// Part 2
let treesVisible = (points, height) =>
  Array.reduce(points, (0.0, true), ((sum, counting), current) =>
    counting ? (sum +. 1.0, current < height) : (sum, false)
  )->fst

let getScenicScore = (map, x, y) => {
  let height = getPointValue(map, (x, y))

  let paths = getPaths(Array.getExn(map, 0)->Array.length, Array.length(map), x, y)

  Array.map(paths, path => Array.map(path, point => getPointValue(map, point)))
  ->Array.map(path => treesVisible(path, height))
  ->Array.reduce(1.0, (sum, trees) => sum *. trees)
}

// Part 2 solution
Array.mapWithIndex(map, (y, row) => Array.mapWithIndex(row, (x, _) => getScenicScore(map, x, y)))
->Array.concatMany
->Array.reduce(0.0, Js.Math.max_float)
->Js.log
