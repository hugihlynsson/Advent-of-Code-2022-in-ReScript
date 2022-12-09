open Belt

let exampleInput = `R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2`

type directions = Up | Right | Down | Left

let parseDirection = direction =>
  switch direction {
  | "U" => Up
  | "R" => Right
  | "D" => Down
  | "L" => Left
  | direction => failwith(`Unknown direction: ${direction}`)
  }

let parseMotions = input =>
  Js.String2.split(input, "\n")->Array.map(line =>
    switch Js.String2.split(line, " ") {
    | [direction, distance] => (
        parseDirection(direction),
        Js.Float.fromString(distance)->Belt.Float.toInt,
      )
    | _ => failwith("Unknown line values")
    }
  )

module PointComparator = Id.MakeComparable({
  type t = (int, int)
  let cmp = ((a0, a1), (b0, b1)) =>
    switch Pervasives.compare(a0, b0) {
    | 0 => Pervasives.compare(a1, b1)
    | c => c
    }
})

// We're using a map where 0, 0 is in the lower left corner:
// 3
// 2     O
// 1
// 0 1 2 3
// Where the O would be (3, 2) or x=3, y=2

let applyMotion = ((x, y), (direction, distance)) => {
  switch direction {
  | Up => (x + distance, y)
  | Right => (x, y + distance)
  | Down => (x - distance, y)
  | Left => (x, y - distance)
  }
}

let getDistance = ((tailX, tailY), (headX, headY)) =>
  Js.Math.max_int(Js.Math.abs_int(headX - tailX), Js.Math.abs_int(headY - tailY))

let maxOne = i => Js.Math.max_int(i, -1)->Js.Math.min_int(1)

let moveTowards = ((tailX, tailY), (headX, headY)) => (
  tailX + maxOne(headX - tailX),
  tailY + maxOne(headY - tailY),
)

let moveKnotWithVisited = (from, to) =>
  Array.range(0, getDistance(from, to) - 2)->Array.reduce((from, list{}), (
    (from, visited),
    _step,
  ) => {
    let rightByTo = moveTowards(from, to)
    (rightByTo, List.add(visited, rightByTo))
  })

let getVisitedTailLocations = (input, knots) => {
  // Get the head steps
  let headLocations =
    parseMotions(input)
    ->Array.reduce(list{(0, 0)}, (locations, motion) =>
      List.add(locations, List.headExn(locations)->applyMotion(motion))
    )
    ->List.reverse

  // Move each subsequent knot after the head
  let secondLastKnotLocations = Array.range(0, knots - 3)->Array.reduce(headLocations, (
    prevLocations,
    _,
  ) =>
    List.reduce(prevLocations, list{}, (locations, to) => {
      let lastLocation = List.head(locations)->Option.getWithDefault((0, 0))
      let newLocationNextTo = moveKnotWithVisited(lastLocation, to)->fst
      List.add(locations, newLocationNextTo)
    })->List.reverse
  )

  // Finally capture the visited locations of the tail
  let visited = Set.make(~id=module(PointComparator))->Set.add((0, 0))

  List.reduce(secondLastKnotLocations, (visited, (0, 0)), ((visited, from), to) => {
    let (rightByTo, newVisited) = moveKnotWithVisited(from, to)
    let visited = Set.mergeMany(visited, List.toArray(newVisited))
    (visited, rightByTo)
  })->fst
}

getVisitedTailLocations(exampleInput, 2)->Set.size->Js.log2("example 1") // As expected!
getVisitedTailLocations(Day9Input.data, 2)->Set.size->Js.log2("part 1") // ⭐️

let part2ExampleInput = `R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20`

getVisitedTailLocations(part2ExampleInput, 10)->Set.size->Js.log2("example 2") // As expected!
getVisitedTailLocations(Day9Input.data, 10)->Set.size->Js.log2("part 2") // ⛔️ 🤷
