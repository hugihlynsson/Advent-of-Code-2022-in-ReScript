let exampleInput = `2-4,6-8
2-3,4-5
5-7,7-9
6-6,4-6
2-8,3-7
2-6,4-8`

let parseSection = section =>
  switch Js.String2.split(section, "-") {
  | [a, b] => (Js.Float.fromString(a), Js.Float.fromString(b))
  | _ => failwith("Unecpected section")
  }

let parseLine = line => {
  let sections = Js.String2.split(line, ",")
  switch sections {
  | [a, b] => (parseSection(a), parseSection(b))
  | _ => failwith("Unexpected section")
  }
}

// Part 1
let sectionIsSubset = ((a1, a2), (b1, b2)) => a1 >= b1 && a2 <= b2

Js.String2.split(Day4Input.data, "\n")
->Js.Array2.map(parseLine)
->Belt.Array.keep(((a, b)) => sectionIsSubset(a, b) || sectionIsSubset(b, a))
->Js.Array2.length
->Js.log

// Part 2
let isOnLine = ((a1, a2), b) => b >= a1 && b <= a2
let sectionsIntersect = (a, (b1, b2)) => isOnLine(a, b1) || isOnLine(a, b2)

Js.String2.split(Day4Input.data, "\n")
->Js.Array2.map(parseLine)
->Belt.Array.keep(((a, b)) => sectionsIntersect(a, b) || sectionsIntersect(b, a))
->Js.Array2.length
->Js.log
