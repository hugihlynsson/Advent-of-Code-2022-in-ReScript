let demoInput = `    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2`

// Parse stacks in input into an array of lists. The beginning of the list is the top of the stack
let (stacksInput, procedureInput) = switch Js.String2.split(Day5Input.data, "\n\n") {
| [stacks, procedures] => (stacks, procedures)
| _ => failwith("Failed to parse input")
}

// Split each line into an array of every 4th char, starting at the second
let stacksInputArray =
  Js.String2.split(stacksInput, "\n")
  ->Js.Array2.slice(~start=0, ~end_=-1) // Remove the stack numerations
  ->Js.Array2.map(line =>
    Js.String2.splitByRe(line, %re("/(.{4})/"))->Belt.Array.keepMap(line =>
      Belt.Option.flatMap(line, line => line == "" ? None : Some(Js.String2.charAt(line, 1)))
    )
  )
  ->Belt.Array.reverse

// Warp the matrix so that it becomes an arrays of stacks with the top one first
let stacks =
  Belt.Array.makeBy(Belt.Array.length(stacksInputArray[0]), i =>
    Belt.Array.makeBy(Belt.Array.length(stacksInputArray), j => stacksInputArray[j][i])
  )->Belt.Array.map(line =>
    Belt.Array.keep(line, line => line != " ")->Belt.List.fromArray->Belt.List.reverse
  )

// Parse procedures in input into: from, to and count
type procedure = {
  count: int,
  from: int, // 0 based
  to: int, // 0 based
}

let parseProcedure = line =>
  Js.String2.match_(line, %re("/\d+/g"))->Belt.Option.map(matches =>
    switch matches {
    | [Some(count), Some(from), Some(to)] => {
        count: Belt.Int.fromString(count)->Belt.Option.getExn,
        from: Belt.Int.fromString(from)->Belt.Option.getExn - 1,
        to: Belt.Int.fromString(to)->Belt.Option.getExn - 1,
      }
    | line => failwith(`Unexpected procedure: ${Obj.magic(line)}`)
    }
  )

let procedures = Js.String2.split(procedureInput, "\n")->Belt.Array.keepMap(parseProcedure)

// Apply procedures to the stacks.
let part1Stack = Belt.Array.copy(stacks)
Belt.Array.forEach(procedures, ({count, from, to}) => {
  let _ = Belt.Array.set(
    part1Stack,
    to,
    Belt.Array.getExn(part1Stack, from)
    ->Belt.List.take(count)
    ->Belt.Option.getWithDefault(list{})
    ->Belt.List.reverse
    ->Belt.List.concat(Belt.Array.getExn(part1Stack, to)),
  )
  let _ = Belt.Array.set(
    part1Stack,
    from,
    Belt.Array.getExn(part1Stack, from)->Belt.List.drop(count)->Belt.Option.getWithDefault(list{}),
  )
})

// Reduce the stacks array into a string of the heads of each stack
let part1Solution = Belt.Array.reduce(part1Stack, "", (firsts, stack) =>
  firsts ++ Belt.List.head(stack)->Belt.Option.getWithDefault("")
)

Js.log(part1Solution)

let part2Stack = Belt.Array.copy(stacks)
Belt.Array.forEach(procedures, ({count, from, to}) => {
  let _ = Belt.Array.set(
    part2Stack,
    to,
    part2Stack[from]
    ->Belt.List.take(count)
    ->Belt.Option.getWithDefault(list{})
    ->Belt.List.concat(part2Stack[to]),
  )
  let _ = Belt.Array.set(
    part2Stack,
    from,
    part2Stack[from]->Belt.List.drop(count)->Belt.Option.getWithDefault(list{}),
  )
})

let part2Solution = Belt.Array.reduce(part2Stack, "", (firsts, stack) =>
  firsts ++ Belt.List.head(stack)->Belt.Option.getWithDefault("")
)

Js.log(part2Solution)
