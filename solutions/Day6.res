open Belt

let exampleInput = "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let findIndexOfDistinctChars = (string, n) => {
  let chars = Js.String2.split(string, "")
  Array.range(n, Array.length(chars) - 1)->Array.getBy(index =>
    Array.slice(chars, ~offset=index - n, ~len=n)->Set.String.fromArray->Set.String.size == n
  )
}

// Part 1
findIndexOfDistinctChars(Day6Input.data, 4)->Js.log

// Part 2
findIndexOfDistinctChars(Day6Input.data, 14)->Js.log
