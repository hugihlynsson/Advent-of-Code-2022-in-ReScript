open Belt

let exampleInput = `$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k`

// Create a recursive type for a folder. It should hold an array of sub-folders and an array of files. We need the names of the folders but just the size of the files
type rec folder = {
  parent: option<folder>,
  subFolders: Js.Dict.t<folder>,
  files: array<float>,
}

let rec walkToTop = folder =>
  switch folder.parent {
  | Some(parent) => walkToTop(parent)
  | None => folder
  }

type lines = Open(string) | List | Back | File(float) | Folder(string)

// Parse the input as command variants with the data that we need
let commands = Js.String2.split(Day7Input.data, "\n")->Array.map(line => {
  let splittedLine = Js.String2.split(line, " ")
  switch (splittedLine[0], splittedLine[1], splittedLine[2]) {
  | (Some("$"), Some("cd"), Some("..")) => Back
  | (Some("$"), Some("cd"), Some(folderName)) => Open(folderName)
  | (Some("$"), Some("ls"), _) => List
  | (Some("dir"), Some(folderName), _) => Folder(folderName)
  | (Some(fileSize), Some(_fileName), _) => File(Js.Float.fromString(fileSize))
  | _ => failwith("Failed to parse line")
  }
})

// Construct a file tree by reducing the commands, starting from a root node
let root =
  Js.Array2.sliceFrom(commands, 1) // Remove the first ("cd /") command as we're starting from there
  ->Array.reduce({parent: None, files: [], subFolders: Js.Dict.empty()}, (folder, command) =>
    switch command {
    | Open(subFolderName) => Js.Dict.get(folder.subFolders, subFolderName)->Option.getExn
    | List => folder // This is a no-op as the items are captured in the File and Folder variants
    | Back => Option.getExn(folder.parent)
    | File(size) =>
      let _ = Js.Array2.push(folder.files, size)
      folder
    | Folder(name) =>
      Js.Dict.set(
        folder.subFolders,
        name,
        {parent: Some(folder), subFolders: Js.Dict.empty(), files: []},
      )
      folder
    }
  )
  ->walkToTop

let sum = (sum, item) => sum +. item

let rec getFolderSizes = (folder): array<float> => {
  let subFolderSizes =
    Js.Dict.values(folder.subFolders)
    ->Array.map(folder => getFolderSizes(folder))
    ->Js.Array.concatMany([])

  let currentTotalSize =
    Array.reduce(folder.files, 0.0, sum) +. Array.reduce(subFolderSizes, 0.0, sum)

  Js.Array.concat(subFolderSizes, [currentTotalSize])
}

// Part 1
getFolderSizes(root)->Js.log
getFolderSizes(root)->Array.keep(size => size <= 100000.0)->Array.reduce(0.0, sum)->Js.log
