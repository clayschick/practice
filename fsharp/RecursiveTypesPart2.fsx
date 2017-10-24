type FileSystemItem = 
   | File of File
   | Directory of Directory
and File = {name: string; fileSize: int}
and Directory = {name: string; dirSize: int; subItems: FileSystemItem list}

let cataFS fFile fDir item: 'r =
   let recurse = cataFS fFile fDir
   match item with
   | File file -> 
      fFile file
   | Directory dir
      