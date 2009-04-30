type t = (int * string) list

let create str =
  let len = String.length str in
  let rec loop i result =
    if i >= len then result
    else (
      let part = String.sub str i (len - i) in
      loop (i + 1) ((i, part) :: result)
    ) in
  let parts = loop 0 [] in
  List.sort (fun a b -> String.compare (snd a) (snd b)) parts
