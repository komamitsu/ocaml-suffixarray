type t = (int * string) list

let create text =
  let len = String.length text in
  let strip str =
    let re = Str.regexp "^\\([0-9a-zA-Z]+\\)" in
    ignore (Str.search_forward re str 0);
    Str.matched_string str
  in
  let rec loop i result =
    if i >= len then result
    else 
      try
        let part = strip (String.sub text i (len - i)) in
        loop (i + 1) ((i, part) :: result)
      with Not_found -> loop (i + 1) result
  in
  let parts = loop 0 [] in
  List.fast_sort (fun a b -> String.compare (snd a) (snd b)) parts

let strstr text str =
  let textlen = String.length text in
  let strlen = String.length str in
  if strlen > textlen then false
  else 
    let rec loop i =
      if i >= strlen then true
      else 
        if text.[i] = str.[i] then loop (i + 1) else false
    in
    loop 0

let find (sa:t) str =
  let sa_size = List.length sa in
  let rec loop first last =
    let center_pos = (first + last) / 2 in
    let pivot_idx, pivot_str = List.nth sa center_pos in
    if strstr pivot_str str then 
      let rec check_neighbor dir i findlist =
        if i < 0 or i >= sa_size then findlist
        else 
          let the_pos, the_str = List.nth sa i in
          if strstr the_str str then 
            check_neighbor dir (i + dir) (the_pos::findlist)
          else
            findlist
      in
      let pre_findlist = check_neighbor (-1) (center_pos - 1) [] in
      let post_findlist = check_neighbor 1 (center_pos + 1) [] in
      pivot_idx::pre_findlist@post_findlist
    else
      if first = last then []
      else 
        let next_first, next_last = 
          if pivot_str > str then (first, (center_pos - 1))
          else ((center_pos + 1), last) in
        loop next_first next_last
  in
  loop 0 (sa_size - 1)
