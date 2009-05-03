open Printf

type t = (int * string) list

let create text =
  let len = String.length text in
  let rec loop i result =
    if i >= len then result
    else 
      let part = String.sub text i (len - i) in
      loop (i + 1) ((i, part) :: result)
  in
  let parts = loop 0 [] in
  List.sort (fun a b -> String.compare (snd a) (snd b)) parts

let strstr text str =
  let textlen = String.length text in
  let strlen = String.length str in
  if strlen > textlen then false
  else 
    let rec loop i =
      if i >= strlen then true
      else 
        if text.[i] == str.[i] then loop (i + 1) else false
    in
    loop 0

let find sa str =
  let sa_size = List.length sa in
  let rec loop first last =
    let center_pos = (first + last) / 2 in
    let pivot = snd (List.nth sa center_pos) in
    if strstr pivot str then fst (List.nth sa center_pos)
    else
      if first = last then raise Not_found
      else if pivot > str then loop first (center_pos - 1)
      else loop (center_pos + 1) last
  in
  loop 0 (sa_size - 1)

let _ =
  assert ((strstr "abcdefg" "ab") = true);
  assert ((strstr "abcdefg" "abcdefg") = true);
  assert ((strstr "abcdefg" "cde") = false);
  assert ((strstr "abcdefg" "abcdefgh") = false);
  let sa = [
    (2, "adc"); (1, "badc"); (0, "cbadc"); (4, "c"); (3, "dc")
  ] in
  assert ((find sa "dc") == 3);
  let sa = create "March 14, 2003
The CML home page has moved to a virtual webhost. The new URL is
http://cml.cs.uchicago.edu.
March 10, 2003
The CML Reference manual is back online at
people.cs.uchicago.edu/~jhr/cml/pages/refman.html.
October 9, 2002
The CML web pages have relocated to the University of Chicago (as has their
author).
June 20, 2001
CML has been ported to work with SML/NJ Version 110.33; see the CHANGES file
for details. " in
  printf "<<<%d>>>\n" (find sa "110.33")

