let f x =
  x + 3

let g x =
  if true then x
  else  x - 1

let concat_string s1 s2 =
  s1^s2

let simple_match x =
  match x with
    0 -> x
   |1 -> x-1
   |_ -> x + 2

let match_if x =
  match x with
    0 -> if x + 1 < 5 then x
         else 3
  | _ -> x
