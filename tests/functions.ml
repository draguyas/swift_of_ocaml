let f x =
  match x with
    0. -> 0.
  | 1. -> if (x -. 1.) = 0. then 0. else 1.
  | _ -> 42.

let g x =
  if f x = 42. then "life"
  else "noob"
