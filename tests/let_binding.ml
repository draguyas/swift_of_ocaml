let rec f l =
  match l with
    [] -> []
  | e::l -> (e+3)::(f l)
