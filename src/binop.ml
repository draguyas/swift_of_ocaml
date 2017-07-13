module OpMap = Map.Make(String)

let binop_map =
  List.fold_left
    (fun map (k,v) ->
      OpMap.add k v map)
    OpMap.empty
    [
      "+" , "+";
      "+.", "+";
      "-" , "-";
      "-.", "-";
      "*" , "*";
      "*.", "*";
      "/" , "/";
      "/.", "/";
      "==", "==";
      "!=", "!=";
      "=" , "==";
      "<>", "!=";
      "<" , "<";
      ">" , ">";
      "<=", "<=";
      ">=", ">=";
      "&&", "&&";
      "&", "&&";
      "||" , "||";
      "or" , "||";
      "mod", "%";
      "^", "+";
    ]

let is_binop k =
  OpMap.mem k binop_map
