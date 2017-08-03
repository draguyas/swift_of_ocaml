open Types
open Typedtree
   
let rec get_type type_expr list=
  match type_expr.desc with
    Tconstr (path,_,_) -> (match path with
                             Path.Pident i -> i.Ident.name::list
                           | _ -> [])
  | Tarrow (_, te1, te2, _) ->
     (get_type te1 [])@(get_type te2 [])
                      
  | Tlink link -> get_type link []
                
  | _ -> []

let rec get_label_from_cases cases list=
  match cases with
    case::l -> (match case.c_lhs.pat_desc with
                  Tpat_var (ident,_) ->
                  ident.Ident.name::(get_label_from_cases l list)
                | _ -> [])
  |[] -> []
