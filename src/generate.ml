open Typedtree
open Path
open Types
open Path


type env = { func_decl : (string list) Func_table.t;
             have_return : bool
           }
   
exception Not_implemented_yet of string

(* faire une table des binop et des conversions binop caml -> swift*)
          
let rec generate_constant fmt cst =
  let open Asttypes in
  match cst with
  | Const_int i -> Format.fprintf fmt "%d" i
  | Const_char c -> Format.fprintf fmt "\'%c\'" c
  | Const_float f -> Format.fprintf fmt "%s" f
  | Const_int32 i -> Format.fprintf fmt "%ld" i
  | Const_int64 i -> Format.fprintf fmt "%Ld" i
  | Const_nativeint i -> Format.fprintf fmt "%nd" i
                       
  | _ -> raise (Not_implemented_yet "generate_constant")

and generate_binop fmt op_name =
  Format.fprintf fmt "%s" (Binop.(OpMap.find op_name binop_map))
       
and generate_apply fmt expr args =
  match expr.exp_desc with
  | Texp_ident
    (Pdot (Pident {Ident.name="Pervasives"; _}, op_name, _), _, _) ->
     if Binop.is_binop op_name then
       (match args with
          (_, Some exp1)::(_, Some exp2)::[] ->
          
          Format.fprintf fmt "(%a %a %a)"
                         generate_expression exp1.exp_desc
                         generate_binop op_name
                         generate_expression exp2.exp_desc
        | _ -> failwith "Unsupported binop"
       )
     else
       (* Todo : à voir *)
       () 
  | _ -> ()
         
    
and generate_expression fmt exp_desc =
  match exp_desc with
  | Texp_constant cst ->
     generate_constant fmt cst
  | Texp_apply (expr,args) ->
     generate_apply fmt expr args
  | Texp_function (label, cases, partial) ->
     (match cases with
        e::l ->
        (match e.c_lhs.pat_desc with
           Tpat_var (ident,_) ->
           Printf.printf "%s\n" ident.Ident.name;
         | _ -> ())
          (*(match e.c_lhs.pat_type.desc with
             Tlink link -> (match link.desc with
                              Tconstr (path,_,_) ->
                              (match path with
                                 Pident i ->
                                 Printf.printf "%s\n" i.Ident.name
                                |_->())
                            | _ -> Printf.printf "noob\n")
           | _ -> Printf.printf "noob\n")*)
        
        
      | [] -> ())
  | _ -> raise (Not_implemented_yet "generate_expression")
       
let generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
  let ident  =
    match vb_pat.pat_desc with
    | Tpat_var (ident, loc) ->
       ident
    | _ -> raise (Not_implemented_yet "generate_value_binding")
  in
  match vb_expr.exp_desc with
    Texp_function (_,_,p) ->
    Format.fprintf fmt "func %s %a\n"
                   ident.Ident.name
                   generate_expression vb_expr.exp_desc;
    
  | _ ->  Format.fprintf fmt "let %s = %a\n"
                         ident.Ident.name
                         generate_expression vb_expr.exp_desc
        
let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) ->
     
    List.iter (generate_value_binding fmt) val_binds;
    Format.fprintf fmt "\n"

  | Tstr_eval (expr, attributes) ->
     generate_expression fmt expr.exp_desc;
     Format.fprintf fmt "\n"     
     
  | _ -> raise (Not_implemented_yet "generate_structure_item")
    
       
let generate_from_structure fmt structure =
  let {str_items; str_type; str_final_env} = structure in
  
  List.iter (generate_structure_item fmt) str_items;

  (* Flushing the output *)
  Format.fprintf fmt "\n%!";
  ()
