open Typedtree
open Path
open Types
open Asttypes
open Path
   
exception Not_implemented_yet of string
                               
let rec gen_func fmt l cpt=
  match l with
    [] -> ()
  | e::l ->
     (match e.c_lhs.pat_desc with
        Tpat_var (n,_) ->
        if cpt >= 1 then
          (Format.fprintf fmt "_ %s : %s, "
                          n.Ident.name
                          (get_type e.c_lhs.pat_type);
           gen_func fmt l cpt)
        else 
          Format.fprintf fmt "_ %s : %s "
                         n.Ident.name
                         (get_type e.c_lhs.pat_type);
        (gen_func fmt l cpt);
        
        (match e.c_rhs.exp_desc with
           Texp_function (_,l,_) ->
           gen_func fmt l (cpt-1)
           
         | Texp_ident (path,_,_) ->
            Format.fprintf fmt ") -> %s {\nreturn "
                           (get_type e.c_rhs.exp_type);
            (match path with
               Pident ident -> gen_ident fmt path;                               
             | _ -> ()
            )
           
         | Texp_constant cst ->
            Format.fprintf fmt ") -> %s {\nreturn "
                           (get_type e.c_rhs.exp_type);
            generate_constant fmt cst
           
         | Texp_apply (expr,args) ->
            Format.fprintf fmt ") -> %s {\nreturn "
                           (get_type e.c_rhs.exp_type);
            generate_apply fmt expr args
            
         | Texp_ifthenelse (cond,thens, Some elses) ->
            Format.fprintf fmt ") -> %s {\n"
                           (get_type e.c_rhs.exp_type);
            Format.fprintf fmt "if %a {\n %a%a\n}else{\n %a%a\n}"
                           generate_expression cond
                           flag_return thens.exp_desc
                           generate_expression thens
                           flag_return elses.exp_desc
                           generate_expression elses;

         | Texp_let (_,val_binds,expr) ->
            Format.fprintf fmt "%a%a"
                           flag_return expr.exp_desc
                           generate_expression expr
        
         | Texp_match (expr,c1,c2,_) ->
            Format.fprintf fmt ") -> %s {\n"
                           (get_type e.c_rhs.exp_type);
            Format.fprintf fmt "switch %a {\n%a"
                           generate_expression expr
                           generate_match c1;
            
         | Texp_sequence (expr1,expr2) ->
            Format.fprintf fmt ") -> %s {\n"
                           (get_type e.c_rhs.exp_type);
            generate_expression fmt expr1;
            Format.fprintf fmt "\n";
            flag_return fmt expr2.exp_desc;
            generate_expression fmt expr2;
            
         | _ -> Format.fprintf fmt "error gen_func")
      | _ -> ());

and generate_match_pattern fmt c_lhs =
  match c_lhs.pat_desc with
    Tpat_constant cst -> Format.fprintf fmt "case %a"
                                        generate_constant cst
  | Tpat_any  -> Format.fprintf fmt "default"
  | Tpat_construct (_,const_desc,pat_list)->
      Format.fprintf fmt "case ";
     (match const_desc.cstr_name with
        "[]" -> Format.fprintf fmt "List.Vide"
      | _ -> gen_pats fmt pat_list)
        
  | _ -> ()

and gen_pats fmt list =
  match list with
    [] -> ()
  | e1::e2::[] -> (match e1.pat_desc,e2.pat_desc with
                     Tpat_var(id1,_),Tpat_var(id2,_) ->
                     Format.fprintf fmt "List.Cons(let e,let l)"
                   | _ -> ())
  | _ -> ()
  
and generate_match fmt cases =
  List.iter (fun x ->
      Format.fprintf fmt "%a : %a%a\n"
                     generate_match_pattern x.c_lhs
                     flag_return x.c_rhs.exp_desc
                     generate_expression x.c_rhs;
    ) cases;
  Format.fprintf fmt "}"
     
and flag_return fmt expr_desc =
  match expr_desc with
    Texp_apply _ -> Format.fprintf fmt "return "
  | Texp_constant _ -> Format.fprintf fmt "return "
  | Texp_ident _ -> Format.fprintf fmt "return "
  | Texp_construct _ -> Format.fprintf fmt "return "
  | _ -> ()
     
and cpt_var l cpt=
  match l with
    [] -> cpt
  | e::l -> (match e.c_rhs.exp_desc with
               Texp_function (_,l,_) -> cpt_var l (cpt + 1)
             | _ -> cpt)
          
and get_type pat_type =
  (*Printtyp.type_expr Format.std_formatter pat_type;*)
  match pat_type.desc with
  | Tvar (Some s) -> s
  | Tlink type_expr -> (get_type type_expr)
  | Tarrow (l,lex,rex,c) -> "arrow"
  | Tunivar _ -> "univar"
  | Ttuple _ -> "tuple"
  | Tconstr (path,l,_) -> (match path with
                             Pident ident ->
                             transform_type ident.Ident.name
                            |_ -> "")^(match l with
                                         e::l -> "<"^get_type e^">"
                                        | _ -> "")
  | Tpoly _ -> "poly"
  | Tobject _ -> "object"
  | Tfield _ -> "field"
  | Tnil -> "nil"
  | Tsubst _ -> "subst"         
  | Tvariant _ -> "variant"
  | Tpackage _ -> "package"
  | Tvar None -> "T"^string_of_int pat_type.id
        
and transform_type types =
  match types with
    "int" -> "Int"
  | "string" -> "String"
  | "float" -> "Float"
  | "list" -> "List"
  | _ -> types
       
and gen_ident fmt path =
  match path with
    Pident ident -> Format.fprintf fmt "%s" ident.Ident.name
  | Pdot (_,s,i) -> Format.fprintf fmt "%s" s
  | _ -> ()
       
and generate_constant fmt cst =
  let open Asttypes in
  match cst with
  | Const_int i -> Format.fprintf fmt "%d" i
  | Const_char c -> Format.fprintf fmt "\'%c\'" c
  | Const_float f -> Format.fprintf fmt "%s0" f
  | Const_int32 i -> Format.fprintf fmt "%ld" i
  | Const_int64 i -> Format.fprintf fmt "%Ld" i
  | Const_nativeint i -> Format.fprintf fmt "%nd" i
  | Const_string (s,_) -> Format.fprintf fmt "\"%s\"" s
       
and generate_binop fmt op_name =
  Format.fprintf fmt "%s" (Binop.(OpMap.find op_name binop_map))
  
and gen_args fmt args =
  match args with
    [] -> ()
  | e::[] -> (match e with
                (_,Some exp) ->
                Format.fprintf fmt "%a"
                               generate_expression
                               exp;
              | _ -> ())
           
  | e::l -> (match e with
               (_,Some exp) ->
               Format.fprintf fmt "%a,"
                              generate_expression
                              exp;
               gen_args fmt l;
             | _ -> ())
          
and generate_apply fmt expr args =
  match expr.exp_desc with
  | Texp_ident
    (Pdot (Pident {Ident.name="Pervasives"; _}, op_name, _), _, _) ->
     if Binop.is_binop op_name then
       (match args with
          (_, Some exp1)::(_, Some exp2)::[] ->
          Format.fprintf fmt "(%a %a %a)"
                         generate_expression exp1
                         generate_binop op_name
                         generate_expression exp2
        | _ -> failwith "Unsupported binop"
       )
     else
       ()
    
  | Texp_ident (path,_,_) ->
     (match path with
        Pident ident -> gen_ident fmt path;
                        Format.fprintf fmt "(";
                        gen_args fmt args;
                        Format.fprintf fmt ")";
                        
      | _ -> ()
     )
 
  | _ -> Printf.printf "autre\n"


and generate_construct fmt constr_desc pat_list =
  match constr_desc.cstr_name with
    "()" -> ()
  | "[]" -> Format.fprintf fmt "List.Vide"
  | "::" -> Format.fprintf fmt "cons(";
            Printf.printf "%d\n" (List.length pat_list);
            let rec aux fmt pat_list =
              match pat_list with
                [] -> ()
              | e::[] -> generate_expression fmt e;
                         Format.fprintf fmt ")"
              | e::l -> generate_expression fmt e;
                        Format.fprintf fmt ", ";
                        aux fmt l
            in
            aux fmt pat_list
  | s -> Format.fprintf fmt "%s" s
       
and generate_expression fmt expr =
  match expr.exp_desc with
  | Texp_constant cst ->
     generate_constant fmt cst
    
  | Texp_apply (expr,args) ->
     generate_apply fmt expr args
    
  | Texp_function (_,l,_) ->
     let cpt = cpt_var l 0 in
     Format.fprintf fmt "(";
     gen_func fmt l cpt;
     Format.fprintf fmt "\n}\n"
     
  | Texp_ident (path,_,_) ->
     gen_ident fmt path
    
  | Texp_let (_,value_binding,expr) ->
     List.iter (fun x -> generate_value_binding fmt x) value_binding;
     generate_expression fmt expr
     
  | Texp_ifthenelse (cond,thens, Some elses) ->
     Format.fprintf fmt "if %a{\n %a%a\n}else{\n %a%a}\n"
                    generate_expression cond
                    flag_return thens.exp_desc
                    generate_expression thens
                    flag_return elses.exp_desc
                    generate_expression elses;
     
  | Texp_construct (_,constr_desc,pat_list) ->
     generate_construct fmt constr_desc pat_list

  | Texp_sequence (expr1,expr2) ->
     generate_expression fmt expr1;
     Format.fprintf fmt "\n";
     generate_expression fmt expr2;
     
  | _ -> raise (Not_implemented_yet "generate_expression")
       
and generate_value_binding fmt value_binding =
  let {vb_pat; vb_expr; vb_attributes; vb_loc} = value_binding in
  
  let ident  =
    match value_binding.vb_pat.pat_desc with
    | Tpat_var (ident, loc) ->
       ident
    | _ -> raise (Not_implemented_yet "generate_value_binding")
  in
  
  match vb_expr.exp_desc with
    Texp_function _ ->
    Format.fprintf fmt "func %s %a\n"
                   ident.Ident.name
                   generate_expression vb_expr;
    
  | _ ->  Format.fprintf fmt "let %s = %a\n"
                         ident.Ident.name
                         generate_expression vb_expr
        
        
let generate_structure_item fmt item =
  let { str_desc; _ } = item in
  match str_desc with
  | Tstr_value (rec_flag, val_binds) ->
    
        List.iter (generate_value_binding fmt) val_binds;
        Format.fprintf fmt ""
    
  | Tstr_eval (expr, attributes) ->
     generate_expression fmt expr;
     Format.fprintf fmt ""     
     
  | _ -> raise (Not_implemented_yet "generate_structure_item")
       
           
let generate_from_structure fmt structure =
  let {str_items; str_type; str_final_env} = structure in
  
  List.iter (generate_structure_item fmt) str_items;
  
  (* Flushing the output *)
  Format.fprintf fmt "\n%!";
  ()

