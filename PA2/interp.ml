(* $Id: interp.ml,v 1.7 2019-01-29 17:26:15-08 - - $ *)

open Absyn

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> 
        (match memref with
            | Variable (ident) -> 
                if Hashtbl.mem Tables.variable_table ident
                then Hashtbl.find Tables.variable_table ident 
                else 0.0 
            | Arrayref (ident, idx) -> 
                read_arr ident idx
            ); 
    | Unary (oper, expr) -> 
        if Hashtbl.mem Tables.unary_fn_table oper
        then (Hashtbl.find Tables.unary_fn_table oper) (eval_expr expr)
        else no_expr ("Invalid unary oper: " ^ oper)
    | Binary (oper, expr1, expr2) -> 
        if Hashtbl.mem Tables.binary_fn_table oper
        then (Hashtbl.find Tables.binary_fn_table oper)
          (eval_expr expr1) (eval_expr expr2)
        else no_expr ("Invalid binary oper: " ^ oper)
and read_arr ident idx =
  if Hashtbl.mem Tables.array_table ident
  then
    let arr = Hashtbl.find Tables.array_table ident in
    let index = int_of_float (eval_expr idx) in
      if index >= (Array.length arr) || (index < 0)
      then no_expr ("Array index out of bounds: "
        ^ (string_of_int index))
      else Array.get arr index
  else no_expr ("Array not found: " ^ ident)

and write_arr ident idx expr =
  if Hashtbl.mem Tables.array_table ident
  then
    let arr = Hashtbl.find Tables.array_table ident in
    let index = int_of_float (eval_expr idx) in
      if index >= (Array.length arr) || (index < 0)
      then no_expr ("Array index out of bounds: "
        ^ (string_of_int index))
      else Array.set arr index (eval_expr expr)
  else no_expr ("Array not found: " ^ ident)

let eval_relop (expr : Absyn.expr) : bool = match expr with
    | Binary (o, e1, e2) ->
        if Hashtbl.mem Tables.rel_fn_table o
        then (Hashtbl.find Tables.rel_fn_table o)
          (eval_expr e1) (eval_expr e2)
        else no_expr ("Invalid relational oper: " ^ o)
    | _ -> no_expr "Unexpected non-boolean expression"

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continuation
    | Let (memref, expr) -> interp_let memref expr continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> interp_if expr label continuation
    | Print print_list -> interp_print print_list continuation 
    | Input memref_list -> interp_input memref_list continuation

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ());
    interpret continuation

and interp_let (ref : Absyn.memref) (expr : Absyn.expr) 
                (continuation : Absyn.program) = 
    (match ref with 
    | Variable (ident) -> 
        Hashtbl.add Tables.variable_table ident (eval_expr expr)
    | Arrayref (ident, idx) -> 
        let arr = Hashtbl.find Tables.array_table ident 
        in Array.set arr (int_of_float (eval_expr idx)) (eval_expr expr)
    ); (* int_of_float = float -> int *)
    interpret continuation


and interp_dim (ident : Absyn.ident) (len : Absyn.expr)
                (continuation : Absyn.program) = 
    Hashtbl.add Tables.array_table ident 
        (Array.make (int_of_float (eval_expr len)) 0.0); 
    interpret continuation

and interp_goto (label : string) (continuation : Absyn.program) = 
    if Hashtbl.mem Tables.label_table label
    then interpret (Hashtbl.find Tables.label_table label)
    else no_expr ("Invalid label: " ^ label)

and interp_if (expr: Absyn.expr) (label : string) 
                (continuation : Absyn.program) = 
    if eval_relop expr 
    then interp_goto label continuation 
    else interpret continuation 

and interp_input (memref_list : Absyn.memref list)
                (continuation : Absyn.program) =
    let input_number memref =
            try let number = Etc.read_number ()
            in (match memref with
             | Variable (ident) ->
                Hashtbl.add Tables.variable_table ident number
             | Arrayref (ident, idx) ->
                if Hashtbl.mem Tables.array_table ident
                then
                    let arr = Hashtbl.find Tables.array_table ident in
                    let index = int_of_float (eval_expr idx) in
                        if index >= (Array.length arr) || (index < 0)
                    then no_expr ("Array index out of bounds: "
                      ^ (string_of_int index))
                    else Array.set arr index number
                else no_expr ("Array not found: " ^ ident)
            );
            with End_of_file -> 
                (Hashtbl.add Tables.variable_table "eof" 1.0);
        in List.iter input_number memref_list;
        interpret continuation


let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)
