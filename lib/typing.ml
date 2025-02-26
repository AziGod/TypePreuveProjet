open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string


type tc_result = (environment, string list) result

(* Functions for manipulating the environment *)

let add_var vn t (env:environment) = 
  {env with bindings = (vn,t)::env.bindings}

let remove_var vn env = 
  {env with bindings = List.remove_assoc vn env.bindings}

(* TODO: add more auxiliary functions here *)

(* TODO: fill in details *)
let check_graph_types (DBG (list_node, rtdecls)) = Result.Ok ()

(* TODO: fill in details *)
let rec tp_expr env = function
  Const v -> IntT
| AttribAcc (vn, fn) -> IntT
| BinOp (bop, e1, e2) -> tp_expr env e1

(* check expression e with expected type et in environment env *)
let check_expr e et env : tc_result = 
  try 
    if tp_expr env e = et
    then Result.Ok env
    else Result.Error ["Expression does not have expected type " ^ (show_attrib_tp et)]
  with 
  | TypeError s -> Result.Error [s]
  | FieldAccError s -> Result.Error [s]
  

let tc_instr (i: instruction) (env: environment) : tc_result = 
  match i with
  | IActOnNode (CreateAct, vn, lb) ->
    if not (verif_label_declared lb env)
    then Result.Error ["label non declare"]
    else if verif_declared_var vn env
    then Result.Error ["var deja declare"]
    else Result.Ok (add_var_to_env vn lb env)

  | _  -> Result.Error ["also not implemented"]

(* type check list of instructions and stop on error *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs


  (* TODO: typecheck instructions *)
let typecheck_instructions continue gt instrs np = 
  let r = Result.Ok initial_environment in   (* call to real typechecker here *)
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  |_ -> np
  

  (* Typecheck program; 
     If continue is true, continue typechecking even 
     when errors have been discovered (can be ignored in a first version) *)  
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
  



(* Returns true if list is without duplicates *)
  let rec no_duplicates = function
| [] -> true
| (x :: xs) -> not (List.mem x xs) && (no_duplicates xs);;

(* verify that types are unique (no duplicate declaration of a type) *)
let types_unique list_node = 
no_duplicates (List.map (fun (DBN(n, _)) -> n) list_node) 

(* TODO: fill in details *)
let check_graph_types (DBG (list_node, list_rel)) = 
   if types_unique list_node
   then Result.Ok () 
   else Result.Error "duplicates"