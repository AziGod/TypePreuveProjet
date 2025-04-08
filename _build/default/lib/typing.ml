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
  





(* Fonctions utilisées dans tc_instr *)
(* Vérifie si un label est déclaré dans l'environnement *)
let verif_label_declared (lb: label) (env: environment) : bool =
  match env.types with
  | DBG (list_node, _) -> 
      List.exists (fun (DBN(n, _)) -> n = lb) list_node

(* Vérifie si une variable est déclarée dans l'environnement *)
let verif_declared_var (vn: vname) (env: environment) : bool =
  List.exists (fun (declared_vn, _) -> declared_vn = vn) env.bindings

(* Ajoute une variable avec son label dans l'environnement *)
let add_var_to_env (vn: vname) (lb: label) (env: environment) : environment =
  { env with bindings = (vn, lb) :: env.bindings }

(* Supprime une variable de l'environnement *)
let remove_var_from_env (vn: vname) (env: environment) : environment =
  { env with bindings = List.remove_assoc vn env.bindings }

(* Filtre l'environnement pour ne garder que les variables spécifiées dans la liste vn_list *)
let filter_env_with_vars (vn_list: vname list) (env: environment) : environment =
  let filtered_bindings = List.filter (fun (vn, _) -> List.mem vn vn_list) env.bindings in
  { env with bindings = filtered_bindings }




let tc_instr (i: instruction) (env: environment) : tc_result = 
  match i with
  | IActOnNode (_, vn, lb) ->
    if not (verif_label_declared lb env) then 
      Result.Error ["label non déclaré"]
    else if verif_declared_var vn env then 
      Result.Error ["variable déjà déclarée"]
    else 
      Result.Ok (add_var_to_env vn lb env)


  | IActOnRel (_, vn, lb, v2) ->
    if not (verif_label_declared lb env) then 
      Result.Error ["label non déclaré"]
    else if not (verif_declared_var vn env) then 
      Result.Error ["première variable non déclarée"]
    else if not (verif_declared_var v2 env) then 
      Result.Error ["deuxième variable non déclarée"]
    else 
      Result.Ok env  (* L’environnement reste inchangé *)


  | IDeleteNode vn ->
    if not (verif_declared_var vn env) then 
      Result.Error ["variable non déclarée"]
    else 
      Result.Ok (remove_var_from_env vn env)  (* Supprime la variable de l’environnement *)
    

  | IReturn vn_list ->
    if not (List.for_all (fun vn -> verif_declared_var vn env) vn_list) then 
      Result.Error ["une ou plusieurs variables retournées ne sont pas déclarées"]
    else 
      Result.Ok (filter_env_with_vars vn_list env)  (* Ne garde que les variables retournées *)
    

  | IWhere expr ->
    let expr_type = tp_expr env expr in
    if expr_type <> BoolT then 
      Result.Error ["l'expression WHERE doit être de type booléen"]
    else 
      Result.Ok env  (* L’environnement ne change pas *)


  | ISet (vn, fn, expr) ->
    (* Il faut vérifier que lors d’une affectation de la forme marie.age = 25, l’expression affectée
    est bien typée et correspond au type de l’attribut. L’environnement ne change pas *)
    Result.Error [""]


    
  | _  -> Result.Error ["instruction non implémentée"]




  





  

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
  





(* Vérifie que les types sont uniques et que les labels dans les relations sont déclarés dans les noeuds *)
let check_graph_types (DBG (list_node, list_rel)) =
  (* Fonction auxiliaire pour vérifier l'unicité des types *)
  let rec no_duplicates = function
    | [] -> true
    | (x :: xs) -> not (List.mem x xs) && (no_duplicates xs)
  in
  (* Vérifie que les types sont uniques *)
  let types_unique list_node =
    no_duplicates (List.map (fun (DBN(n, _)) -> n) list_node)
  in
  (* Vérifie que tous les labels des relations sont déclarés dans les noeuds *)
  let labels_declared_in_relations list_node list_rel =
    let declared_labels = List.map (fun (DBN(n, _)) -> n) list_node in
    List.for_all (fun (DBR(n1, _, n2)) -> List.mem n1 declared_labels && List.mem n2 declared_labels) list_rel
  in
  (* Vérification des doublons et des labels dans les relations *)
  if types_unique list_node && labels_declared_in_relations list_node list_rel then
    Result.Ok ()  (* Aucun problème détecté *)
  else
    Result.Error "Types sont dupliqués ou relation fait référence à un label non déclaré"




















