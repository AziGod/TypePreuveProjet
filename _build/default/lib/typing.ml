open Graphstruct
open Lang
open Instr
 
type environment = { types:  db_tp; bindings: (vname * label) list }

let initial_environment gt = {types = gt; bindings = []}
let initial_result gt = Result.Ok (initial_environment gt)
  
exception FieldAccError of string
exception TypeError of string

(* Résultat d’un typage avec possibilité d’erreurs sous forme de string list *)
type tc_result = (environment, string list) result



(* Typage des expressions *)
let rec tp_expr env = function
  | Const v -> 
      (match v with
       | BoolV _ -> BoolT
       | IntV _ -> IntT
       | StringV _ -> StringT)
  | AttribAcc (vn, fn) -> 
      (* Vérifier que la variable vn est déclarée dans l'environnement et que l'attribut existe *)
      let label = List.assoc vn env.bindings in
      let field_type = try
        (* Chercher l'attribut dans la liste de déclarations de type *)
        let fields = List.assoc label (List.map (fun (DBN (l,a)) -> (l,a)) (nodes_of_graph env.types)) in
        List.assoc fn fields
      with Not_found -> raise (FieldAccError ("Field " ^ fn ^ " not found"))
      in
      field_type
  | BinOp (bop, e1, e2) ->
      let t1 = tp_expr env e1 in
      let t2 = tp_expr env e2 in
      (match bop with
       | BArith _ -> if t1 = t2 && t1 = IntT then IntT else raise (TypeError "Arithmetic operation type mismatch")
       | BCompar _ -> if t1 = t2 && (t1 = IntT || t1 = BoolT) then BoolT else raise (TypeError "Comparison operation type mismatch")
       | BLogic _ -> if t1 = BoolT && t2 = BoolT then BoolT else raise (TypeError "Logical operation type mismatch"))


 
(* Vérifie qu’une expression a bien un type attendu *)
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



(* Vérifie une instruction *)
let tc_instr (i: instruction) (env: environment) : tc_result = 
  match i with
  | IActOnNode (act, vn, lb) ->
      (* Vérifie si le label est un nœud du graphe de types *)
      let label_exists =
        List.exists (fun (DBN (l, _)) -> l = lb) (nodes_of_graph env.types)
      in
      if not label_exists then
        Result.Error [lb ^ " label not found in the type graph."]
      else
        begin match act with
        | CreateAct ->
            if List.mem_assoc vn env.bindings then
              Result.Error [vn ^ " is already bound."]
            else
              Result.Ok { env with bindings = (vn, lb) :: env.bindings }

        | MatchAct ->
            begin match List.assoc_opt vn env.bindings with
            | Some existing_lb when existing_lb = lb ->
                Result.Ok env
            | Some existing_lb ->
                Result.Error [vn ^ " has label " ^ existing_lb ^ " but expected " ^ lb]
            | None ->
                Result.Error [vn ^ " is not bound."]
            end
        end

      
  | IActOnRel (act, vn1, lb_rel, vn2) ->(
    let open Result in
    (* Vérifie que les deux variables sont déjà bindées *)
    match List.assoc_opt vn1 env.bindings, List.assoc_opt vn2 env.bindings with 
      | Some lb1, Some lb2 ->
        (* Vérifie que la relation est bien déclarée dans le graphe de types *)
        let rel_exists =
          List.exists (fun (DBR (src, rel, tgt)) ->
            src = lb1 && rel = lb_rel && tgt = lb2
          ) (rels_of_graph env.types)
        in
        if not rel_exists then
          Error ["Relation " ^ lb_rel ^ " not found between types " ^ lb1 ^ " and " ^ lb2]
        else
          Ok env
    | _ ->
      let missing = 
        [ if Option.is_none (List.assoc_opt vn1 env.bindings) then Some (vn1 ^ " is not bound.") else None;
          if Option.is_none (List.assoc_opt vn2 env.bindings) then Some (vn2 ^ " is not bound.") else None ]
        |> List.filter_map Fun.id
      in
      Error missing

    )
  | IDeleteNode vn ->
      (* Vérifier que la variable existe dans l'environnement avant de la supprimer *)
      if List.mem_assoc vn env.bindings then
        Result.Ok (remove_var_from_env vn env)
      else
        Result.Error [vn ^ " not found in the environment."]

  | IDeleteRel (vn1, lb, vn2) ->
      (* Vérifier que les variables existent dans l'environnement *)
      if List.mem_assoc vn1 env.bindings && List.mem_assoc vn2 env.bindings then
        Result.Ok env
      else
        Result.Error [vn1 ^ " or " ^ vn2 ^ " not found in the environment."]

  | IReturn vn_list ->
      (* Vérifier que toutes les variables dans la liste sont déclarées dans l'environnement *)
      let undeclared_vars = List.filter (fun vn -> not (List.mem_assoc vn env.bindings)) vn_list in
      if undeclared_vars = [] then
        Result.Ok env
      else
        Result.Error (List.map (fun vn -> vn ^ " not found in the environment.") undeclared_vars)

  | IWhere expr ->
      (* Vérifier que l'expression est bien de type BoolT *)
      let expr_type = tp_expr env expr in
      if expr_type = BoolT then
        Result.Ok env
      else
        Result.Error ["Expression in IWhere is not of type BoolT."]

  | ISet (vn, fn, expr) ->
      (* Vérifier que l'expression a le bon type pour l'attribut de la variable *)
      let expected_type = 
        try
          (* Chercher le type attendu pour l'attribut de la variable *)
          let label = List.assoc vn env.bindings in
          let field = List.find (fun (f, _) -> f = fn) (List.assoc label (List.map (fun (DBN (l,a)) -> (l,a)) (nodes_of_graph env.types)) ) in
          snd field
        with _ -> failwith "Field not found" in
      let expr_type = tp_expr env expr in
      if expr_type = expected_type then
        Result.Ok env
      else
        Result.Error ["Expression in ISet does not match the expected attribute type."]






  

(* Applique une vérification à une liste d'instructions avec arrêt immédiat en cas d’erreur *)
let check_and_stop (res : tc_result) i : tc_result = Result.bind res (tc_instr i)

let tc_instrs_stop gt instrs : tc_result = 
  List.fold_left check_and_stop (initial_result gt) instrs

(* Point d’entrée du typage complet *)
let typecheck_instructions continue gt instrs np = 
  let r = tc_instrs_stop gt instrs in
  match r with
  | Result.Error etc -> Printf.printf "%s\n" (String.concat "\n" etc); 
                        failwith "stopped"
  | _ -> np
  
  


(* Vérifie que les types dans le graphe sont bien formés *)
let check_graph_types (DBG (ntdecls, rtdecls)) = Result.Ok true
(*
  (* Vérification des nœuds *)
  let check_node_decl (label, fields) = 
    if List.exists (fun (l, _) -> l = label) ntdecls then
      Result.Error ["Node type " ^ label ^ " is declared multiple times."]
    else
      Result.Ok ()
  in

  (* Vérification des relations *)
  let check_rel_decl (label, fields) =
    if List.exists (fun (l, _) -> l = label) rtdecls then
      Result.Error ["Relation type " ^ label ^ " is declared multiple times."]
    else
      Result.Ok ()
  in

  (* Appliquer les vérifications *)
  let check_nodes = List.fold_left (fun acc decl -> Result.bind acc (check_node_decl decl)) (Result.Ok ()) ntdecls in
  let check_rels = List.fold_left (fun acc decl -> Result.bind acc (check_rel_decl decl)) (Result.Ok ()) rtdecls in

  Result.bind check_nodes check_rels
*)








(* Fonction principale pour vérifier un programme *)
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
  



















