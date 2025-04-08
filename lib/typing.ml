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
let rec tp_expr (env: environment) = function
  | Const (IntV _) -> IntT
  | Const (BoolV _) -> BoolT
  | Const (StringV _) -> StringT


  | AttribAcc (vn, fn) -> (* Accès à un attribut d’un nœud via une variable *)
      (match List.assoc_opt vn env.bindings with
      | None -> raise (FieldAccError ("Variable " ^ vn ^ " non déclarée"))
      | Some lb -> 
          (* On va chercher le type de l’attribut fn dans les nœuds *)
          let rec find_label l = function
            | [] -> None
            | DBN(n, atlist)::rest when n = l -> List.assoc_opt fn atlist
            | _::rest -> find_label l rest
          in
          (match env.types with
          | DBG (list_node, _) ->
              (match find_label lb list_node with
              | Some t -> t
              | None -> raise (FieldAccError ("Attribut " ^ fn ^ " non trouvé pour le label " ^ lb))
              )
          )
      )

      | BinOp (bop, e1, e2) ->
        let t1 = tp_expr env e1 in
        let t2 = tp_expr env e2 in
        (match bop with
        | BArith bop' -> (* Cas pour les opérations arithmétiques *)
            (match bop' with
            | BAadd | BAsub | BAmul | BAdiv | BAmod ->
                if t1 = IntT && t2 = IntT then IntT
                else raise (TypeError "Opération arithmétique sur des types non entiers")
            )
        
        | BCompar bop' -> (* Cas pour les comparaisons *)
            (match bop' with
            | BCeq | BCge | BCgt | BCle | BClt | BCne ->
                if t1 = t2 then BoolT
                else raise (TypeError "Comparaison entre types différents")
            )
        
        | BLogic bop' -> (* Cas pour les opérations logiques *)
            (match bop' with
            | BLand | BLor ->
                if t1 = BoolT && t2 = BoolT then BoolT
                else raise (TypeError "Opération logique sur des types non booléens")
            )
        )

 
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
  | IActOnNode (_, vn, lb) ->
    if not (verif_label_declared lb env) then 
      Result.Error ["label non déclaré: " ^ (lb)]
    else if verif_declared_var vn env then 
      Result.Error ["variable déjà déclarée: " ^ (vn)]
    else 
      Result.Ok (add_var_to_env vn lb env)


  | IActOnRel (_, vn, lb, v2) ->
    if not (verif_label_declared lb env) then 
      Result.Error ["label non déclaré: " ^ (lb)]
    else if not (verif_declared_var vn env) then 
      Result.Error ["première variable non déclarée: " ^ (vn)]
    else if not (verif_declared_var v2 env) then 
      Result.Error ["deuxième variable non déclarée: " ^ (v2)]
    else 
      Result.Ok env  (* L’environnement reste inchangé *)


  | IDeleteNode vn ->
    if not (verif_declared_var vn env) then 
      Result.Error ["variable non déclarée: " ^ (vn)]
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
      (* Vérifie que la variable est déclarée *)
      (match List.assoc_opt vn env.bindings with
      | None -> Result.Error ["variable non déclarée"]
      | Some lb ->
          (* On cherche le type du label dans la DB *)
          let rec find_label l = function
            | [] -> None
            | DBN (n, atlist) :: rest -> if n = l then Some atlist else find_label l rest
          in
          (match env.types with
          | DBG (list_node, _) ->
              (match find_label lb list_node with
              | None -> Result.Error ["label non trouvé dans la DB"]
              | Some atlist ->
                  (* On cherche le type de l’attribut *)
                  (match List.assoc_opt fn atlist with
                  | None -> Result.Error ["attribut non trouvé dans le label"]
                  | Some expected_tp ->
                      (* On vérifie le type de l'expression *)
                      try
                        let actual_tp = tp_expr env expr in
                        if actual_tp = expected_tp then
                          Result.Ok env  (* l'environnement ne change pas *)
                        else
                          Result.Error [
                            "mauvais type pour l'expression affectée : attendu " ^
                            (show_attrib_tp expected_tp) ^ " mais obtenu " ^ (show_attrib_tp actual_tp)
                          ]
                      with
                      | TypeError s -> Result.Error [s]
                      | FieldAccError s -> Result.Error [s]
                  )
              )
          )
      )
  | _  -> Result.Error ["instruction non implémentée"]




  





  

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








(* Fonction principale pour vérifier un programme *)
let typecheck continue (NormProg(gt, NormQuery instrs) as np) = 
  match check_graph_types gt with
  | Result.Error egt -> Printf.printf "%s\n" ("Undeclared types in\n" ^ egt);
                        failwith "stopped"
  | _ -> typecheck_instructions continue gt instrs np
  



















