
(* Semantics of instructions *)
open Lang
open Graphstruct
open Instr

  (* State of the program, essentially a graph structure and a binding in form of a table,
  and as convenience info an overapproximation of the maximal node number
  allocated in the graph (could in principle be recomputed each time)
  Nodes have a unique identifier (an int) which is incremented when creating new nodes.
  When deleting nodes, the max node number is currently not decremented, 
  thus does not reflect the number of nodes in the graph, but one could think
  of a garbage collector when running out of node identifiers.
   *)

(* Naive implementation of bindings as tables, ie. 
   a heading (variable list 'v list) and a list of lines containing nodes 
   that each have the same length as the variable list  *)

type ('v, 'n) table = Table of 'v list * 'n list list
  [@@deriving show]

type vname_nodeid_table = (vname, nodeid) table
  [@@deriving show]

let empty_table = Table([], [[]])

(* Add a single variable v, bound to a single node n, to a table,
  as during node creation (old interpretation, now superseded, 
  see create_node and add_var_mult_nodes_to_table) *)
let add_var_single_node_to_table v n (Table (vns, lns)) = 
    Table (v::vns, List.map (fun ln -> n::ln) lns)

(* Add multiple nodes contained in ns for a new variable v to a table,
  one node per line. ns and lns have to have the same length.  *)
let add_var_mult_nodes_to_table v ns (Table (vns, lns)) = 
      Table (v::vns, List.map2 (fun n ln -> n::ln) ns lns)
      


type attrib_val = fieldname * value
  [@@deriving show]
type attrib_struct = label * (attrib_val list)
  [@@deriving show]
      
type db_graph_struct = (Graphstruct.nodeid, attrib_struct, label) Graphstruct.db_graph
  [@@deriving show]
 

type state = State of db_graph_struct * (vname, nodeid) table * nodeid
let initial_state = State(empty_graph, empty_table, 0)

(* fonction annexe pour trouver l'indice d'un element dans une liste *)
let index_of elt lst =
  let rec aux i = function
    | [] -> raise Not_found
    | x :: xs -> if x = elt then i else aux (i + 1) xs
  in
  aux 0 lst

(* fonction annexe pour évaluer une expression *)
let rec eval_expr (State(g, tab, mn)) = function
  | Const v -> v  (* Already handles constant values *)
  | AttribAcc (vn, a) -> 
      let Table(vns, lns) = tab in
      (match List.find_opt ((=) vn) vns with
      | Some _ ->
          let idx_vn = index_of vn vns in
          let node_ids = List.filter_map (fun ln -> List.nth_opt ln idx_vn) lns in
          (match List.find_map (fun (DBN(n, (_, attrs))) ->
            if List.mem n node_ids then List.assoc_opt a attrs else None
          ) (nodes_of_graph g) with
          | Some v -> v
          | None -> failwith "Attribute not found")
      | None -> failwith "Variable not found")
  | BinOp (op, e1, e2) -> 
      let v1 = eval_expr (State(g, tab, mn)) e1 in
      let v2 = eval_expr (State(g, tab, mn)) e2 in
      match op with
      (* Arithmetic operations *)
      | BArith BAadd -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> IntV (i1 + i2)
           | _ -> failwith "Addition requires integer operands")
      | BArith BAsub -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> IntV (i1 - i2)
           | _ -> failwith "Subtraction requires integer operands")
      | BArith BAmul -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> IntV (i1 * i2)
           | _ -> failwith "Multiplication requires integer operands")
      | BArith BAdiv -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) when i2 <> 0 -> IntV (i1 / i2)
           | (IntV _, IntV 0) -> failwith "Division by zero"
           | _ -> failwith "Division requires integer operands")
      | BArith BAmod -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) when i2 <> 0 -> IntV (i1 mod i2)
           | (IntV _, IntV 0) -> failwith "Modulo by zero"
           | _ -> failwith "Modulo requires integer operands")
      (* Comparison operations *)
      | BCompar BCeq -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 = i2)
           | (StringV s1, StringV s2) -> BoolV (s1 = s2)
           | (BoolV b1, BoolV b2) -> BoolV (b1 = b2)
           | _ -> failwith "Equality comparison requires compatible types")
      | BCompar BCge -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 >= i2)
           | _ -> failwith "Greater-than-or-equal requires integer operands")
      | BCompar BCgt -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 > i2)
           | _ -> failwith "Greater-than requires integer operands")
      | BCompar BCle -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 <= i2)
           | _ -> failwith "Less-than-or-equal requires integer operands")
      | BCompar BClt -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 < i2)
           | _ -> failwith "Less-than requires integer operands")
      | BCompar BCne -> 
          (match (v1, v2) with
           | (IntV i1, IntV i2) -> BoolV (i1 <> i2)
           | (StringV s1, StringV s2) -> BoolV (s1 <> s2)
           | (BoolV b1, BoolV b2) -> BoolV (b1 <> b2)
           | _ -> failwith "Inequality comparison requires compatible types")
      (* Logical operations *)
      | BLogic BLand -> 
          (match (v1, v2) with
           | (BoolV b1, BoolV b2) -> BoolV (b1 && b2)
           | _ -> failwith "Logical AND requires boolean operands")
      | BLogic BLor -> 
          (match (v1, v2) with
           | (BoolV b1, BoolV b2) -> BoolV (b1 || b2)
           | _ -> failwith "Logical OR requires boolean operands")


let create_node v lb (State(g, tab, mn)) = 
  let Table(_vns, lns) = tab in 
  let new_node_ids = List.init (List.length lns) (fun i -> mn +i) in 
  let new_nodes = List.init (List.length lns) (fun i -> DBN(mn + i , (lb, []))) in
  let new_tab = add_var_mult_nodes_to_table v new_node_ids tab in
  let new_graph = add_nodes_to_graph new_nodes g in 
  State (new_graph, new_tab, mn + 1)


(*
 * create_relation : Crée de nouvelles relations dans le graphe entre les nœuds liés à deux variables.
 * 
 * Paramètres :
 *   - v1 : string - Le nom de la variable source dont les nœuds liés seront les origines des relations.
 *   - lb : label - L'étiquette à attribuer aux nouvelles relations.
 *   - v2 : string - Le nom de la variable cible dont les nœuds liés seront les destinations des relations.
 *   - State(g, tab, mn) : state - L'état actuel contenant le graphe, la table et l'ID maximal des nœuds.
 *
 * Description :
 *   Créer une relation entre chaque noeud de la colonne v1 vers chaque noeud de la colonne v2
 *)
 let create_relation v1 lb v2 (State(g, tab, mn)) = 
  let Table(vns, lns) = tab in  (* Extrait les noms des variables et les lignes de la table *)
  let v1_index = index_of v1 vns in  (* Récupère l'index de la colonne de la variable source v1 *)
  let v2_index = index_of v2 vns in  (* Récupère l'index de la colonne de la variable cible v2 *)
  (* Crée une liste de relations (DBR) pour chaque ligne de la table, utilisant les nœuds de v1 comme sources et ceux de v2 comme cibles *)
  let relations = List.map (fun ln -> DBR(List.nth ln v1_index, lb, List.nth ln v2_index)) lns in
  (* Ajoute toutes les nouvelles relations au graphe en utilisant fold_left *)
  let new_graph = List.fold_left add_rel_to_graph g relations in
  State (new_graph, tab, mn)  (* Retourne l'état mis à jour avec le nouveau graphe *)

  
(*
 * match_node : Associe une variable à tous les nœuds du graphe ayant une étiquette donnée.
 * 
 * Paramètres :
 *   - v : string - Le nom de la variable à associer aux nœuds correspondants.
 *   - lb : label - L'étiquette des nœuds à rechercher dans le graphe.
 *   - State(g, tab, mn) : state - L'état actuel contenant le graphe, la table et l'ID maximal.
 * 
 Description :
 *   Cette fonction trouve tous les nœuds du graphe étiquetés 'lb' et les associe à la variable 'v' 
 *   dans la table en effectuant un produit cartésien avec les lignes existantes.
 *
 *)
let match_node v lb (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les noms des variables et les lignes de la table *)
  (* Récupère tous les IDs des nœuds du graphe ayant l'étiquette lb *)
  let matching_nodes = List.filter_map (fun (DBN(n, (node_lb, _))) ->
    if node_lb = lb then Some n else None
  ) (nodes_of_graph g) in
  (* Crée un produit cartésien : combine chaque nœud correspondant avec chaque ligne existante *)
  let new_lns = List.concat_map (fun ln ->
    List.map (fun n -> n :: ln) matching_nodes
  ) lns in
  State(g, Table(v :: vns, new_lns), mn)  (* Retourne l'état avec la table mise à jour *)
      
      
(*
 * match_relation : Filtre les lignes de la table selon l'existence d'une relation spécifiée.
 * 
 * Paramètres :
 *   - sv : string - La variable source de la relation.
 *   - lb : label - L'étiquette de la relation à vérifier.
 *   - tv : string - La variable cible de la relation.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 * Description :
 *   Cette fonction conserve uniquement les lignes de la table où une relation étiquetée 'lb' 
 *   existe entre les nœuds liés à 'sv' et 'tv'.
 *)
let match_relation sv lb tv (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  let sv_index = index_of sv vns in  (* Index de la variable source *)
  let tv_index = index_of tv vns in  (* Index de la variable cible *)
  let relations = rels_of_graph g in  (* Récupère toutes les relations du graphe *)
  (* Filtre les lignes où une relation 'lb' existe de sv à tv *)
  let filtered_lns = List.filter (fun ln ->
    let source_node = List.nth ln sv_index in  (* Nœud source de la ligne *)
    let target_node = List.nth ln tv_index in  (* Nœud cible de la ligne *)
    List.exists (fun (DBR(s, rel_lb, t)) ->
      s = source_node && t = target_node && rel_lb = lb
    ) relations
  ) lns in
  State(g, Table(vns, filtered_lns), mn)  (* Retourne l'état avec la table filtrée *)
    

 (*
 * delete_node : Supprime un nœud du graphe et met à jour la table.
 * 
 * Paramètres :
 *   - v : string - La variable dont les nœuds associés doivent être supprimés.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 * Description :
 *   Supprime les nœuds liés à 'v' du graphe, ainsi que leurs relations, et retire la colonne 
 *   correspondante de la table.
 *)
let delete_node v (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  let v_index = index_of v vns in  (* Index de la variable à supprimer *)
  let nodes_to_delete = List.filter_map (fun ln -> List.nth_opt ln v_index) lns in  (* IDs des nœuds à supprimer *)
  let updated_nodes = List.filter (fun (DBN(n, _)) -> not (List.mem n nodes_to_delete)) (nodes_of_graph g) in  (* Supprime les nœuds *)
  let updated_rels = List.filter (fun (DBR(s, _, t)) -> not (List.mem s nodes_to_delete || List.mem t nodes_to_delete)) (rels_of_graph g) in  (* Supprime les relations associées *)
  let new_vns = List.filter ((<>) v) vns in  (* Retire la variable de la liste *)
  let new_lns = List.map (fun ln -> List.filteri (fun i _ -> i <> v_index) ln) lns in  (* Retire la colonne *)
  State(DBG(updated_nodes, updated_rels), Table(new_vns, new_lns), mn)  (* Retourne l'état mis à jour *)
            
(*
 * delete_relation : Supprime les relations spécifiées du graphe.
 * 
 * Paramètres :
 *   - sv : string - Variable source des relations.
 *   - lb : label - Étiquette des relations à supprimer.
 *   - tv : string - Variable cible des relations.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 * Description :
 *   Supprime toutes les relations étiquetées 'lb' entre les nœuds liés à 'sv' et 'tv'.
 *)
 let delete_relation sv lb tv (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  let sv_index = index_of sv vns in  (* Index de la variable source *)
  let tv_index = index_of tv vns in  (* Index de la variable cible *)
  let node_pairs = List.map (fun ln -> (List.nth ln sv_index, List.nth ln tv_index)) lns in  (* Paires de nœuds sv-tv *)
  let updated_rels = List.filter (fun (DBR(s, rel_lb, t)) ->
    not (rel_lb = lb && List.exists (fun (src, tgt) -> s = src && t = tgt) node_pairs)
  ) (rels_of_graph g) in  (* Supprime les relations correspondantes *)
  State(DBG(nodes_of_graph g, updated_rels), tab, mn)  (* Retourne l'état avec le graphe mis à jour *)

        
(*
 * return_variables : Projette la table sur une liste de variables spécifiées.
 * 
 * Paramètres :
 *   - vs : string list - Liste des variables à conserver dans l'ordre souhaité.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 * Description :
 *   Cette fonction réduit la table aux colonnes correspondant aux variables dans 'vs', 
 *   en respectant leur ordre.
 *)
 let return_variables vs (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  let indices = List.map (fun v -> index_of v vns) vs in  (* Indices des variables dans vs *)
  (* Projette chaque ligne pour ne garder que les variables spécifiées dans l'ordre de vs *)
  let projected_lns = List.map (fun ln ->
    List.map (fun idx -> List.nth ln idx) indices
  ) lns in
  State(g, Table(vs, projected_lns), mn)  (* Retourne l'état avec la table projetée *)
      

(*
 * where_instr : Filtre les lignes de la table selon une expression.
 * 
 * Paramètres :
 *   - e : expr - L'expression à évaluer pour chaque ligne.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 *
 * Description :
 *   Conserve uniquement les lignes où l'expression 'e' évalue à true.
 *)
let where_instr e (State(g, tab, mn)) =
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  let filtered_lns = List.filter (fun ln ->
    let temp_tab = Table(vns, [ln]) in  (* Table temporaire avec une seule ligne *)
    let temp_state = State(g, temp_tab, mn) in  (* État temporaire pour évaluation *)
    match eval_expr temp_state e with
    | BoolV true -> true  (* Garde la ligne si e est vrai *)
    | BoolV false -> false  (* Supprime la ligne si e est faux *)
    | _ -> failwith "L'expression de where doit être un booléen"
  ) lns in
  State(g, Table(vns, filtered_lns), mn)  (* Retourne l'état avec la table filtrée *)

      
(*
 * set_attribut : Définit un attribut pour les nœuds liés à une variable.
 * 
 * Paramètres :
 *   - vn : string - La variable dont les nœuds auront l'attribut modifié.
 *   - a : fieldname - Le nom de l'attribut à définir.
 *   - e : expr - L'expression dont la valeur sera assignée à l'attribut.
 *   - State(g, tab, mn) : state - L'état actuel.
 *
 * Description :
 *   Évalue 'e' et assigne sa valeur à l'attribut 'a' pour tous les nœuds liés à 'vn'.
 *)
 let set_attribut vn a e (State(g, tab, mn)) =
  let e_value = eval_expr (State(g, tab, mn)) e in  (* Évalue l'expression e *)
  let Table(vns, lns) = tab in  (* Extrait les variables et les lignes *)
  match List.find_opt ((=) vn) vns with
  | Some _ ->
      let idx_vn = index_of vn vns in  (* Index de la variable *)
      let node_ids = List.filter_map (fun ln -> List.nth_opt ln idx_vn) lns in  (* IDs des nœuds liés à vn *)
      let updated_nodes = List.map (fun (DBN(n, (lb, attrs))) ->
        if List.mem n node_ids
        then DBN(n, (lb, (a, e_value) :: List.remove_assoc a attrs))  (* Met à jour l'attribut *)
        else DBN(n, (lb, attrs))  (* Laisse inchangé *)
      ) (nodes_of_graph g) in
      State (DBG(updated_nodes, rels_of_graph g), tab, mn)  (* Retourne l'état avec le graphe mis à jour *)
  | None -> State (g, tab, mn)  (* Retourne l'état inchangé si la variable n'existe pas *)


  (* TODO: complete following definition *)
  let exec_instr s = function
    | IActOnNode (CreateAct, v, lb) -> create_node v lb s 
    | IActOnRel (CreateAct, v1, lb, v2) -> create_relation v1 lb v2 s
    | IActOnNode (MatchAct, v, lb) -> match_node v lb s 
    | IActOnRel (MatchAct, sv, lb, tv) -> match_relation sv lb tv s
    | IDeleteNode v -> delete_node v s
    | IDeleteRel (sv, lb, tv) -> delete_relation sv lb tv s
    | IReturn vs -> return_variables vs s
    | IWhere e -> where_instr e s
    | ISet (vn, a, e) -> set_attribut vn a e s 
    
  
  let exec (NormProg(_tps, NormQuery(instrs))) = 
    List.fold_left exec_instr initial_state instrs
 
