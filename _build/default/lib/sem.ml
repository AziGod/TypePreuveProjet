
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

let create_node v lb (State(g, tab, mn)) = 
  let Table(_vns, lns) = tab in 
  let new_node_ids = List.init (List.length lns) (fun i -> mn +i) in 
  let new_nodes = List.init (List.length lns) (fun i -> DBN(mn + i , (lb, []))) in
  let new_tab = add_var_mult_nodes_to_table v new_node_ids tab in
  let new_graph = add_nodes_to_graph new_nodes g in 
  State (new_graph, new_tab, mn + 1)

  (*fonction annexe pour trouver l'indice d'un element dans une liste*)
  let index_of elt lst =
    let rec aux i = function
      | [] -> raise Not_found
      | x :: xs -> if x = elt then i else aux (i + 1) xs
    in
    aux 0 lst

  let create_relation v1 lb v2 (State(g, tab, mn)) = 
    let Table(vns, lns) = tab in
    let v1_index = index_of v1 vns in
    let v2_index = index_of v2 vns in
    let relations = List.map (fun ln -> DBR(List.nth ln v1_index, lb, List.nth ln v2_index)) lns in
    let new_graph = List.fold_left add_rel_to_graph g relations in
    State (new_graph, tab, mn)

    let eval_expr (State(g, tab, _)) = function
    | Const v -> v
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
    | BinOp _ -> failwith "Opération binaire pas encore implémentée"
        
  
  let set_attribut vn a e (State(g, tab, mn)) =
    let e_value = eval_expr (State(g, tab, mn)) e in  (* Évaluation de e *)
    let Table(vns, lns) = tab in
    match List.find_opt ((=) vn) vns with
    | Some _ ->
        let idx_vn = index_of vn vns in
        let node_ids = List.filter_map (fun ln -> List.nth_opt ln idx_vn) lns in
        let updated_nodes = List.map (fun (DBN(n, (lb, attrs))) ->
          if List.mem n node_ids
          then DBN(n, (lb, (a, e_value) :: List.remove_assoc a attrs))
          else DBN(n, (lb, attrs))
        ) (nodes_of_graph g) in
        State (DBG(updated_nodes, rels_of_graph g), tab, mn)
    | None -> State (g, tab, mn)
  


(* TODO: complete following definition *)
let exec_instr s = function
  | IActOnNode (CreateAct, v, lb) -> create_node v lb s 
  | IActOnRel (CreateAct, v1, lb, v2) -> create_relation v1 lb v2 s
  | ISet (vn, a, e) -> set_attribut vn a e s 
  | _ -> s
  

let exec (NormProg(_tps, NormQuery(instrs))) = 
  List.fold_left exec_instr initial_state instrs

