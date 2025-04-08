
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


  let create_relation v1 lb v2 (State(g, tab, mn)) = 
    let Table(vns, lns) = tab in
    let v1_index = index_of v1 vns in
    let v2_index = index_of v2 vns in
    let relations = List.map (fun ln -> DBR(List.nth ln v1_index, lb, List.nth ln v2_index)) lns in
    let new_graph = List.fold_left add_rel_to_graph g relations in
    State (new_graph, tab, mn)
        
  
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
  
let match_node v lb (State(g, tab, mn))=
  let Table(vns, lns) = tab in
  (* Get all node IDs with label lb from the graph *)
  let matching_nodes = List.filter_map (fun (DBN(n, (node_lb, _))) ->
    if node_lb = lb then Some n else None
  ) (nodes_of_graph g) in
    (* Create Cartesian product: combine each matching node with each existing line *)
    let new_lns = List.concat_map (fun ln ->
      List.map (fun n -> n :: ln) matching_nodes
    ) lns in State(g, Table(v :: vns, new_lns), mn)


(* Helper function to check if a relation exists and filter table lines *)
  let match_relation sv lb tv (State(g, tab, mn)) =
    let Table(vns, lns) = tab in
    (* Find indices of source and target variables *)
    let sv_index = index_of sv vns in
    let tv_index = index_of tv vns in
    (* Get all relations from the graph *)
    let relations = rels_of_graph g in
    (* Filter lines where there exists a relation labeled lb from sv to tv *)
    let filtered_lns = List.filter (fun ln ->
      let source_node = List.nth ln sv_index in
      let target_node = List.nth ln tv_index in
      List.exists (fun (DBR(s, rel_lb, t)) ->
        s = source_node && t = target_node && rel_lb = lb
      ) relations
    ) lns in
    State(g, Table(vns, filtered_lns), mn)

(* Helper function to project table onto a list of variables *)
let return_variables vs (State(g, tab, mn)) =
  let Table(vns, lns) = tab in
  (* Get the indices of the variables in vs from the original table *)
  let indices = List.map (fun v -> index_of v vns) vs in
  (* Project each line to only include the specified variables in the order of vs *)
  let projected_lns = List.map (fun ln ->
    List.map (fun idx -> List.nth ln idx) indices
  ) lns in
  State(g, Table(vs, projected_lns), mn)

(* Helper function to delete a node and its associated relations, and update the table *)
let delete_node v (State(g, tab, mn)) =
  let Table(vns, lns) = tab in
  (* Find the index of the variable to delete *)
  let v_index = index_of v vns in
  (* Get all node IDs associated with this variable *)
  let nodes_to_delete = List.filter_map (fun ln -> List.nth_opt ln v_index) lns in
  (* Remove nodes from the graph *)
  let updated_nodes = List.filter (fun (DBN(n, _)) ->
    not (List.mem n nodes_to_delete)
  ) (nodes_of_graph g) in
  (* Remove all relations involving these nodes as source or target *)
  let updated_rels = List.filter (fun (DBR(s, _, t)) ->
    not (List.mem s nodes_to_delete || List.mem t nodes_to_delete)
  ) (rels_of_graph g) in
  (* Update the table: remove the column corresponding to v *)
  let new_vns = List.filter ((<>) v) vns in
  let new_lns = List.map (fun ln ->
    List.filteri (fun i _ -> i <> v_index) ln
  ) lns in
  State(DBG(updated_nodes, updated_rels), Table(new_vns, new_lns), mn)

  (* Helper function to delete relations matching source, label, and target *)
let delete_relation sv lb tv (State(g, tab, mn)) =
  let Table(vns, lns) = tab in
  (* Find indices of source and target variables *)
  let sv_index = index_of sv vns in
  let tv_index = index_of tv vns in
  (* Get all pairs of nodes from the table for sv and tv *)
  let node_pairs = List.map (fun ln ->
    (List.nth ln sv_index, List.nth ln tv_index)
  ) lns in
  (* Filter out relations that match the label and the node pairs *)
  let updated_rels = List.filter (fun (DBR(s, rel_lb, t)) ->
    not (rel_lb = lb && List.exists (fun (src, tgt) -> s = src && t = tgt) node_pairs)
  ) (rels_of_graph g) in
  State(DBG(nodes_of_graph g, updated_rels), tab, mn)

(* Helper function to filter table lines based on an expression *)
let where_instr e (State(g, tab, mn)) =
  let Table(vns, lns) = tab in
  (* Filter lines where e evaluates to true *)
  let filtered_lns = List.filter (fun ln ->
    (* Temporarily modify the table to evaluate e for this specific line *)
    let temp_tab = Table(vns, [ln]) in
    let temp_state = State(g, temp_tab, mn) in
    match eval_expr temp_state e with
    | BoolV true -> true
    | BoolV false -> false
    | _ -> failwith "Where expression must evaluate to a boolean"
  ) lns in
  State(g, Table(vns, filtered_lns), mn)
  
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

