(* Normalized language, after removal of patterns and other syntactic sugar *)

open Lang

type action = CreateAct | MatchAct
  [@@deriving show]

type instruction
  = IActOnNode of action * vname * label
  | IActOnRel of action * vname * label * vname
  | IDeleteNode of vname
  | IDeleteRel of vname * label * vname 
  | IReturn of vname list
  | IWhere of expr
  | ISet of vname * fieldname * expr 
  [@@deriving show]

(* Normalized query *)
type norm_query = NormQuery of instruction list
  [@@deriving show]

type norm_prog = NormProg of db_tp * norm_query
  [@@deriving show]


let normalize_node_pattern act = function 
| DeclPattern (v, l) -> (v, [IActOnNode(act, v, l)])
| VarRefPattern (v) -> (v, [])

(*
* Fonction qui permet de traiter la création de noeud et de relation selon que l'on ait un pattern simple ou complexe
*)
let rec normalize_pattern act = function 
| SimpPattern p -> normalize_node_pattern act p
| CompPattern (npt, rl, pt) -> 
  let (v1, instr1) = normalize_node_pattern act npt in
  let (v2, instr2) = normalize_pattern act pt in
  match instr2 with (* match pour traiter d'abord les noeuds non existants avant les relations*)
  | firstInstr::instr2 -> ( 
    match firstInstr with
    | IActOnNode _ -> (v1, instr1 @ firstInstr :: [IActOnRel(act, v1, rl, v2)] @ instr2 ) (* si c'est une instruction pour créer un noeud on la renvoie avant l'instruction pour ajouter une relation *)
    | _ -> (v1, instr1 @ [IActOnRel(act, v1, rl, v2)] @ firstInstr::instr2 ) (* sinon pas d'instruction donc osef *)
    )
  | [] -> (v1, instr1 @ [IActOnRel(act, v1, rl, v2)]) (* si instr2 ne contient pas d'instruction on ne le traite pas *)

let normalize_clause = function
| Create pats -> List.concat_map (fun p -> snd (normalize_pattern CreateAct p)) pats
| Match pats -> List.concat_map (fun p -> snd (normalize_pattern MatchAct p)) pats
| Delete (DeleteNodes vns) -> List.map (fun vn -> IDeleteNode vn) vns
| Delete (DeleteRels rels) -> List.map (fun (v1, l, v2) -> IDeleteRel (v1, l, v2)) rels
| Return vns -> [IReturn vns]
| Where e -> [IWhere e]
| Set sets -> List.map (fun (vn, fn, e) -> ISet (vn, fn, e)) sets

let normalize_query (Query cls) = NormQuery (List.concat_map normalize_clause cls)

let normalize_prog (Prog(tds, q)) = NormProg(tds, normalize_query q)
