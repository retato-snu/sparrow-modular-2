(***********************************************************************)
(* Deterministic residual-cycle scheduling evidence.                    *)
(***********************************************************************)

type edge_kind =
  | Direct_program_call
  | Residual_call_binding
  | Residual_dependency

type provenance_level =
  | Direct_program_callgraph
  | Residual_call_binding_provenance
  | Residual_dependency_only

type edge = {
  edge_kind : edge_kind;
  source : string;
  importer_or_caller : string;
  provider_or_callee : string;
  symbol : string;
  provenance_level : provenance_level;
  evidence_id : string;
}

type scc = {
  scc_id : string;
  members : string list;
  edges : edge list;
  is_cyclic : bool;
  callgraph_backed : bool;
}

type t

val residual_call_binding_edge :
  importer_module:string ->
  provider_module:string ->
  import_name:string ->
  export_name:string ->
  edge

val scc_groups : nodes:string list -> edges:edge list -> string list list

val compute : scc_id:string -> nodes:string list -> edges:edge list -> t

val callgraph_backed : t -> bool
val source : t -> string
val edges : t -> edge list
val sccs : t -> scc list

val edge_to_json : edge -> Yojson.Safe.t
val to_json : t -> Yojson.Safe.t
