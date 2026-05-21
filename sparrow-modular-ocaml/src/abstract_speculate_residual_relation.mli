(***********************************************************************)
(* Prototype/internal selected-observation relation for residual PE.    *)
(***********************************************************************)

(** Prototype/internal API for the witness-bounded selected-observation
    relation used by residual-linking PE checkers. This interface is not a
    stable public residual-linker API and does not claim arbitrary-C semantic
    equivalence. *)

type observation_domain =
  | Return_value
  | Global_value
  | Pointer_write
  | Memory_location
  | Call_effect
  | Provider_binding
  | Phase_ordering
  | Provenance

type direction = Residual_to_oracle | Oracle_to_residual

type provenance = {
  source : string;
  path : string;
}

type summary_observation = {
  domain : observation_domain;
  witness_id : string;
  symbol : string;
  location : string;
  abstract_value : string;
  normalized_value : Yojson.Safe.t;
  provenance : provenance list;
}

type relation_failure = {
  domain : observation_domain;
  direction : direction;
  reason : string;
  evidence_path : string;
}

type comparison = private {
  status : string;
  residual_observations : Yojson.Safe.t list;
  oracle_observations : Yojson.Safe.t list;
  residual_to_oracle : Yojson.Safe.t;
  oracle_to_residual : Yojson.Safe.t;
  failures : relation_failure list;
}

val return_location : string -> string
val parse_interval_value : string -> (int * int) option
val parse_singleton_interval_value : string -> int option
val interval_contains_value : string -> int -> bool
val contains : string -> string -> bool
val selected_observation_relation_json : witness_id:string -> residual:Yojson.Safe.t -> oracle:Yojson.Safe.t -> Yojson.Safe.t
val full_itv_semantic_relation_json : witness_id:string -> residual:Yojson.Safe.t -> oracle:Yojson.Safe.t -> Yojson.Safe.t
val full_itv_required_fields : string list
val full_itv_relation_missing_required_fields : Yojson.Safe.t -> string list
val full_itv_relation_contract_json : Yojson.Safe.t -> Yojson.Safe.t
val full_itv_relation_has_required_fields : Yojson.Safe.t -> bool
val primary_linkage_ok : Yojson.Safe.t -> bool
val primary_linkage_check_json : Yojson.Safe.t -> Yojson.Safe.t
val oracle_suite_obligations :
  source_guard_obligation:(string -> Yojson.Safe.t) ->
  string -> string -> Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t list
val return_observations : string -> Yojson.Safe.t -> Yojson.Safe.t -> Yojson.Safe.t list * Yojson.Safe.t list
val observation_has_provenance : Yojson.Safe.t -> bool
val observations_have_provenance : Yojson.Safe.t list -> bool
val obligation_passes : Yojson.Safe.t list -> bool
val witness_pass_status : Yojson.Safe.t list -> Yojson.Safe.t list -> Yojson.Safe.t list -> bool
