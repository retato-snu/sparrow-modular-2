(***********************************************************************)
(* Typed ExternalSummary v3 memory-delta protocol.                    *)
(***********************************************************************)

(** Witness-bounded memory-delta evidence for Abstract Speculate residual
    linking.  This module is an encoding/validation boundary only: it does not
    discover providers, schedule residual work, invoke solvers, or model Oct or
    Taint domains. *)

type memory_domain = Global_write_read | Pointer_memory_effect

type memory_location = private {
  domain : memory_domain;
  raw_location : string;
  normalized_location : string;
  symbol : string;
  alias_key : string option;
}

type value_transition = private {
  read_value : string;
  write_value : string;
  read_canonical_value : Yojson.Safe.t;
  write_canonical_value : Yojson.Safe.t;
}

type provenance = private {
  provider_module : string;
  provider_source_hash : string;
  provider_artifact_path : string;
  provider_phase_index : int;
  source_evidence_path : string;
}

type delta = private {
  delta_id : string;
  location : memory_location;
  transition : value_transition;
  provenance : provenance;
  chain_id : string;
  chain_hash : string;
}

val external_summary_schema_id : string
val memory_delta_schema_id : string
val summary_api_status : string
val witness_scope : string

val domain_to_string : memory_domain -> string
val domain_of_string : string -> (memory_domain, string) result

val make_provider_delta :
  provider_module:string ->
  provider_source_hash:string ->
  provider_artifact_path:string ->
  provider_phase_index:int ->
  export_name:string ->
  domain:memory_domain ->
  raw_location:string ->
  summary_location:string ->
  symbol:string ->
  value:string ->
  source_evidence_path:string ->
  (delta, string) result

val delta_id : delta -> string
val chain_id : delta -> string
val delta_to_json : delta -> Yojson.Safe.t
val delta_chain_json : delta -> Yojson.Safe.t list
val compatibility_effect_json : delta -> Yojson.Safe.t

val validate_delta_json : Yojson.Safe.t -> (unit, string list) result
val validate_summary_json : Yojson.Safe.t -> (unit, string list) result
val validation_ok : (unit, string list) result -> bool
val validation_reasons : (unit, string list) result -> string list
val validation_result_json : (unit, string list) result -> Yojson.Safe.t

val relation_failure_reasons : string list
