(***********************************************************************)
(* Typed scalar-call protocol for Abstract Speculate residual linking.  *)
(***********************************************************************)

(** A narrow typed envelope for scalar return/call evidence shared by the
    residual linker, residual relation, and tests.  It intentionally wraps the
    existing full-ITV residual-cell value semantics and does not model Oct,
    Taint, provider discovery, scheduling, import resolution, or module
    traversal. *)

type scalar_value = private {
  raw : string;
  canonical : Abstract_speculate_itv_residual_cell.itv_value;
}

type provider_return = private {
  provider_module : string;
  provider_source_hash : string;
  provider_artifact_path : string;
  export_name : string;
  return_node : string;
  return_location : string;
  effect_id : string;
  provider_phase_index : int;
  return_value : int;
  abstract_return_value : string;
  scalar_value : scalar_value;
}

type linked_derivation = private {
  importer_module : string;
  importer_extern_root : string;
  import_name : string;
  provider_return : provider_return;
  linked_return_value : int;
}

val schema_id : string
val scalar_scope : string

val provider_return_effect_id :
  provider_module:string -> export_name:string -> return_location:string -> string

val make_provider_return :
  provider_module:string ->
  provider_source_hash:string ->
  provider_artifact_path:string ->
  export_name:string ->
  return_node:string ->
  return_location:string ->
  effect_id:string ->
  provider_phase_index:int ->
  return_value:int ->
  abstract_return_value:string ->
  provider_row:Yojson.Safe.t ->
  (provider_return, string) result

val make_linked_derivation :
  importer_module:string ->
  importer_extern_root:string ->
  import_name:string ->
  linked_return_value:int ->
  provider_return:provider_return ->
  (linked_derivation, string) result

val scalar_protocol_id : provider_return -> string
val scalar_value_kind : provider_return -> string
val metadata_json : provider_return -> Yojson.Safe.t
val return_effect_json : provider_return -> Yojson.Safe.t
val v1_extern_scalar_value_json : provider_return -> Yojson.Safe.t
val function_return_summary_json : provider_return -> Yojson.Safe.t
val linked_derivation_metadata_json : linked_derivation -> Yojson.Safe.t
val add_fields : (string * Yojson.Safe.t) list -> Yojson.Safe.t -> Yojson.Safe.t
val validation_result_json : (unit, string list) result -> Yojson.Safe.t

val validate_return_effect_json : Yojson.Safe.t -> (unit, string list) result
val validate_v1_extern_scalar_value_json : Yojson.Safe.t -> (unit, string list) result
val validate_linked_derivation_json : Yojson.Safe.t -> (unit, string list) result
val validation_ok : (unit, string list) result -> bool
val validation_reasons : (unit, string list) result -> string list
