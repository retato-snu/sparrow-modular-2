(***********************************************************************)
(* Projection-only JSON schema for typed ExternalSummary effects.       *)
(***********************************************************************)

val schema_id : string
val summary_api_status : string
val projection_status : string

val effect_to_json : Abstract_speculate_effect_algebra.t -> Yojson.Safe.t
val projection_to_json : Abstract_speculate_effect_algebra.projection -> Yojson.Safe.t

val defined_effect_to_json :
  Abstract_speculate_effect_algebra.t Abstract_speculate_effect_algebra.operation_result ->
  (Yojson.Safe.t, Abstract_speculate_effect_algebra.undefined_reason) result

val serialize_projection_result :
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result ->
  (Yojson.Safe.t, Abstract_speculate_effect_algebra.undefined_reason) result

val summary_to_json :
  effects:Abstract_speculate_effect_algebra.t list ->
  projections:Abstract_speculate_effect_algebra.projection list ->
  legacy_projection:Yojson.Safe.t ->
  Yojson.Safe.t

val validate_defined_artifact_json : Yojson.Safe.t -> (unit, string list) result
val validate_projection_json : Yojson.Safe.t -> (unit, string list) result
val validation_ok : (unit, string list) result -> bool
val validation_reasons : (unit, string list) result -> string list
val validation_result_json : (unit, string list) result -> Yojson.Safe.t
