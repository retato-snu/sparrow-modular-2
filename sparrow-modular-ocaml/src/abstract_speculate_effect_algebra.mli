(** Typed partial effect algebra for residual external-summary observations.

    This module is the construction authority for the effect-algebra v1
    boundary.  Callers can inspect effects and projections, but cannot build
    them without smart constructors and partial operations. *)

val schema_id : string

type memory_kind =
  | Memory_read
  | Memory_write
  | Memory_read_write

type domain =
  | Return
  | Memory of memory_kind
  | Alias
  | Heap
  | Struct_field
  | Array_segment
  | Taint
  | Product_pair

type observation =
  | Return_observation
  | Global_observation
  | Pointer_observation
  | Taint_observation
  | Product_pair_observation

type undefined_reason =
  | Incompatible_domain
  | Incompatible_provenance
  | Incompatible_path
  | Missing_alias_evidence
  | Lossy_heap_projection
  | Unsupported_observation
  | Invalid_composition_order
  | Taint_product_mismatch

type 'a operation_result =
  | Defined of 'a
  | Undefined of undefined_reason

type provenance
type t
type projection

val make_provenance :
  provider_module:string ->
  provider_source_hash:string ->
  provider_artifact_path:string ->
  provider_phase_index:int ->
  export_name:string ->
  provenance operation_result

val provenance_id : provenance -> string
val provenance_module : provenance -> string
val provenance_source_hash : provenance -> string
val provenance_artifact_path : provenance -> string
val provenance_phase_index : provenance -> int
val provenance_export_name : provenance -> string

val make_return :
  provenance:provenance ->
  return_location:string ->
  abstract_value:string ->
  evidence_path:string list ->
  t operation_result

val make_memory_transition :
  provenance:provenance ->
  kind:memory_kind ->
  location:string ->
  value:string ->
  alias_evidence:string option ->
  evidence_path:string list ->
  t operation_result

val make_alias :
  provenance:provenance ->
  source:string ->
  target:string ->
  evidence_path:string list ->
  t operation_result

val make_heap :
  provenance:provenance ->
  allocation:string ->
  location:string ->
  precise:bool ->
  evidence_path:string list ->
  t operation_result

val make_struct_field :
  provenance:provenance ->
  symbol:string ->
  location:string ->
  value:Yojson.Safe.t ->
  related_effect_ids:string list ->
  t operation_result

val make_taint :
  provenance:provenance ->
  source:string ->
  sink:string ->
  taint_state:string ->
  evidence_path:string list ->
  t operation_result

val make_product_pair :
  provenance:provenance ->
  left_effect:t ->
  right_effect:t ->
  evidence_path:string list ->
  t operation_result

val identity : provenance:provenance -> t

val effect_id : t -> string
val effect_domain : t -> domain
val effect_provenance : t -> provenance
val effect_path : t -> string list
val effect_evidence_path : t -> string list
val effect_payload : t -> (string * string) list

val projection_id : projection -> string
val projection_observation : projection -> observation
val projection_source_effect_id : projection -> string
val projection_source_provenance_id : projection -> string
val projection_evidence_path : projection -> string list

val compose : t -> t -> t operation_result
val join : t -> t -> t operation_result
val restrict : path:string list -> t -> t operation_result
val observe : observation -> t -> projection operation_result

val undefined_reason_to_string : undefined_reason -> string
val domain_to_string : domain -> string
val observation_to_string : observation -> string

val compose_identity_holds : t -> bool
val compose_associative_holds : t -> t -> t -> bool
val join_idempotent_holds : t -> bool
val join_commutative_holds : t -> t -> bool
val restrict_idempotent_holds : path:string list -> t -> bool
val projection_stable : observation -> t -> bool
