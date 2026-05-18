(***********************************************************************)
(* Shared typed ITV residual cells for Abstract Speculate residual PE.  *)
(***********************************************************************)

(** First-pass typed residual ITV cell/lattice boundary shared by the
    residual solver and full ITV relation.  The representation is abstract so
    later Sparrow ITV refinements do not become a public schema contract. *)

type cell_id = private {
  table : string;
  node : string;
  location : string;
}

type itv_value =
  | Singleton of int
  | Range of int * int
  | Top
  | ExactNonNumeric of string
      (** Legacy bot/empty/unknown/symbolic data retained as exact values;
          this is distinct from mathematical lattice bottom. *)
  | Opaque of string

type cell

val value_model_id : string
val join_id : string
val leq_id : string
val relation_adapter_id : string

val cell_id : table:string -> node:string -> location:string -> cell_id
val cell_id_to_string : cell_id -> string

val of_legacy_cell_json : table:string -> node:string -> Yojson.Safe.t -> cell option
val of_solver_row : target:cell_id -> Yojson.Safe.t -> cell option
val to_legacy_cell_json : cell -> Yojson.Safe.t

val bottom : cell_id -> cell
val value : cell -> itv_value
val canonical_value_json : itv_value -> Yojson.Safe.t
val canonical_value_json_of_legacy_string : string -> Yojson.Safe.t
val metadata_json : cell -> Yojson.Safe.t

val join : cell -> cell -> cell
val leq : cell -> cell -> bool
val covers : residual:cell -> origin:cell -> bool

val join_row_for_target_cell :
  target:cell_id -> old_row:Yojson.Safe.t -> new_row:Yojson.Safe.t -> Yojson.Safe.t
val leq_row_for_target_cell :
  target:cell_id -> left_row:Yojson.Safe.t -> right_row:Yojson.Safe.t -> bool
val covers_legacy_values : residual:string -> origin:string -> bool
