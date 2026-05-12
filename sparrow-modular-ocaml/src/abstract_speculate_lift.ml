(***********************************************************************)
(* Explicit lifting helpers for Abstract Speculate MetaOCaml residuals. *)
(***********************************************************************)

let lift_string (s : string) = .<s>.
let lift_json (j : Yojson.Safe.t) = .<j>.
let lift_json_list (xs : Yojson.Safe.t list) = .<xs>.
let lift_string_list (xs : string list) = .<xs>.

let lift_location_id (loc : string) = .<loc>.
let lift_node_id (node : string) = .<node>.
let lift_interval_value (value : string) = .<value>.
let lift_product_value (value : string) = .<value>.
let lift_sparse_fragment (fragment : Yojson.Safe.t) = .<fragment>.
let lift_stage2_input (input : Abstract_speculate_stage_types.stage2_input) = .<input>.
