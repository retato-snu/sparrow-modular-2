(***********************************************************************)
(* Static-projection equality for Abstract Speculate staged sparse PE.  *)
(***********************************************************************)

module T = Abstract_speculate_stage_types

type 'a projection = {
  static_rows : Yojson.Safe.t list;
  residual_values : 'a T.ps list;
}

let sort_json rows =
  rows |> List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b))

let static_projection rows = sort_json rows

let equal_rows left right =
  static_projection left = static_projection right

let equal_static_projection left right =
  equal_rows left.static_rows right.static_rows

let residual_obligation_count projection =
  List.fold_left (fun n -> function T.D _ -> n + 1 | T.S _ -> n) 0 projection.residual_values

let static_value_count projection =
  List.fold_left (fun n -> function T.S _ -> n + 1 | T.D _ -> n) 0 projection.residual_values

let convergence_witness left right =
  `Assoc [
    "equality", `String "static-projection";
    "ignores_residual_code_structure", `Bool true;
    "static_projection_equal", `Bool (equal_static_projection left right);
    "left_static_row_count", `Int (List.length left.static_rows);
    "right_static_row_count", `Int (List.length right.static_rows);
    "left_residual_obligation_count", `Int (residual_obligation_count left);
    "right_residual_obligation_count", `Int (residual_obligation_count right);
    "left_static_value_count", `Int (static_value_count left);
    "right_static_value_count", `Int (static_value_count right);
  ]
