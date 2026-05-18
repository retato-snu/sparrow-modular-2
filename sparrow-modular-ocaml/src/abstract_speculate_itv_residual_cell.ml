(***********************************************************************)
(* Shared typed ITV residual cells for Abstract Speculate residual PE.  *)
(***********************************************************************)

module Json = Yojson.Safe

let value_model_id = "typed-itv-residual-cell/v1"
let join_id = "typed-itv-join/v1"
let leq_id = "typed-itv-leq/v1"
let relation_adapter_id = "typed-itv-relation-adapter/v1"

type cell_id = {
  table : string;
  node : string;
  location : string;
}

type itv_value =
  | Singleton of int
  | Range of int * int
  | Top
  | ExactNonNumeric of string
  | Opaque of string

type cell = {
  id : cell_id;
  value : itv_value;
  raw : string;
  is_bottom : bool;
}

let member = Yojson.Safe.Util.member

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field_opt name json =
  match assoc_field name json with
  | Some (`String s) -> Some s
  | Some (`Int n) -> Some (string_of_int n)
  | _ -> None

let cell_id ~table ~node ~location = { table; node; location }

let cell_id_to_string id = id.table ^ ":" ^ id.node ^ ":" ^ id.location

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let int_of_trimmed s =
  try Some (int_of_string (String.trim s)) with Failure _ -> None

let parse_interval_value value =
  let value = String.trim value in
  match int_of_trimmed value with
  | Some n -> Some (n, n)
  | None ->
      let prefix = "([" in
      let prefix_len = String.length prefix in
      if String.length value <= prefix_len || String.sub value 0 prefix_len <> prefix then None
      else
        try
          let comma = String.index_from value prefix_len ',' in
          let close = String.index_from value (comma + 1) ']' in
          let lo_s = String.trim (String.sub value prefix_len (comma - prefix_len)) in
          let hi_s = String.trim (String.sub value (comma + 1) (close - comma - 1)) in
          match lo_s, hi_s with
          | "-oo", _ | _, "+oo" -> None
          | _ ->
              begin match int_of_trimmed lo_s, int_of_trimmed hi_s with
              | Some lo, Some hi -> Some (lo, hi)
              | _ -> None
              end
        with Not_found -> None

let is_top_syntax trimmed =
  contains trimmed "[-oo,+oo]" || contains trimmed "[-oo, +oo]"

let exact_non_numeric_syntax trimmed =
  let lower = String.lowercase_ascii trimmed in
  trimmed = "" ||
  contains lower "bot" ||
  contains lower "empty" ||
  contains lower "unknown" ||
  contains lower "symbolic"

let itv_value_of_raw raw =
  let trimmed = String.trim raw in
  if is_top_syntax trimmed then Top
  else
    match parse_interval_value trimmed with
    | Some (lo, hi) when lo = hi -> Singleton lo
    | Some (lo, hi) -> Range (min lo hi, max lo hi)
    | None when exact_non_numeric_syntax trimmed -> ExactNonNumeric trimmed
    | None -> Opaque trimmed

let raw_of_value = function
  | Singleton n -> string_of_int n
  | Range (lo, hi) -> "([" ^ string_of_int lo ^ "," ^ string_of_int hi ^ "], " ^ value_model_id ^ ")"
  | Top -> "([-oo,+oo], " ^ value_model_id ^ ")"
  | ExactNonNumeric raw -> raw
  | Opaque raw -> raw

let make_cell ?(is_bottom=false) id value =
  { id; value; raw = raw_of_value value; is_bottom }

let value cell = cell.value

let bottom id =
  { id; value = ExactNonNumeric "__typed_itv_bottom__"; raw = "__typed_itv_bottom__"; is_bottom = true }

let canonical_value_json value =
  match value with
  | Singleton n -> `Assoc ["kind", `String "singleton"; "lo", `Int n; "hi", `Int n; "raw", `String (raw_of_value value)]
  | Range (lo, hi) -> `Assoc ["kind", `String "range"; "lo", `Int lo; "hi", `Int hi; "raw", `String (raw_of_value value)]
  | Top -> `Assoc ["kind", `String "top"; "raw", `String (raw_of_value value)]
  | ExactNonNumeric raw -> `Assoc ["kind", `String "exact-non-numeric"; "raw", `String raw]
  | Opaque raw -> `Assoc ["kind", `String "opaque"; "raw", `String raw]

let canonical_value_json_of_legacy_string raw =
  canonical_value_json (itv_value_of_raw raw)

let metadata_json cell =
  `Assoc [
    "value_model", `String value_model_id;
    "cell_id", `String (cell_id_to_string cell.id);
    "table", `String cell.id.table;
    "node", `String cell.id.node;
    "location", `String cell.id.location;
    "is_lattice_bottom", `Bool cell.is_bottom;
    "canonical_value", canonical_value_json cell.value;
  ]

let with_assoc_fields json extras =
  match json with
  | `Assoc fields ->
      let extra_keys = List.map fst extras in
      `Assoc (extras @ List.filter (fun (key, _) -> not (List.mem key extra_keys)) fields)
  | _ -> `Assoc extras

let to_legacy_cell_json cell =
  `Assoc [
    "location", `String cell.id.location;
    "value", `String cell.raw;
    "normalized_value", `String cell.raw;
  ]

let finite_bounds = function
  | Singleton n -> Some (n, n)
  | Range (lo, hi) -> Some (lo, hi)
  | _ -> None

let values_equal left right =
  match left, right with
  | Singleton a, Singleton b -> a = b
  | Range (alo, ahi), Range (blo, bhi) -> alo = blo && ahi = bhi
  | Top, Top -> true
  | ExactNonNumeric a, ExactNonNumeric b -> a = b
  | Opaque a, Opaque b -> a = b
  | _ -> false

let leq_value left right =
  match finite_bounds left, finite_bounds right with
  | Some (llo, lhi), Some (rlo, rhi) -> rlo <= llo && lhi <= rhi
  | Some _, None -> (match right with Top -> true | _ -> false)
  | None, _ -> values_equal left right

let join_value left right =
  if values_equal left right then left
  else
    match finite_bounds left, finite_bounds right with
    | Some (llo, lhi), Some (rlo, rhi) -> Range (min llo rlo, max lhi rhi)
    | _, _ ->
        begin match left, right with
        | Top, _ | _, Top -> Top
        | _ -> Opaque ("typed-conflict(" ^ raw_of_value left ^ "|" ^ raw_of_value right ^ ")")
        end

let same_cell_id left right =
  left.table = right.table && left.node = right.node && left.location = right.location

let join left right =
  if left.is_bottom then right
  else if right.is_bottom then left
  else if same_cell_id left.id right.id then make_cell left.id (join_value left.value right.value)
  else make_cell left.id (Opaque ("typed-cell-id-conflict(" ^ cell_id_to_string left.id ^ "|" ^ cell_id_to_string right.id ^ ")"))

let leq left right =
  left.is_bottom ||
  ((not right.is_bottom) && same_cell_id left.id right.id && leq_value left.value right.value)

let covers ~residual ~origin = leq origin residual

let cell_raw_value json =
  match string_field_opt "value" json with
  | Some s -> Some s
  | None -> string_field_opt "normalized_value" json

let of_legacy_cell_json ~table ~node json =
  match string_field_opt "location" json, cell_raw_value json with
  | Some location, Some raw ->
      let id = cell_id ~table ~node ~location in
      Some { id; value = itv_value_of_raw raw; raw = String.trim raw; is_bottom = false }
  | _ -> None

let find_target_cell target row =
  match member "memory" row with
  | `List cells ->
      cells |> List.find_opt (fun cell -> string_field_opt "location" cell = Some target.location)
  | _ when string_field_opt "location" row = Some target.location -> Some row
  | _ -> None

let of_solver_row ~target row =
  match find_target_cell target row with
  | Some cell -> of_legacy_cell_json ~table:target.table ~node:target.node cell
  | None ->
      begin match cell_raw_value row with
      | Some raw -> Some { id = target; value = itv_value_of_raw raw; raw = String.trim raw; is_bottom = false }
      | None -> None
      end

let legacy_cell_for_existing_json cell existing =
  with_assoc_fields existing [
    "location", `String cell.id.location;
    "value", `String cell.raw;
    "normalized_value", `String cell.raw;
  ]

let replace_target_cell target joined row =
  match row with
  | `Assoc fields ->
      begin match List.assoc_opt "memory" fields with
      | Some (`List cells) ->
          let replaced = ref false in
          let cells =
            cells |> List.map (fun cell ->
              if string_field_opt "location" cell = Some target.location then begin
                replaced := true;
                legacy_cell_for_existing_json joined cell
              end else cell)
          in
          let cells = if !replaced then cells else to_legacy_cell_json joined :: cells in
          with_assoc_fields row ["memory", `List cells]
      | _ -> row
      end
  | _ -> row

let join_row_for_target_cell ~target ~old_row ~new_row =
  match of_solver_row ~target old_row, of_solver_row ~target new_row with
  | Some old_cell, Some new_cell -> replace_target_cell target (join old_cell new_cell) new_row
  | _ -> new_row

let leq_row_for_target_cell ~target ~left_row ~right_row =
  match of_solver_row ~target left_row, of_solver_row ~target right_row with
  | Some left_cell, Some right_cell -> leq left_cell right_cell
  | _ -> Json.to_string left_row = Json.to_string right_row

let covers_legacy_values ~residual ~origin =
  let id = cell_id ~table:"legacy" ~node:"legacy" ~location:"legacy" in
  let residual = { id; value = itv_value_of_raw residual; raw = String.trim residual; is_bottom = false } in
  let origin = { id; value = itv_value_of_raw origin; raw = String.trim origin; is_bottom = false } in
  covers ~residual ~origin
