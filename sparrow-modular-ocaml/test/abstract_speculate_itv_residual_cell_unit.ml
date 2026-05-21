module MetaSparse = Sparrow_modular_ocaml.Abstract_speculate_meta_sparse
module Cell = Sparrow_modular_ocaml.Abstract_speculate_itv_residual_cell

let expect cond msg = if not cond then failwith msg

let member = Yojson.Safe.Util.member

let string_field name json =
  match member name json with
  | `String s -> s
  | _ -> ""

let bool_field name json =
  match member name json with
  | `Bool b -> b
  | _ -> false

let int_field name json =
  match member name json with
  | `Int n -> n
  | _ -> 0

let cell_json ?(location="x") value =
  `Assoc [
    "location", `String location;
    "value", value;
    "normalized_value", value;
    "preserved", `String "legacy";
  ]

let row ?(node="n") ?(extra=`String "keep") cells =
  `Assoc [
    "node", `String node;
    "row_extra", extra;
    "memory", `List cells;
  ]

let parse_value value =
  match Cell.of_legacy_cell_json ~table:"output" ~node:"n" (cell_json value) with
  | Some cell -> Cell.value cell
  | None -> failwith "failed to parse test cell"

let expect_kind expected json =
  expect (string_field "kind" json = expected)
    ("expected canonical kind " ^ expected ^ ", got " ^ string_field "kind" json)

let () =
  begin match parse_value (`Int 7), parse_value (`String "([7,7], unit)") with
  | Cell.Singleton 7, Cell.Singleton 7 -> ()
  | _ -> failwith "singleton parsing failed"
  end;
  begin match parse_value (`String "([1, 4], unit)") with
  | Cell.Range (1, 4) -> ()
  | _ -> failwith "range parsing failed"
  end;
  begin match parse_value (`String "([-oo,+oo], unit)") with
  | Cell.Top -> ()
  | _ -> failwith "top parsing failed"
  end;
  begin match parse_value (`String "bot"), parse_value (`String "unknown-symbolic") with
  | Cell.ExactNonNumeric "bot", Cell.ExactNonNumeric "unknown-symbolic" -> ()
  | _ -> failwith "exact non-numeric parsing failed"
  end;
  begin match parse_value (`String "custom-token") with
  | Cell.Opaque "custom-token" -> ()
  | _ -> failwith "opaque parsing failed"
  end;
  let id = Cell.cell_id ~table:"output" ~node:"n" ~location:"x" in
  let bottom = Cell.bottom id in
  let bot_exact =
    match Cell.of_legacy_cell_json ~table:"output" ~node:"n" (cell_json (`String "bot")) with
    | Some cell -> cell
    | None -> failwith "bot cell parse failed"
  in
  expect (Cell.leq bottom bot_exact) "mathematical bottom should be <= exact bot";
  expect (not (Cell.leq bot_exact bottom)) "exact bot must be distinct from mathematical bottom";
  let singleton n =
    match Cell.of_legacy_cell_json ~table:"output" ~node:"n" (cell_json (`Int n)) with
    | Some cell -> cell
    | None -> failwith "singleton cell parse failed"
  in
  let range = Cell.join (singleton 1) (singleton 4) in
  begin match Cell.value range with
  | Cell.Range (1, 4) -> ()
  | _ -> failwith "join did not merge finite singleton evidence into a range"
  end;
  expect (Cell.leq (singleton 2) range) "finite singleton should be <= joined range";
  expect (Cell.covers ~residual:range ~origin:(singleton 3)) "range should cover contained singleton";
  expect (not (Cell.covers ~residual:(singleton 3) ~origin:range)) "singleton should not cover wider range";
  expect (Cell.covers_legacy_values ~residual:"([-oo, +oo], unit)" ~origin:"([1,2], unit)")
    "top syntax should cover finite origin interval";
  expect (not (Cell.covers_legacy_values ~residual:"custom-a" ~origin:"custom-b"))
    "opaque values should not cross-cover";
  expect_kind "range" (Cell.canonical_value_json_of_legacy_string "([1,2], unit)");
  expect_kind "exact-non-numeric" (Cell.canonical_value_json_of_legacy_string "unknown");
  let legacy = Cell.to_legacy_cell_json range in
  expect (string_field "location" legacy = "x") "legacy JSON location not preserved";
  expect (string_field "value" legacy <> "") "legacy JSON value missing";
  let metadata = Cell.metadata_json range in
  expect (string_field "value_model" metadata = Cell.value_model_id) "metadata value model missing";
  expect (string_field "cell_id" metadata = "output:n:x") "metadata cell id missing";
  expect (string_field "table" metadata = "output") "metadata table missing";
  expect (string_field "node" metadata = "n") "metadata node missing";
  expect (string_field "location" metadata = "x") "metadata location missing";
  expect (not (bool_field "is_lattice_bottom" metadata)) "non-bottom metadata marked bottom";
  let canonical = member "canonical_value" metadata in
  expect (string_field "kind" canonical = "range") "metadata canonical kind missing";
  expect (int_field "lo" canonical = 1 && int_field "hi" canonical = 4)
    "metadata canonical bounds missing";
  expect (string_field "raw" canonical = "([1,4], typed-itv-residual-cell/v1)")
    "metadata canonical raw missing";
  let source_metadata =
    MetaSparse.typed_metadata_for_value ~table:"output" ~node:"n"
      ~location:"x" ~value:"([1,4], typed-itv-residual-cell/v1)"
  in
  expect
    (MetaSparse.itv_mem_typed_metadata_consistent ~table:"output" ~node:"n"
       ~location:"x" ~value:"([1,4], typed-itv-residual-cell/v1)"
       source_metadata)
    "coverage metadata consistency should accept exact source metadata";
  expect
    (not
       (MetaSparse.itv_mem_typed_metadata_consistent
          ~table:"final_output_table" ~node:"n" ~location:"x"
          ~value:"([1,4], typed-itv-residual-cell/v1)" source_metadata))
    "coverage metadata consistency must not silently retable source metadata";
  let wrong_location_metadata =
    match source_metadata with
    | `Assoc fields ->
        `Assoc (("location", `String "wrong") :: List.remove_assoc "location" fields)
    | _ -> source_metadata
  in
  expect
    (not
       (MetaSparse.itv_mem_typed_metadata_consistent ~table:"output" ~node:"n"
          ~location:"x" ~value:"([1,4], typed-itv-residual-cell/v1)"
          wrong_location_metadata))
    "coverage metadata consistency must reject malformed source metadata identity";
  let wrong_canonical_metadata =
    match source_metadata with
    | `Assoc fields ->
        `Assoc
          (("canonical_value", Cell.canonical_value_json_of_legacy_string "([9,9], unit)")
          :: List.remove_assoc "canonical_value" fields)
    | _ -> source_metadata
  in
  expect
    (not
       (MetaSparse.itv_mem_typed_metadata_consistent ~table:"output" ~node:"n"
          ~location:"x" ~value:"([1,4], typed-itv-residual-cell/v1)"
          wrong_canonical_metadata))
    "coverage metadata consistency must reject malformed source metadata value";
  let api_metadata =
    `Assoc
      (MetaSparse.api_extra_fields
         ~function_name:"memcpy"
         ~abstract_effect:
           "source array memory copied to destination locations; memcpy return destination is tracked only when assigned")
  in
  expect (string_field "api_residual_function" api_metadata = "memcpy")
    "api residual function missing";
  expect
    (string_field "api_baseline_source" api_metadata
    = "sparrow-modular-ocaml/src/itvSem.ml:482-492; sparrow-modular-ocaml/src/apiSem.ml:70-75")
    "api baseline source missing";
  expect
    (string_field "api_abstract_effect" api_metadata
    = "source array memory copied to destination locations; memcpy return destination is tracked only when assigned")
    "api abstract effect missing";
  expect
    (string_field "api_semantic_provenance" api_metadata = "residual-api-model-coverage/slice-1")
    "api semantic provenance missing";
  expect (bool_field "covered_api" api_metadata) "api covered flag missing";
  expect (not (bool_field "unsupported_api_coverage" api_metadata))
    "api unsupported coverage flag set";
  let target = id in
  let old_row = row [cell_json (`Int 1); cell_json ~location:"z" (`String "z-preserved")] in
  let new_row = row ~extra:(`String "new-row-field") [cell_json (`Int 4); cell_json ~location:"z" (`String "z-new")] in
  let joined_row = Cell.join_row_for_target_cell ~target ~old_row ~new_row in
  let target_cell =
    match member "memory" joined_row with
    | `List (first :: _) -> first
    | _ -> failwith "joined row did not retain memory"
  in
  expect (string_field "value" target_cell = "([1,4], typed-itv-residual-cell/v1)")
    "row adapter did not write joined typed range for target cell";
  expect (string_field "row_extra" joined_row = "new-row-field")
    "row adapter did not preserve non-target row fields from new row";
  expect (Cell.leq_row_for_target_cell ~target ~left_row:(row [cell_json (`Int 2)]) ~right_row:joined_row)
    "row leq did not use typed target-cell containment";
  print_endline "abstract_speculate_itv_residual_cell_unit: PASS"
