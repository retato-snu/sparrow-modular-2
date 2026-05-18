(***********************************************************************)
(* Typed scalar-call protocol for Abstract Speculate residual linking.  *)
(***********************************************************************)

module Json = Yojson.Safe
module Cell = Abstract_speculate_itv_residual_cell

let schema_id = "abstract-speculate-residual-scalar-call/v1"
let scalar_scope = "selected-sparrow-itv-scalar-only"

type scalar_value = {
  raw : string;
  canonical : Cell.itv_value;
}

type provider_return = {
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

type linked_derivation = {
  importer_module : string;
  importer_extern_root : string;
  import_name : string;
  provider_return : provider_return;
  linked_return_value : int;
}

let assoc_field name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match assoc_field name json with
  | Some (`String s) -> s
  | Some (`Int n) -> string_of_int n
  | _ -> ""

let int_field name json =
  match assoc_field name json with
  | Some (`Int n) -> n
  | Some (`String s) -> (try int_of_string (String.trim s) with Failure _ -> min_int)
  | _ -> min_int

let bool_field name json = match assoc_field name json with Some (`Bool b) -> b | _ -> false
let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []

let provider_return_effect_id ~provider_module ~export_name ~return_location =
  provider_module ^ ":" ^ export_name ^ ":return:" ^ return_location

let scalar_protocol_id p =
  p.provider_module ^ ":" ^ p.provider_source_hash ^ ":" ^ p.export_name ^ ":" ^
  p.return_node ^ ":" ^ p.return_location ^ ":" ^ p.effect_id

let canonical_value_json p = Cell.canonical_value_json p.scalar_value.canonical
let scalar_value_kind p = string_field "kind" (canonical_value_json p)

let cell_for_scalar ~return_node ~return_location ~abstract_return_value =
  let id = Cell.cell_id ~table:"provider_return" ~node:return_node ~location:return_location in
  match
    Cell.of_legacy_cell_json ~table:id.table ~node:id.node
      (`Assoc [
        "location", `String return_location;
        "value", `String abstract_return_value;
        "normalized_value", `String abstract_return_value;
      ])
  with
  | Some cell -> Ok cell
  | None -> Error "abstract_return_value is not a full-ITV scalar residual cell"

let value_agrees_with_singleton cell return_value =
  match Cell.value cell with
  | Cell.Singleton n -> n = return_value
  | Cell.Range (lo, hi) -> lo <= return_value && return_value <= hi
  | Cell.Top -> true
  | Cell.ExactNonNumeric _ | Cell.Opaque _ -> false

let make_provider_return
    ~provider_module
    ~provider_source_hash
    ~provider_artifact_path
    ~export_name
    ~return_node
    ~return_location
    ~effect_id
    ~provider_phase_index
    ~return_value
    ~abstract_return_value
    ~provider_row:_ =
  let expected_effect_id = provider_return_effect_id ~provider_module ~export_name ~return_location in
  if provider_module = "" then Error "provider_module is required"
  else if provider_source_hash = "" then Error "provider_source_hash is required"
  else if export_name = "" then Error "export_name is required"
  else if return_node = "" then Error "return_node is required"
  else if return_location = "" then Error "return_location is required"
  else if effect_id <> expected_effect_id then
    Error ("effect_id mismatch: expected " ^ expected_effect_id ^ ", got " ^ effect_id)
  else
    match cell_for_scalar ~return_node ~return_location ~abstract_return_value with
    | Error reason -> Error reason
    | Ok cell when not (value_agrees_with_singleton cell return_value) ->
        Error "abstract_return_value does not contain concrete return_value"
    | Ok cell ->
        Ok {
          provider_module;
          provider_source_hash;
          provider_artifact_path;
          export_name;
          return_node;
          return_location;
          effect_id;
          provider_phase_index;
          return_value;
          abstract_return_value;
          scalar_value = { raw = abstract_return_value; canonical = Cell.value cell };
        }

let make_linked_derivation
    ~importer_module
    ~importer_extern_root
    ~import_name
    ~linked_return_value
    ~provider_return =
  if importer_module = "" then Error "importer_module is required"
  else if importer_extern_root = "" then Error "importer_extern_root is required"
  else if linked_return_value <> provider_return.return_value then
    Error "linked_return_value does not match provider scalar return"
  else Ok { importer_module; importer_extern_root; import_name; provider_return; linked_return_value }

let add_fields extras json =
  let extra_keys = List.map fst extras in
  match json with
  | `Assoc fields -> `Assoc (extras @ List.filter (fun (key, _) -> not (List.mem key extra_keys)) fields)
  | _ -> `Assoc extras

let metadata_json p =
  `Assoc [
    "scalar_protocol_schema", `String schema_id;
    "scalar_protocol_scope", `String scalar_scope;
    "scalar_call_protocol_id", `String (scalar_protocol_id p);
    "value_model", `String Cell.value_model_id;
    "scalar_value_kind", `String (scalar_value_kind p);
    "canonical_value", canonical_value_json p;
    "provider_module", `String p.provider_module;
    "provider_source_hash", `String p.provider_source_hash;
    "provider_artifact_path", `String p.provider_artifact_path;
    "export_name", `String p.export_name;
    "return_node", `String p.return_node;
    "return_location", `String p.return_location;
    "effect_id", `String p.effect_id;
    "provider_phase_index", `Int p.provider_phase_index;
    "return_value", `Int p.return_value;
    "abstract_return_value", `String p.abstract_return_value;
    "excluded_domains", `List [`String "Oct"; `String "Taint"];
  ]

let protocol_fields p = [
  "scalar_protocol_schema", `String schema_id;
  "scalar_call_protocol_id", `String (scalar_protocol_id p);
  "scalar_value_kind", `String (scalar_value_kind p);
  "scalar_value", canonical_value_json p;
  "typed_scalar_metadata_valid", `Bool true;
  "typed_scalar_metadata", metadata_json p;
]

let return_effect_json p =
  add_fields (protocol_fields p @ [
    "effect_id", `String p.effect_id;
    "domain", `String "return";
    "symbol", `String p.export_name;
    "location", `String p.return_location;
    "normalized_location", `String p.return_location;
    "value", `Int p.return_value;
    "abstract_value", `Int p.return_value;
    "singleton_int", `Int p.return_value;
    "provider_module", `String p.provider_module;
    "provider_source_hash", `String p.provider_source_hash;
    "provider_artifact_path", `String p.provider_artifact_path;
    "provider_phase_index", `Int p.provider_phase_index;
    "derivation_source", `String "provider-stage2-output";
    "source_evidence_path", `String "provider_row.return";
    "witness_scope", `String "selected-sparrow-itv";
  ]) (`Assoc [])

let v1_extern_scalar_value_json p =
  add_fields (protocol_fields p @ [
    "name", `String p.export_name;
    "export_name", `String p.export_name;
    "provider_module", `String p.provider_module;
    "provider_source_hash", `String p.provider_source_hash;
    "provider_artifact_path", `String p.provider_artifact_path;
    "return_location", `String p.return_location;
    "effect_id", `String p.effect_id;
    "provider_phase_index", `Int p.provider_phase_index;
    "value", `Int p.return_value;
    "abstract_value", `String p.abstract_return_value;
    "source", `String "function-return-singleton";
  ]) (`Assoc [])

let function_return_summary_json p =
  add_fields (protocol_fields p @ [
    "function", `String p.export_name;
    "export_name", `String p.export_name;
    "provider_module", `String p.provider_module;
    "provider_source_hash", `String p.provider_source_hash;
    "provider_artifact_path", `String p.provider_artifact_path;
    "effect_id", `String p.effect_id;
    "provider_phase_index", `Int p.provider_phase_index;
    "return_node", `String p.return_node;
    "return_location", `String p.return_location;
    "return_value", `Int p.return_value;
    "abstract_return_value", `String p.abstract_return_value;
  ]) (`Assoc [])

let linked_derivation_metadata_json d =
  let p = d.provider_return in
  `Assoc [
    "scalar_protocol_schema", `String schema_id;
    "scalar_call_protocol_id", `String (scalar_protocol_id p);
    "importer_module", `String d.importer_module;
    "importer_extern_root", `String d.importer_extern_root;
    "import_name", `String d.import_name;
    "linked_return_value", `Int d.linked_return_value;
    "provider_return_metadata", metadata_json p;
  ]

let validation_ok = function Ok () -> true | Error _ -> false
let validation_reasons = function Ok () -> [] | Error reasons -> reasons
let validation_result_json result =
  `Assoc [
    "status", `String (if validation_ok result then "pass" else "fail");
    "reasons", `List (List.map (fun reason -> `String reason) (validation_reasons result));
  ]

let singleton_value_matches eff =
  match assoc_field "value" eff with
  | Some (`Int n) -> int_field "singleton_int" eff = n || int_field "singleton_int" eff = min_int
  | Some (`String s) -> s <> ""
  | _ -> false

let first_non_empty fields json =
  fields |> List.find_map (fun field ->
    match string_field field json with
    | "" -> None
    | value -> Some value)
  |> Option.value ~default:""

let agrees_when_present field metadata_field eff metadata =
  let top = string_field field eff in
  top = "" || string_field metadata_field metadata = top

let metadata_reasons eff =
  let metadata = match assoc_field "typed_scalar_metadata" eff with Some m -> m | None -> `Null in
  let add cond reason reasons = if cond then reasons else reason :: reasons in
  let export_name = first_non_empty ["symbol"; "export_name"; "name"; "function"] eff in
  []
  |> add (string_field "scalar_protocol_schema" eff = schema_id) "missing_or_wrong_scalar_protocol_schema"
  |> add (bool_field "typed_scalar_metadata_valid" eff) "typed_scalar_metadata_valid_false_or_missing"
  |> add (metadata <> `Null) "missing_typed_scalar_metadata"
  |> add (string_field "scalar_protocol_schema" metadata = schema_id) "metadata_schema_mismatch"
  |> add (string_field "scalar_call_protocol_id" eff <> "") "missing_scalar_call_protocol_id"
  |> add (string_field "scalar_call_protocol_id" metadata = string_field "scalar_call_protocol_id" eff) "metadata_protocol_id_mismatch"
  |> add (string_field "value_model" metadata = Cell.value_model_id) "metadata_value_model_mismatch"
  |> add (agrees_when_present "provider_module" "provider_module" eff metadata) "metadata_provider_module_mismatch"
  |> add (agrees_when_present "provider_source_hash" "provider_source_hash" eff metadata) "metadata_provider_source_hash_mismatch"
  |> add (agrees_when_present "provider_artifact_path" "provider_artifact_path" eff metadata) "metadata_provider_artifact_path_mismatch"
  |> add (string_field "export_name" metadata = export_name) "metadata_export_name_mismatch"
  |> add (agrees_when_present "location" "return_location" eff metadata) "metadata_return_location_mismatch"
  |> add (agrees_when_present "return_location" "return_location" eff metadata) "metadata_return_location_mismatch"
  |> add (agrees_when_present "effect_id" "effect_id" eff metadata) "metadata_effect_id_mismatch"
  |> add (int_field "provider_phase_index" eff = min_int || int_field "provider_phase_index" metadata = int_field "provider_phase_index" eff) "metadata_phase_index_mismatch"
  |> add (int_field "value" eff = min_int || int_field "return_value" metadata = int_field "value" eff) "metadata_return_value_mismatch"
  |> add (int_field "return_value" eff = min_int || int_field "return_value" metadata = int_field "return_value" eff) "metadata_return_value_mismatch"
  |> add (string_field "scalar_value_kind" metadata = string_field "scalar_value_kind" eff) "metadata_scalar_value_kind_mismatch"
  |> add (match assoc_field "scalar_value" eff with
          | Some scalar_value -> scalar_value = (match assoc_field "canonical_value" metadata with Some json -> json | None -> `Null)
          | None -> true) "scalar_value_metadata_mismatch"

let validate_return_effect_json eff =
  let expected_effect_id =
    provider_return_effect_id
      ~provider_module:(string_field "provider_module" eff)
      ~export_name:(string_field "symbol" eff)
      ~return_location:(string_field "location" eff)
  in
  let add cond reason reasons = if cond then reasons else reason :: reasons in
  let reasons = metadata_reasons eff in
  let reasons =
    reasons
    |> add (string_field "domain" eff = "return") "domain_not_return"
    |> add (string_field "derivation_source" eff = "provider-stage2-output") "derivation_source_mismatch"
    |> add (string_field "source_evidence_path" eff = "provider_row.return") "source_evidence_path_mismatch"
    |> add (string_field "witness_scope" eff = "selected-sparrow-itv") "witness_scope_mismatch"
    |> add (string_field "effect_id" eff = expected_effect_id) "effect_id_mismatch"
    |> add (string_field "location" eff <> "") "missing_return_location"
    |> add (string_field "provider_module" eff <> "") "missing_provider_module"
    |> add (string_field "provider_source_hash" eff <> "") "missing_provider_source_hash"
    |> add (string_field "symbol" eff <> "") "missing_export_symbol"
    |> add (singleton_value_matches eff) "scalar_value_not_singleton_int"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_v1_extern_scalar_value_json json =
  let add cond reason reasons = if cond then reasons else reason :: reasons in
  let reasons = metadata_reasons json in
  let reasons =
    reasons
    |> add (string_field "source" json = "function-return-singleton") "v1_source_mismatch"
    |> add (string_field "name" json <> "") "missing_v1_name"
    |> add (int_field "value" json <> min_int) "missing_v1_value"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_linked_derivation_json derivation =
  let return_effect = match assoc_field "return_effect" derivation with Some eff -> eff | None -> `Null in
  let external_summary = match assoc_field "external_summary" derivation with Some s -> s | None -> `Null in
  let summary_return = match list_field "return_effects" external_summary with eff :: _ -> eff | [] -> `Null in
  let return_validation = validate_return_effect_json return_effect in
  let metadata = match assoc_field "typed_scalar_metadata" return_effect with Some m -> m | None -> `Null in
  let add cond reason reasons = if cond then reasons else reason :: reasons in
  let reasons = validation_reasons return_validation in
  let reasons =
    reasons
    |> add (string_field "effect_reason" derivation = "linked-provider-return") "effect_reason_mismatch"
    |> add (string_field "derivation_source" derivation = "provider-stage2-output") "derivation_source_mismatch"
    |> add (string_field "external_summary_schema" derivation = "abstract-speculate-external-summary/v2") "external_summary_schema_mismatch"
    |> add (string_field "summary_api_status" derivation = "prototype-internal") "summary_api_status_mismatch"
    |> add (string_field "external_summary_effect_id" derivation = string_field "effect_id" return_effect) "derivation_effect_id_mismatch"
    |> add (return_effect = summary_return) "return_effect_not_structurally_equal_to_summary"
    |> add (int_field "linked_return_value" derivation = int_field "value" return_effect) "linked_return_value_mismatch"
    |> add (string_field "provider_module" derivation = string_field "provider_module" return_effect) "provider_module_mismatch"
    |> add (string_field "export_name" derivation = string_field "symbol" return_effect) "export_name_mismatch"
    |> add (string_field "return_location" derivation = string_field "location" return_effect) "return_location_mismatch"
    |> add (string_field "provider_source_hash" derivation = string_field "provider_source_hash" metadata) "provider_source_hash_mismatch"
    |> add (string_field "scalar_protocol_schema" derivation = schema_id) "derivation_scalar_protocol_schema_mismatch"
    |> add (string_field "scalar_call_protocol_id" derivation = string_field "scalar_call_protocol_id" return_effect) "derivation_protocol_id_mismatch"
    |> add (bool_field "typed_scalar_metadata_valid" derivation) "derivation_typed_scalar_metadata_valid_false_or_missing"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs
