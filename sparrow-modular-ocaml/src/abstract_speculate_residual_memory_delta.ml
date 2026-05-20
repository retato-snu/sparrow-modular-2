(***********************************************************************)
(* Typed ExternalSummary v3 memory-delta protocol.                    *)
(***********************************************************************)

module Cell = Abstract_speculate_itv_residual_cell

let external_summary_schema_id = "abstract-speculate-external-summary/v3"
let memory_delta_schema_id = "abstract-speculate-external-summary-memory-delta/v3"
let taint_protocol_schema_id = "abstract-speculate-taint-product-component/v1"
let product_pair_schema_id = "abstract-speculate-itv-taint-product-pair/v1"
let summary_api_status = "prototype-internal"
let witness_scope = "selected-sparrow-itv"

let relation_failure_reasons = [
  "memory_delta_role_mismatch";
  "memory_delta_location_mismatch";
  "memory_delta_value_transition_mismatch";
  "memory_delta_provenance_mismatch";
  "memory_delta_chain_missing";
  "taint_product_schema_mismatch";
  "taint_product_component_mismatch";
  "taint_product_relation_mismatch";
  "taint_product_chain_missing";
]

let taint_product_schema_id = "abstract-speculate-taint-product-evidence/v1"

type memory_domain = Global_write_read | Pointer_memory_effect | Taint_product_component

type taint_state = Taint_bottom | Untainted | Tainted of string

type taint_product_evidence = {
  taint_witness_id : string;
  taint_source : string;
  taint_sink : string;
  taint_state : taint_state;
  taint_semantic_relation : string;
  related_residual_location : string;
  itv_observable_value : string;
  evidence_paths : string list;
  taint_chain_id : string;
}

type memory_location = {
  domain : memory_domain;
  raw_location : string;
  normalized_location : string;
  symbol : string;
  alias_key : string option;
}

type value_transition = {
  read_value : string;
  write_value : string;
  read_canonical_value : Yojson.Safe.t;
  write_canonical_value : Yojson.Safe.t;
}

type provenance = {
  provider_module : string;
  provider_source_hash : string;
  provider_artifact_path : string;
  provider_phase_index : int;
  source_evidence_path : string;
}

type delta = {
  delta_id : string;
  location : memory_location;
  transition : value_transition;
  provenance : provenance;
  chain_id : string;
  chain_hash : string;
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

let list_field name json = match assoc_field name json with Some (`List xs) -> xs | _ -> []
let bool_field name json = match assoc_field name json with Some (`Bool b) -> b | _ -> false

let starts_with s prefix =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let bounded_taint_component_schema_id = "bounded-taint-product-component/v1"

let domain_to_string = function
  | Global_write_read -> "global-write-read"
  | Pointer_memory_effect -> "pointer-memory-effect"
  | Taint_product_component -> "Taint"

let domain_of_string = function
  | "global-write-read" -> Ok Global_write_read
  | "pointer-memory-effect" -> Ok Pointer_memory_effect
  | "Taint" | "taint" | "bounded-taint-product-component" ->
      Error "Taint is a bounded product component, not a memory delta domain"
  | "Oct" | "oct" -> Error "unsupported memory delta domain: Oct"
  | other -> Error ("unsupported memory delta domain: " ^ other)

let taint_state_to_string = function
  | Taint_bottom -> "bottom"
  | Untainted -> "untainted"
  | Tainted source -> "tainted:" ^ source

let taint_state_of_string = function
  | "bottom" -> Ok Taint_bottom
  | "untainted" -> Ok Untainted
  | s when starts_with s "tainted:" && String.length s > String.length "tainted:" ->
      Ok (Tainted (String.sub s (String.length "tainted:") (String.length s - String.length "tainted:")))
  | other -> Error ("unsupported taint state: " ^ other)

let canonical_value_json value = Cell.canonical_value_json_of_legacy_string value

let make_taint_component_json
    ~witness_id
    ~symbol
    ~location
    ~taint_state
    ~source_evidence_path
    ~related_effect_id =
  `Assoc [
    "taint_protocol_schema", `String taint_protocol_schema_id;
    "domain", `String "Taint";
    "witness_scope", `String witness_scope;
    "taint_witness_id", `String witness_id;
    "symbol", `String symbol;
    "location", `String location;
    "taint_state", `String taint_state;
    "semantic_relation", `String "provider-return-taint-flow";
    "source_evidence_path", `String source_evidence_path;
    "related_effect_id", `String related_effect_id;
    "metadata_only", `Bool false;
  ]

let make_product_pair_evidence_json ~witness_id ~itv_value ~taint_component =
  `Assoc [
    "product_pair_schema", `String product_pair_schema_id;
    "witness_scope", `String witness_scope;
    "taint_witness_id", `String witness_id;
    "product_components", `List [`String "Itv"; `String "Taint"];
    "itv_component", `Assoc [
      "domain", `String "Itv";
      "value", `String itv_value;
      "canonical_value", canonical_value_json itv_value;
    ];
    "taint_component", taint_component;
    "taint_semantic_relation", `String "provider-return-taint-flow";
    "semantic_relation_status", `String "pass";
    "negative_case_contract", `List [
      `String "taint evidence omitted";
      `String "taint evidence empty";
      `String "taint evidence unrelated to return effect";
      `String "taint evidence metadata-only";
    ];
  ]

let deterministic_hash fields = String.concat ":" fields

let make_taint_product_evidence
    ~taint_witness_id
    ~taint_source
    ~taint_sink
    ~taint_state
    ~taint_semantic_relation
    ~related_residual_location
    ~itv_observable_value
    ~evidence_paths =
  if taint_witness_id = "" then Error "taint_witness_id is required"
  else if taint_source = "" then Error "taint_source is required"
  else if taint_sink = "" then Error "taint_sink is required"
  else if taint_semantic_relation = "" then Error "taint_semantic_relation is required"
  else if related_residual_location = "" then Error "related_residual_location is required"
  else if itv_observable_value = "" then Error "itv_observable_value is required"
  else if evidence_paths = [] then Error "evidence_paths is required"
  else
    let taint_chain_id =
      deterministic_hash ["taint-product-chain"; taint_witness_id; taint_source; taint_sink; related_residual_location]
    in
    Ok {
      taint_witness_id; taint_source; taint_sink; taint_state; taint_semantic_relation;
      related_residual_location; itv_observable_value; evidence_paths; taint_chain_id;
    }

let taint_chain_json evidence = [
  `Assoc [
    "index", `Int 0;
    "role", `String "source";
    "location", `String evidence.taint_source;
    "taint_state", `String (taint_state_to_string evidence.taint_state);
    "chain_id", `String evidence.taint_chain_id;
  ];
  `Assoc [
    "index", `Int 1;
    "role", `String "linker";
    "location", `String evidence.related_residual_location;
    "taint_state", `String (taint_state_to_string evidence.taint_state);
    "chain_id", `String evidence.taint_chain_id;
  ];
  `Assoc [
    "index", `Int 2;
    "role", `String "sink";
    "location", `String evidence.taint_sink;
    "taint_state", `String (taint_state_to_string evidence.taint_state);
    "chain_id", `String evidence.taint_chain_id;
  ];
]

let taint_product_evidence_to_json evidence =
  `Assoc [
    "taint_product_schema", `String taint_product_schema_id;
    "taint_witness_id", `String evidence.taint_witness_id;
    "taint_semantic_relation", `String evidence.taint_semantic_relation;
    "product_components", `List [`String "Itv"; `String "Taint"];
    "taint_source", `String evidence.taint_source;
    "taint_sink", `String evidence.taint_sink;
    "taint_state", `String (taint_state_to_string evidence.taint_state);
    "related_residual_location", `String evidence.related_residual_location;
    "itv_observable", `Assoc [
      "location", `String evidence.related_residual_location;
      "value", `String evidence.itv_observable_value;
      "component", `String "Itv";
    ];
    "metadata_only", `Bool false;
    "evidence_paths", `List (List.map (fun path -> `String path) evidence.evidence_paths);
    "taint_chain_id", `String evidence.taint_chain_id;
    "taint_chain", `List (taint_chain_json evidence);
    "bounded_support", `String "named-witness-only";
    "non_claims", `List [
      `String "no general Taint domain parity";
      `String "no Oct or OctImpact semantics";
      `String "no alarm/report PE";
    ];
  ]

let make_provider_delta
    ~provider_module
    ~provider_source_hash
    ~provider_artifact_path
    ~provider_phase_index
    ~export_name
    ~domain
    ~raw_location
    ~summary_location
    ~symbol
    ~value
    ~source_evidence_path =
  if provider_module = "" then Error "provider_module is required"
  else if provider_source_hash = "" then Error "provider_source_hash is required"
  else if export_name = "" then Error "export_name is required"
  else if raw_location = "" then Error "raw_location is required"
  else if summary_location = "" then Error "summary_location is required"
  else if value = "" then Error "value is required"
  else if source_evidence_path = "" then Error "source_evidence_path is required"
  else
    let domain_s = domain_to_string domain in
    let normalized_location = summary_location in
    let alias_key = match domain with Pointer_memory_effect -> Some summary_location | Global_write_read | Taint_product_component -> None in
    let delta_id = deterministic_hash [provider_module; export_name; domain_s; normalized_location] in
    let chain_id = deterministic_hash ["memory-delta-chain"; provider_module; provider_source_hash; export_name; domain_s; raw_location; normalized_location] in
    let chain_hash = deterministic_hash [chain_id; provider_artifact_path; string_of_int provider_phase_index; value] in
    Ok {
      delta_id;
      location = { domain; raw_location; normalized_location; symbol; alias_key };
      transition = {
        read_value = value;
        write_value = value;
        read_canonical_value = canonical_value_json value;
        write_canonical_value = canonical_value_json value;
      };
      provenance = {
        provider_module;
        provider_source_hash;
        provider_artifact_path;
        provider_phase_index;
        source_evidence_path;
      };
      chain_id;
      chain_hash;
    }

let delta_id d = d.delta_id
let chain_id d = d.chain_id

let chain_entry ~index ~role ~actor ~evidence_path ~location ~value ~chain_id ~chain_hash =
  `Assoc [
    "index", `Int index;
    "role", `String role;
    "actor", `String actor;
    "evidence_path", `String evidence_path;
    "location", `String location;
    "value", `String value;
    "canonical_value", canonical_value_json value;
    "chain_id", `String chain_id;
    "chain_hash", `String chain_hash;
  ]

let delta_chain_json d = [
  chain_entry
    ~index:0
    ~role:"writer"
    ~actor:("provider:" ^ d.provenance.provider_module)
    ~evidence_path:d.provenance.source_evidence_path
    ~location:d.location.raw_location
    ~value:d.transition.read_value
    ~chain_id:d.chain_id
    ~chain_hash:d.chain_hash;
  chain_entry
    ~index:1
    ~role:"linker"
    ~actor:"external-summary-v3"
    ~evidence_path:("external_summaries.memory_deltas:" ^ d.delta_id)
    ~location:d.location.normalized_location
    ~value:d.transition.write_value
    ~chain_id:d.chain_id
    ~chain_hash:d.chain_hash;
  chain_entry
    ~index:2
    ~role:"reader"
    ~actor:"linked-residual-oracle-observation"
    ~evidence_path:("selected_observation.memory:" ^ d.location.normalized_location)
    ~location:d.location.normalized_location
    ~value:d.transition.write_value
    ~chain_id:d.chain_id
    ~chain_hash:d.chain_hash;
]

let roles_json =
  `Assoc [
    "writer", `String "provider";
    "reader", `String "importer";
    "provider", `String "writer";
    "importer", `String "reader";
    "linker", `String "chain-observer";
  ]

let taint_state_of_value value =
  match String.trim value with
  | "tainted" | "tainted-user-input" | "user-input" -> Some "tainted-user-input"
  | "untainted" | "untainted-bottom" | "bottom" -> Some "untainted-bottom"
  | _ -> None

let taint_component_fields d =
  match d.location.domain with
  | Taint_product_component ->
      let state = match taint_state_of_value d.transition.write_value with Some s -> s | None -> d.transition.write_value in
      [
        "taint_component_schema", `String bounded_taint_component_schema_id;
        "taint_state", `String state;
        "taint_semantic_relation", `String "bounded-user-input-taint-component";
        "product_components", `List [`String "Itv"; `String "Taint"];
        "taint_support_scope", `String "named-witness-only-no-general-product-domain-parity";
      ]
  | Global_write_read | Pointer_memory_effect -> []

let delta_to_json d =
  `Assoc ([
    "memory_delta_schema", `String memory_delta_schema_id;
    "delta_id", `String d.delta_id;
    "domain", `String (domain_to_string d.location.domain);
    "symbol", `String d.location.symbol;
    "raw_location", `String d.location.raw_location;
    "location", `String d.location.normalized_location;
    "normalized_location", `String d.location.normalized_location;
    "alias_key", (match d.location.alias_key with Some key -> `String key | None -> `Null);
    "reader_role", `String "reader";
    "writer_role", `String "writer";
    "provider_role", `String "provider";
    "importer_role", `String "importer";
    "linker_role", `String "linker";
    "roles", roles_json;
    "read_value", `String d.transition.read_value;
    "write_value", `String d.transition.write_value;
    "read_canonical_value", d.transition.read_canonical_value;
    "write_canonical_value", d.transition.write_canonical_value;
    "provider_module", `String d.provenance.provider_module;
    "provider_source_hash", `String d.provenance.provider_source_hash;
    "provider_artifact_path", `String d.provenance.provider_artifact_path;
    "provider_phase_index", `Int d.provenance.provider_phase_index;
    "derivation_source", `String "provider-stage2-output";
    "source_evidence_path", `String d.provenance.source_evidence_path;
    "chain_id", `String d.chain_id;
    "chain_hash", `String d.chain_hash;
    "delta_chain", `List (delta_chain_json d);
    "witness_scope", `String witness_scope;
    "excluded_domains", `List [`String "Oct"];
  ] @ taint_component_fields d)

let compatibility_effect_json d =
  `Assoc [
    "effect_id", `String d.delta_id;
    "domain", `String (domain_to_string d.location.domain);
    "symbol", `String d.location.symbol;
    "location", `String d.location.normalized_location;
    "normalized_location", `String d.location.normalized_location;
    "value", `String d.transition.write_value;
    "abstract_value", `String d.transition.write_value;
    "singleton_int", `Null;
    "provider_module", `String d.provenance.provider_module;
    "provider_source_hash", `String d.provenance.provider_source_hash;
    "provider_artifact_path", `String d.provenance.provider_artifact_path;
    "provider_phase_index", `Int d.provenance.provider_phase_index;
    "derivation_source", `String "provider-stage2-output";
    "source_evidence_path", `String d.provenance.source_evidence_path;
    "witness_scope", `String witness_scope;
    "compatibility_projection", `String "external-summary-v2-non-authoritative";
    "v3_delta_id", `String d.delta_id;
    "v3_chain_id", `String d.chain_id;
  ]

let validation_ok = function Ok () -> true | Error _ -> false
let validation_reasons = function Ok () -> [] | Error reasons -> reasons
let validation_result_json result =
  `Assoc [
    "status", `String (if validation_ok result then "pass" else "fail");
    "reasons", `List (List.map (fun reason -> `String reason) (validation_reasons result));
  ]

let add cond reason reasons = if cond then reasons else reason :: reasons

let string_list_field name json =
  list_field name json
  |> List.filter_map (function `String s -> Some s | _ -> None)

let supported_taint_state = function
  | "bottom" | "untainted" | "tainted-user-input" -> true
  | _ -> false

let validate_taint_component_json component =
  let reasons =
    []
    |> add (string_field "taint_protocol_schema" component = taint_protocol_schema_id)
         "taint_component_schema_mismatch"
    |> add (string_field "domain" component = "Taint") "taint_component_schema_mismatch"
    |> add (string_field "witness_scope" component = witness_scope) "taint_component_schema_mismatch"
    |> add (string_field "taint_witness_id" component <> "") "taint_component_missing_semantic_binding"
    |> add (string_field "symbol" component <> "") "taint_component_missing_semantic_binding"
    |> add (string_field "location" component <> "") "taint_component_missing_semantic_binding"
    |> add (string_field "source_evidence_path" component <> "") "taint_component_missing_semantic_binding"
    |> add (string_field "related_effect_id" component <> "") "taint_component_missing_semantic_binding"
    |> add (string_field "semantic_relation" component = "provider-return-taint-flow")
         "taint_component_missing_semantic_binding"
    |> add (supported_taint_state (string_field "taint_state" component))
         "taint_component_unsupported_state"
    |> add (not (bool_field "metadata_only" component)) "taint_component_metadata_only"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_product_pair_evidence_json evidence =
  let taint_component = match assoc_field "taint_component" evidence with Some json -> json | None -> `Null in
  let components = string_list_field "product_components" evidence in
  let taint_reasons = validation_reasons (validate_taint_component_json taint_component) in
  let pair_witness = string_field "taint_witness_id" evidence in
  let taint_witness = string_field "taint_witness_id" taint_component in
  let reasons =
    taint_reasons
    |> add (string_field "product_pair_schema" evidence = product_pair_schema_id)
         "product_pair_schema_mismatch"
    |> add (string_field "witness_scope" evidence = witness_scope) "product_pair_schema_mismatch"
    |> add (List.mem "Itv" components && List.mem "Taint" components)
         "product_pair_missing_component"
    |> add (pair_witness <> "" && pair_witness = taint_witness)
         "product_pair_missing_component"
    |> add (assoc_field "itv_component" evidence <> None) "product_pair_missing_component"
    |> add (string_field "taint_semantic_relation" evidence = "provider-return-taint-flow")
         "product_pair_semantic_relation_mismatch"
    |> add (string_field "semantic_relation_status" evidence = "pass")
         "product_pair_semantic_relation_mismatch"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_chain delta chain =
  let entries = match chain with `List xs -> xs | _ -> [] in
  let has_role role = List.exists (fun entry -> string_field "role" entry = role) entries in
  []
  |> add (entries <> []) "memory_delta_chain_missing"
  |> add (List.length entries >= 3) "memory_delta_chain_missing"
  |> add (has_role "writer" && has_role "linker" && has_role "reader") "memory_delta_role_mismatch"
  |> add (List.for_all (fun entry -> string_field "chain_id" entry = string_field "chain_id" delta) entries) "memory_delta_chain_id_mismatch"
  |> add (List.for_all (fun entry -> string_field "chain_hash" entry = string_field "chain_hash" delta) entries) "memory_delta_chain_hash_mismatch"

let validate_delta_json delta =
  let location = string_field "location" delta in
  let raw_location = string_field "raw_location" delta in
  let normalized_location = string_field "normalized_location" delta in
  let domain = string_field "domain" delta in
  let source_path = string_field "source_evidence_path" delta in
  let chain_id = string_field "chain_id" delta in
  let provider_module = string_field "provider_module" delta in
  let provider_source_hash = string_field "provider_source_hash" delta in
  let provider_artifact_path = string_field "provider_artifact_path" delta in
  let provider_phase_index = int_field "provider_phase_index" delta in
  let chain_id_provenance_prefix =
    deterministic_hash ["memory-delta-chain"; provider_module; provider_source_hash] ^ ":"
  in
  let expected_chain_hash =
    deterministic_hash
      [chain_id; provider_artifact_path; string_of_int provider_phase_index; string_field "write_value" delta]
  in
  let memory_prefix = "provider_row.memory:" in
  let taint_prefix = "provider_row.taint:" in
  let source_location =
    if starts_with source_path memory_prefix then
      String.sub source_path (String.length memory_prefix) (String.length source_path - String.length memory_prefix)
    else if starts_with source_path taint_prefix then
      String.sub source_path (String.length taint_prefix) (String.length source_path - String.length taint_prefix)
    else ""
  in
  let reasons = validate_chain delta (match assoc_field "delta_chain" delta with Some c -> c | None -> `Null) in
  let reasons =
    reasons
    |> add (string_field "memory_delta_schema" delta = memory_delta_schema_id) "memory_delta_schema_mismatch"
    |> add (domain = "global-write-read" || domain = "pointer-memory-effect" || domain = "Taint") "memory_delta_unsupported_domain"
    |> add (string_field "delta_id" delta <> "") "memory_delta_missing_id"
    |> add (string_field "reader_role" delta = "reader") "memory_delta_role_mismatch"
    |> add (string_field "writer_role" delta = "writer") "memory_delta_role_mismatch"
    |> add (string_field "provider_role" delta = "provider") "memory_delta_role_mismatch"
    |> add (string_field "importer_role" delta = "importer") "memory_delta_role_mismatch"
    |> add (string_field "linker_role" delta = "linker") "memory_delta_role_mismatch"
    |> add (location <> "" && normalized_location = location && raw_location <> "") "memory_delta_location_mismatch"
    |> add (source_location <> "" && source_location = raw_location) "memory_delta_location_mismatch"
    |> add (string_field "read_value" delta <> "" && string_field "write_value" delta <> "") "memory_delta_value_transition_mismatch"
    |> add (assoc_field "read_canonical_value" delta <> None && assoc_field "write_canonical_value" delta <> None) "memory_delta_value_transition_mismatch"
    |> add (provider_module <> "") "memory_delta_provenance_mismatch"
    |> add (provider_source_hash <> "") "memory_delta_provenance_mismatch"
    |> add (provider_artifact_path <> "") "memory_delta_provenance_mismatch"
    |> add (provider_phase_index <> min_int) "memory_delta_provenance_mismatch"
    |> add (string_field "derivation_source" delta = "provider-stage2-output") "memory_delta_provenance_mismatch"
    |> add (chain_id <> "" && string_field "chain_hash" delta <> "") "memory_delta_chain_missing"
    |> add (starts_with chain_id chain_id_provenance_prefix) "memory_delta_provenance_mismatch"
    |> add (string_field "chain_hash" delta = expected_chain_hash) "memory_delta_chain_hash_mismatch"
    |> add (bool_field "typed_memory_delta_valid" delta || assoc_field "typed_memory_delta_valid" delta = None) "memory_delta_schema_mismatch"
  in
  let reasons =
    match domain with
    | "global-write-read" ->
        reasons
        |> add (location = raw_location) "memory_delta_location_mismatch"
        |> add (string_field "symbol" delta = location) "memory_delta_location_mismatch"
    | "pointer-memory-effect" ->
        reasons
        |> add (string_field "alias_key" delta = location || assoc_field "alias_key" delta <> Some `Null) "memory_delta_location_mismatch"
        |> add (contains location ",") "memory_delta_location_mismatch"
    | "Taint" ->
        reasons
        |> add (assoc_field "alias_key" delta = Some `Null) "memory_delta_location_mismatch"
        |> add (string_field "taint_component_schema" delta = bounded_taint_component_schema_id) "taint_component_schema_mismatch"
        |> add (List.mem (string_field "taint_state" delta) ["tainted-user-input"; "untainted-bottom"]) "taint_component_state_mismatch"
        |> add (string_field "taint_semantic_relation" delta = "bounded-user-input-taint-component") "taint_component_relation_mismatch"
        |> add (List.mem (`String "Taint") (list_field "product_components" delta)) "taint_component_product_mismatch"
        |> add (starts_with source_path "provider_row.taint:") "memory_delta_location_mismatch"
        |> add (source_location = raw_location) "memory_delta_location_mismatch"
    | _ -> reasons
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_taint_chain evidence chain =
  let entries = match chain with `List xs -> xs | _ -> [] in
  let has_role role = List.exists (fun entry -> string_field "role" entry = role) entries in
  []
  |> add (entries <> []) "taint_product_chain_missing"
  |> add (List.length entries >= 3) "taint_product_chain_missing"
  |> add (has_role "source" && has_role "linker" && has_role "sink") "taint_product_relation_mismatch"
  |> add (List.for_all (fun entry -> string_field "chain_id" entry = string_field "taint_chain_id" evidence) entries) "taint_product_chain_missing"

let validate_taint_product_evidence_json evidence =
  let product_components =
    list_field "product_components" evidence
    |> List.filter_map (function `String s -> Some s | _ -> None)
  in
  let relation = string_field "taint_semantic_relation" evidence in
  let state = string_field "taint_state" evidence in
  let itv = match assoc_field "itv_observable" evidence with Some x -> x | None -> `Null in
  let chain_reasons = validate_taint_chain evidence (match assoc_field "taint_chain" evidence with Some c -> c | None -> `Null) in
  let state_ok = match taint_state_of_string state with Ok _ -> true | Error _ -> false in
  let reasons = chain_reasons in
  let reasons =
    reasons
    |> add (string_field "taint_product_schema" evidence = taint_product_schema_id) "taint_product_schema_mismatch"
    |> add (string_field "taint_witness_id" evidence <> "") "taint_product_schema_mismatch"
    |> add (List.mem "Itv" product_components && List.mem "Taint" product_components) "taint_product_component_mismatch"
    |> add (relation = "source-taints-sink" || relation = "untainted-preserved") "taint_product_relation_mismatch"
    |> add state_ok "taint_product_relation_mismatch"
    |> add (string_field "taint_source" evidence <> "" && string_field "taint_sink" evidence <> "") "taint_product_relation_mismatch"
    |> add (string_field "related_residual_location" evidence <> "") "taint_product_relation_mismatch"
    |> add (string_field "location" itv = string_field "related_residual_location" evidence) "taint_product_component_mismatch"
    |> add (string_field "value" itv <> "") "taint_product_component_mismatch"
    |> add (not (bool_field "metadata_only" evidence)) "taint_product_relation_mismatch"
    |> add (list_field "evidence_paths" evidence <> []) "taint_product_schema_mismatch"
    |> add (string_field "taint_chain_id" evidence <> "") "taint_product_chain_missing"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_summary_json summary =
  let deltas = list_field "memory_deltas" summary in
  let chains = list_field "delta_chains" summary in
  let summary_chain_ids = List.map (string_field "chain_id") chains in
  let delta_results = List.concat_map validation_reasons (List.map validate_delta_json deltas) in
  let delta_chain_ids = List.map (string_field "chain_id") deltas in
  let reasons = delta_results in
  let reasons =
    reasons
    |> add (string_field "schema_version" summary = external_summary_schema_id) "external_summary_schema_mismatch"
    |> add (string_field "memory_delta_schema" summary = memory_delta_schema_id) "memory_delta_schema_mismatch"
    |> add (string_field "summary_api_status" summary = summary_api_status) "summary_api_status_mismatch"
    |> add (string_field "summary_scope" summary = "sparrow-itv-selected-witness") "summary_scope_mismatch"
    |> add (deltas <> []) "memory_delta_chain_missing"
    |> add (chains <> []) "memory_delta_chain_missing"
    |> add (List.for_all (fun id -> id <> "" && List.mem id summary_chain_ids) delta_chain_ids) "memory_delta_chain_missing"
    |> add (string_field "derivation_source" (match assoc_field "provenance" summary with Some p -> p | None -> `Null) = "provider-stage2-output") "memory_delta_provenance_mismatch"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs
