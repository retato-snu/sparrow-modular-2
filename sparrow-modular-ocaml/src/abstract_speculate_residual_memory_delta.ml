(***********************************************************************)
(* Typed ExternalSummary v3 memory-delta protocol.                    *)
(***********************************************************************)

module Cell = Abstract_speculate_itv_residual_cell

let external_summary_schema_id = "abstract-speculate-external-summary/v3"
let memory_delta_schema_id = "abstract-speculate-external-summary-memory-delta/v3"
let summary_api_status = "prototype-internal"
let witness_scope = "selected-sparrow-itv"

let relation_failure_reasons = [
  "memory_delta_role_mismatch";
  "memory_delta_location_mismatch";
  "memory_delta_value_transition_mismatch";
  "memory_delta_provenance_mismatch";
  "memory_delta_chain_missing";
]

type memory_domain = Global_write_read | Pointer_memory_effect

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

let domain_to_string = function
  | Global_write_read -> "global-write-read"
  | Pointer_memory_effect -> "pointer-memory-effect"

let domain_of_string = function
  | "global-write-read" -> Ok Global_write_read
  | "pointer-memory-effect" -> Ok Pointer_memory_effect
  | "Oct" | "oct" | "Taint" | "taint" -> Error "unsupported memory delta domain: Oct/Taint"
  | other -> Error ("unsupported memory delta domain: " ^ other)

let canonical_value_json value = Cell.canonical_value_json_of_legacy_string value

let deterministic_hash fields = String.concat ":" fields

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
    let alias_key = match domain with Pointer_memory_effect -> Some summary_location | Global_write_read -> None in
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

let delta_to_json d =
  `Assoc [
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
    "excluded_domains", `List [`String "Oct"; `String "Taint"];
  ]

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
  let expected_prefix = "provider_row.memory:" in
  let source_location =
    if starts_with source_path expected_prefix then
      String.sub source_path (String.length expected_prefix) (String.length source_path - String.length expected_prefix)
    else ""
  in
  let reasons = validate_chain delta (match assoc_field "delta_chain" delta with Some c -> c | None -> `Null) in
  let reasons =
    reasons
    |> add (string_field "memory_delta_schema" delta = memory_delta_schema_id) "memory_delta_schema_mismatch"
    |> add (domain = "global-write-read" || domain = "pointer-memory-effect") "memory_delta_unsupported_domain"
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
    |> add (string_field "provider_module" delta <> "") "memory_delta_provenance_mismatch"
    |> add (string_field "provider_source_hash" delta <> "") "memory_delta_provenance_mismatch"
    |> add (string_field "provider_artifact_path" delta <> "") "memory_delta_provenance_mismatch"
    |> add (int_field "provider_phase_index" delta <> min_int) "memory_delta_provenance_mismatch"
    |> add (string_field "derivation_source" delta = "provider-stage2-output") "memory_delta_provenance_mismatch"
    |> add (string_field "chain_id" delta <> "" && string_field "chain_hash" delta <> "") "memory_delta_chain_missing"
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
    | _ -> reasons
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
