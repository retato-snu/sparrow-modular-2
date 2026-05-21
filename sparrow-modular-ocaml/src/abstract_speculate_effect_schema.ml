(***********************************************************************)
(* Projection-only JSON schema for typed ExternalSummary effects.       *)
(***********************************************************************)

module Algebra = Abstract_speculate_effect_algebra

let schema_id = Algebra.schema_id
let summary_api_status = "typed-effect-authority"
let projection_status = "projection-only"
let string_list_json xs = `List (List.map (fun s -> `String s) xs)

let member name = function
  | `Assoc fields -> List.assoc_opt name fields
  | _ -> None

let string_field name json =
  match member name json with Some (`String s) -> s | Some (`Int n) -> string_of_int n | _ -> ""

let list_field name json = match member name json with Some (`List xs) -> xs | _ -> []

let provenance_to_json p =
  `Assoc
    [
      "provider_module", `String (Algebra.provenance_module p);
      "provider_source_hash", `String (Algebra.provenance_source_hash p);
      "provider_artifact_path", `String (Algebra.provenance_artifact_path p);
      "provider_phase_index", `Int (Algebra.provenance_phase_index p);
      "export_name", `String (Algebra.provenance_export_name p);
      "provenance_id", `String (Algebra.provenance_id p);
    ]

let payload_value name payload = List.assoc_opt name payload
let payload_json payload =
  `Assoc (List.map (fun (key, value) -> key, `String value) payload)

let effect_to_json eff =
  let payload = Algebra.effect_payload eff in
  `Assoc
    [
      "schema_version", `String schema_id;
      "artifact_kind", `String "typed-effect";
      "effect_id", `String (Algebra.effect_id eff);
      "domain", `String (Algebra.domain_to_string (Algebra.effect_domain eff));
      "symbol", `String (Option.value ~default:(Algebra.provenance_export_name (Algebra.effect_provenance eff)) (payload_value "symbol" payload));
      "location", `String (String.concat "/" (Algebra.effect_path eff));
      "path", string_list_json (Algebra.effect_path eff);
      "payload", payload_json payload;
      "evidence_path", string_list_json (Algebra.effect_evidence_path eff);
      "provenance", provenance_to_json (Algebra.effect_provenance eff);
      "authority", `String "typed-effect-algebra";
    ]

let projection_to_json projection =
  `Assoc
    [
      "schema_version", `String schema_id;
      "artifact_kind", `String "typed-projection";
      "projection_id", `String (Algebra.projection_id projection);
      "observation", `String (Algebra.observation_to_string (Algebra.projection_observation projection));
      "effect_id", `String (Algebra.projection_source_effect_id projection);
      "source_effect_id", `String (Algebra.projection_source_effect_id projection);
      "source_provenance_id", `String (Algebra.projection_source_provenance_id projection);
      "evidence_path", string_list_json (Algebra.projection_evidence_path projection);
      "projection_status", `String projection_status;
    ]

let defined_effect_to_json = function
  | Algebra.Defined eff -> Ok (effect_to_json eff)
  | Algebra.Undefined reason -> Error reason

let serialize_projection_result = function
  | Algebra.Defined projection -> Ok (projection_to_json projection)
  | Algebra.Undefined reason -> Error reason

let add cond reason reasons = if cond then reasons else reason :: reasons

let validate_effect_json json =
  let reasons =
    []
    |> add (string_field "schema_version" json = schema_id) "effect_schema_mismatch"
    |> add (string_field "artifact_kind" json = "typed-effect") "effect_artifact_kind_mismatch"
    |> add (string_field "authority" json = "typed-effect-algebra") "effect_authority_mismatch"
    |> add (string_field "effect_id" json <> "") "effect_id_missing"
    |> add (string_field "domain" json <> "") "effect_domain_missing"
    |> add (string_field "symbol" json <> "") "effect_symbol_missing"
    |> add (string_field "location" json <> "") "effect_location_missing"
    |> add (member "provenance" json <> None) "effect_provenance_missing"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validate_projection_json json =
  let reasons =
    []
    |> add (string_field "schema_version" json = schema_id) "projection_schema_mismatch"
    |> add (string_field "artifact_kind" json = "typed-projection") "projection_artifact_kind_mismatch"
    |> add (string_field "projection_status" json = projection_status) "projection_status_mismatch"
    |> add (string_field "projection_id" json <> "") "projection_id_missing"
    |> add (string_field "effect_id" json <> "") "projection_effect_id_missing"
    |> add (string_field "observation" json <> "") "projection_observation_missing"
    |> add (list_field "evidence_path" json <> []) "projection_evidence_path_missing"
  in
  match List.rev reasons with [] -> Ok () | rs -> Error rs

let validation_ok = function Ok () -> true | Error _ -> false
let validation_reasons = function Ok () -> [] | Error reasons -> reasons

let validation_result_json result =
  `Assoc
    [
      "status", `String (if validation_ok result then "pass" else "fail");
      "reasons", `List (List.map (fun reason -> `String reason) (validation_reasons result));
    ]

let validate_defined_artifact_json json =
  match string_field "artifact_kind" json with
  | "typed-effect" -> validate_effect_json json
  | "typed-projection" -> validate_projection_json json
  | _ -> Error [ "unknown_typed_artifact_kind" ]

let summary_to_json ~effects ~projections ~legacy_projection =
  `Assoc
    [
      "schema_version", `String schema_id;
      "authority_model", `String "typed-effects-before-json-projections";
      "summary_api_status", `String summary_api_status;
      "typed_effects", `List (List.map effect_to_json effects);
      "typed_projections", `List (List.map projection_to_json projections);
      "legacy_projection", legacy_projection;
    ]
