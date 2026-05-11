let active_dir = ref ""
let reference_dir = ref ""
let report = ref ""
let bta_report = ref ""
let residual_report = ref ""
let usage = "real_sparrow_sparse_fixpoint_pe_compare --active <dir> --reference <dir> --report <json> --bta-report <json> --residual-report <json>"

let member name json = Yojson.Safe.Util.member name json
let to_list = Yojson.Safe.Util.to_list
let to_string = Yojson.Safe.Util.to_string

let read_manifest dir =
  Yojson.Safe.from_file (Filename.concat dir "manifest.json")
  |> member "modules" |> to_list |> List.map to_string

let projection_member name json = json |> member "projection" |> member name
let relation a b = if Yojson.Safe.equal a b then "structural-equiv" else "structural-diverge"
let bool_member name json = match member name json with `Bool b -> b | _ -> false
let int_member name json = match member name json with `Int i -> i | _ -> -1

let completion_ok completion =
  bool_member "worklist_initialized" completion &&
  bool_member "widening_performed" completion &&
  int_member "widening_iterations" completion > 0 &&
  bool_member "finalize_performed" completion &&
  bool_member "worklist_drained" completion &&
  int_member "pfs" completion = 100 &&
  not (bool_member "pfs_binding_path" completion) &&
  ((bool_member "narrowing_enabled" completion && bool_member "narrowing_applicable" completion && bool_member "narrowing_performed" completion && int_member "narrowing_iterations" completion > 0)
   || ((not (bool_member "narrowing_applicable" completion)) && member "narrowing_reason" completion <> `Null))

let module_report active_path reference_path =
  let active_json = Yojson.Safe.from_file active_path in
  let reference_json = Yojson.Safe.from_file reference_path in
  let final_input_rel = relation (projection_member "final_input_table" active_json) (projection_member "final_input_table" reference_json) in
  let final_output_rel = relation (projection_member "final_output_table" active_json) (projection_member "final_output_table" reference_json) in
  let active_completion = projection_member "completion" active_json in
  let reference_completion = projection_member "completion" reference_json in
  let completion_rel = if completion_ok active_completion && completion_ok reference_completion then "accepted" else "rejected" in
  let rel = if final_input_rel = "structural-equiv" && final_output_rel = "structural-equiv" && completion_rel = "accepted" then "structural-equiv" else "structural-diverge" in
  `Assoc [
    "name", `String (Filename.basename (active_json |> member "source" |> to_string));
    "domain_instance", active_json |> member "domain_instance";
    "active", `String active_path;
    "reference", `String reference_path;
    "relation", `String rel;
    "final_input_table", `String final_input_rel;
    "final_output_table", `String final_output_rel;
    "completion", active_completion;
    "reference_completion", reference_completion;
  ]

let relation_of_module = function
  | `Assoc fields -> (match List.assoc_opt "relation" fields with Some (`String r) -> r | _ -> "structural-diverge")
  | _ -> "structural-diverge"

let facts_of artifact = match projection_member "bta" artifact with `List xs -> xs | _ -> []
let assoc_field name fields = List.assoc_opt name fields
let string_field name fields = match assoc_field name fields with Some (`String s) -> Some s | _ -> None

let fact_node_names = function
  | `Assoc fields ->
      ["node"; "src"; "dst"]
      |> List.filter_map (fun name -> string_field name fields)
      |> List.sort_uniq String.compare
  | _ -> []

let bta_report_for_paths active_paths =
  let fixture_reports =
    active_paths |> List.map (fun path ->
      let artifact = Yojson.Safe.from_file path in
      let facts = facts_of artifact in
      let bad = facts |> List.filter (function
        | `Assoc fields ->
            let classification = List.assoc_opt "classification" fields in
            let reason = List.assoc_opt "reason" fields in
            let depends = List.assoc_opt "depends_on_extern" fields in
            begin match classification with
            | Some (`String "dynamic-extern-dependent")
            | Some (`String "residual-extern-dependent") ->
                depends <> Some (`Bool true) || not (List.mem reason [Some (`String "unknown-extern-call"); Some (`String "transitively-extern-dependent")])
            | Some (`String "static-precomputed") -> depends <> Some (`Bool false)
            | _ -> true
            end
        | _ -> true)
      in
      let roots =
        facts
        |> List.filter_map (function
          | `Assoc fields
            when assoc_field "kind" fields = Some (`String "fixpoint-node")
              && assoc_field "reason" fields = Some (`String "unknown-extern-call") ->
              string_field "node" fields
          | _ -> None)
        |> List.sort_uniq String.compare
      in
      let propagated_nonroot =
        facts
        |> List.filter (function
          | `Assoc fields
            when assoc_field "reason" fields = Some (`String "transitively-extern-dependent")
              && assoc_field "depends_on_extern" fields = Some (`Bool true) ->
              fact_node_names (`Assoc fields)
              |> List.exists (fun node -> not (List.mem node roots))
          | _ -> false)
      in
      let propagation_bad =
        match roots, propagated_nonroot with
        | [], _ | _, _ :: _ -> []
        | _ :: _, [] ->
            [`Assoc [
              "kind", `String "bta-propagation";
              "reason", `String "extern root has no transitive non-root dependency evidence";
              "roots", `List (List.map (fun r -> `String r) roots);
            ]]
      in
      `Assoc [
        "name", `String (Filename.basename (artifact |> member "source" |> to_string));
        "extern_roots", `List (List.map (fun r -> `String r) roots);
        "transitive_nonroot_fact_count", `Int (List.length propagated_nonroot);
        "facts", `List facts;
        "bad_facts", `List (bad @ propagation_bad);
      ])
  in
  let ok = fixture_reports |> List.for_all (function `Assoc fields -> List.assoc_opt "bad_facts" fields = Some (`List []) | _ -> false) in
  `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_sparse_fixpoint_pe.bta_schema_version;
    "status", `String (if ok then "pass" else "fail");
    "allowed_dynamic_reasons", `List [`String "unknown-extern-call"; `String "transitively-extern-dependent"];
    "fixtures", `List fixture_reports;
  ]

let residual_report_for_paths active_paths =
  let fixture_reports =
    active_paths |> List.map (fun path ->
      let artifact = Yojson.Safe.from_file path in
      match Sparrow_modular_ocaml.Real_sparrow_sparse_fixpoint_pe.residual_inspection_for_artifact artifact with
      | `Assoc fields -> (match List.assoc_opt "fixtures" fields with Some (`List [x]) -> x | _ -> `Assoc [])
      | _ -> `Assoc [])
  in
  let ok = fixture_reports |> List.for_all (function
    | `Assoc fields -> List.assoc_opt "artifact_present" fields = Some (`Bool true) && List.assoc_opt "residual_content" fields = Some (`String "extern-dependent-only")
    | _ -> false)
  in
  `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_sparse_fixpoint_pe.residual_schema_version;
    "status", `String (if ok then "pass" else "fail");
    "json_is_summary_only", `Bool true;
    "fixtures", `List fixture_reports;
  ]

let () =
  Arg.parse
    ["--active", Arg.Set_string active_dir, "active artifact directory";
     "--reference", Arg.Set_string reference_dir, "frozen reference artifact directory";
     "--report", Arg.Set_string report, "source-lineage report path";
     "--bta-report", Arg.Set_string bta_report, "BTA report path";
     "--residual-report", Arg.Set_string residual_report, "residual inspection report path"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !active_dir = "" || !reference_dir = "" || !report = "" || !bta_report = "" || !residual_report = "" then failwith usage;
  let active_paths = read_manifest !active_dir in
  let reference_paths = read_manifest !reference_dir in
  let fixtures = List.map2 module_report active_paths reference_paths in
  let all_equiv = List.for_all (fun m -> relation_of_module m = "structural-equiv") fixtures in
  let json = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_sparse_fixpoint_pe.schema_version;
    "claim", `String "module-only ItvDom staged sparse fixpoint PE final-table parity";
    "relation", `String (if all_equiv then "structural-equiv" else "structural-diverge");
    "fixtures", `List fixtures;
  ] in
  let bta = bta_report_for_paths active_paths in
  let residual = residual_report_for_paths active_paths in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !report json;
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !bta_report bta;
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json !residual_report residual;
  let bta_ok = match bta with `Assoc fields -> List.assoc_opt "status" fields = Some (`String "pass") | _ -> false in
  let residual_ok = match residual with `Assoc fields -> List.assoc_opt "status" fields = Some (`String "pass") | _ -> false in
  if all_equiv && bta_ok && residual_ok then print_endline ("PASS " ^ !report)
  else failwith ("Sparse fixpoint PE parity failed; see reports")
