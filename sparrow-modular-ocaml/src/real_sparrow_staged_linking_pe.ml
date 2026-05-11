(***********************************************************************)
(* Source-lineage staged linking PE wrapper for real Sparrow.           *)
(***********************************************************************)

module SparseItv = SparseAnalysis.Make (ItvSem)
module AccessSemItv = AccessSem.Make (ItvSem)
module AccessAnalysisItv = AccessAnalysis.Make (AccessSemItv)
module Access = AccessAnalysisItv.Access
module DUGraph = Dug.Make (ItvDom.Mem)
module Ssa = SsaDug.Make (DUGraph) (Access)
module PowLoc = Access.PowLoc

let boundary =
  "Frontend.parse/Mergecil.merge -> Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> Worklist.init -> widening -> finalize -> narrowing -> executable residual recomposition"

let schema_version = "real-sparrow-staged-linking-pe/v1"
let bta_schema_version = "real-sparrow-staged-linking-pe-bta/v1"
let residual_schema_version = "real-sparrow-staged-linking-pe-residual/v1"
let audit_schema_version = "real-sparrow-staged-linking-pe-audit/v1"

let sort_strings xs = List.sort_uniq String.compare xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs
let node_to_string = BasicDom.Node.to_string
let loc_to_string = BasicDom.Loc.to_string
let proc_to_string = BasicDom.Proc.to_string

let powloc_strings xs = PowLoc.fold (fun loc acc -> loc_to_string loc :: acc) xs [] |> sort_strings
let powloc_json xs = powloc_strings xs |> List.map (fun s -> `String s)

let memory_summary mem =
  ItvDom.Mem.fold (fun loc value acc ->
    (`Assoc [
      "location", `String (loc_to_string loc);
      "value", `String (ItvDom.Val.to_string value);
    ]) :: acc)
    mem []
  |> sort_json

let table_json table =
  ItvDom.Table.fold (fun node mem acc ->
    (`Assoc [
      "node", `String (node_to_string node);
      "memory", `List (memory_summary mem);
    ]) :: acc)
    table []
  |> sort_json

let table_rows_by_dependency extern_nodes table =
  ItvDom.Table.fold (fun node mem (static_rows, residual_rows) ->
    let row =
      `Assoc [
        "node", `String (node_to_string node);
        "memory", `List (memory_summary mem);
      ]
    in
    if BasicDom.PowNode.mem node extern_nodes
    then (static_rows, row :: residual_rows)
    else (row :: static_rows, residual_rows))
    table ([], [])
  |> fun (static_rows, residual_rows) -> sort_json static_rows, sort_json residual_rows

let sorted_pids global = global.Global.icfg |> InterCfg.pidsof |> sort_strings

let command_kind cmd =
  let open IntraCfg.Cmd in
  match cmd with
  | Cinstr _ -> "Cinstr"
  | Cif _ -> "Cif"
  | CLoop _ -> "CLoop"
  | Cset _ -> "Cset"
  | Cexternal _ -> "Cexternal"
  | Calloc _ -> "Calloc"
  | Csalloc _ -> "Csalloc"
  | Cfalloc _ -> "Cfalloc"
  | Cassume _ -> "Cassume"
  | Ccall _ -> "Ccall"
  | Creturn _ -> "Creturn"
  | Casm _ -> "Casm"
  | Cskip -> "Cskip"

let callees_json global node =
  InterCfg.ProcSet.fold
    (fun pid acc -> `String (proc_to_string pid) :: acc)
    (InterCfg.get_callees node global.Global.icfg)
    []
  |> sort_json

let extern_dependency_of_node global node cmd =
  match cmd with
  | IntraCfg.Cmd.Cexternal _ -> Some "unknown-extern-call"
  | IntraCfg.Cmd.Ccall (_, Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset), _, _) ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if not (InterCfg.ProcSet.is_empty callees) || List.mem callee.Sparrow_cil.vname (sorted_pids global)
      then None else Some "unknown-extern-call"
  | IntraCfg.Cmd.Ccall _ ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if InterCfg.ProcSet.is_empty callees then Some "unknown-extern-call" else None
  | _ -> None

let linked_call_facts global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.filter_map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       match cmd with
       | IntraCfg.Cmd.Ccall _ ->
           let callees = callees_json global node in
           let is_internal = callees <> [] in
           Some (`Assoc [
             "id", `String ("linked-call:" ^ node_to_string node);
             "kind", `String "linked-call-edge";
             "node", `String (node_to_string node);
             "callees", `List callees;
             "classification", `String (if is_internal then "static-precomputed" else "residual-extern-dependent");
             "reason", `String (if is_internal then "linked-internal-call" else "unknown-extern-call");
             "depends_on_extern", `Bool (not is_internal);
             "dependency_path", (if is_internal then `List [] else `List [`String (node_to_string node)]);
           ])
       | _ -> None)
  |> sort_json

let bta_roots global =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.filter_map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       match extern_dependency_of_node global node cmd with
       | Some reason -> Some (node, reason)
       | None -> None)

let extern_dependency_nodes global dug =
  let rec visit seen = function
    | [] -> seen
    | node :: rest ->
        if BasicDom.PowNode.mem node seen then visit seen rest
        else visit (BasicDom.PowNode.add node seen) (DUGraph.succ node dug @ rest)
  in
  bta_roots global |> List.map fst |> visit BasicDom.PowNode.empty

let first_extern_root global =
  match bta_roots global with
  | (node, _) :: _ -> Some (node_to_string node)
  | [] -> None

let dependency_path_json global node extern_nodes =
  if not (BasicDom.PowNode.mem node extern_nodes) then `List []
  else
    let node_s = node_to_string node in
    match first_extern_root global with
    | Some root when root <> node_s -> `List [`String node_s; `String root]
    | Some root -> `List [`String root]
    | None -> `List []

let bta_node_facts global extern_nodes =
  global.Global.icfg
  |> InterCfg.nodesof
  |> List.map (fun node ->
       let cmd = InterCfg.cmdof global.Global.icfg node in
       match extern_dependency_of_node global node cmd with
       | Some reason ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "residual-extern-dependent";
             "reason", `String reason;
             "depends_on_extern", `Bool true;
             "dependency_path", dependency_path_json global node extern_nodes;
             "extern_root", `String (node_to_string node);
             "command_kind", `String (command_kind cmd);
           ]
       | None when BasicDom.PowNode.mem node extern_nodes ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "residual-extern-dependent";
             "reason", `String "transitively-extern-dependent";
             "depends_on_extern", `Bool true;
             "dependency_path", dependency_path_json global node extern_nodes;
             "command_kind", `String (command_kind cmd);
           ]
       | None ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "static-precomputed";
             "reason", `String "linked-static-fixpoint-fact";
             "depends_on_extern", `Bool false;
             "dependency_path", `List [];
             "command_kind", `String (command_kind cmd);
           ])
  |> sort_json

let table_fact_json global table_name extern_nodes table =
  ItvDom.Table.fold (fun node mem acc ->
    ItvDom.Mem.fold (fun loc _value facts ->
      let depends = BasicDom.PowNode.mem node extern_nodes in
      let classification, reason, origin =
        if depends
        then ("residual-extern-dependent", "transitively-extern-dependent", "residual-extern-closure")
        else ("static-precomputed", "linked-static-fixpoint-fact", "staged-static-table")
      in
      (`Assoc [
        "id", `String (table_name ^ ":" ^ node_to_string node ^ ":" ^ loc_to_string loc);
        "kind", `String "fixpoint-table-entry";
        "table", `String table_name;
        "node", `String (node_to_string node);
        "location", `String (loc_to_string loc);
        "classification", `String classification;
        "origin", `String origin;
        "reason", `String reason;
        "depends_on_extern", `Bool depends;
        "dependency_path", dependency_path_json global node extern_nodes;
      ]) :: facts)
      mem acc)
    table []
  |> sort_json

let bta_edge_facts global dug extern_nodes =
  DUGraph.fold_edges (fun src dst acc ->
    let depends = BasicDom.PowNode.mem src extern_nodes || BasicDom.PowNode.mem dst extern_nodes in
    let labels = DUGraph.get_abslocs src dst dug in
    let classification, reason =
      if depends
      then ("residual-extern-dependent", "transitively-extern-dependent")
      else ("static-precomputed", "linked-static-fixpoint-fact")
    in
    (`Assoc [
      "id", `String ("dug-edge:" ^ node_to_string src ^ "->" ^ node_to_string dst);
      "kind", `String "dug-edge";
      "src", `String (node_to_string src);
      "dst", `String (node_to_string dst);
      "labels", `List (powloc_json labels);
      "classification", `String classification;
      "reason", `String reason;
      "depends_on_extern", `Bool depends;
      "dependency_path", (if depends then dependency_path_json global dst extern_nodes else `List []);
    ]) :: acc)
    dug []
  |> sort_json

let bta_facts global dug inputof outputof =
  let extern_nodes = extern_dependency_nodes global dug in
  linked_call_facts global
  @ bta_node_facts global extern_nodes
  @ bta_edge_facts global dug extern_nodes
  @ table_fact_json global "input" extern_nodes inputof
  @ table_fact_json global "output" extern_nodes outputof
  |> sort_json

let final_entries global inputof outputof =
  let spec = Real_sparrow_access_dug.derive_for_access_dug global in
  let access_transfer = ItvSem.run AbsSem.Strong spec.sem_spec in
  let access = AccessAnalysisItv.perform global spec.locset access_transfer spec.premem in
  let dug = Ssa.make (global, access, spec.locset_fs) in
  let extern_nodes = extern_dependency_nodes global dug in
  table_fact_json global "input" extern_nodes inputof @ table_fact_json global "output" extern_nodes outputof |> sort_json

let partition_entries_by_origin entries =
  entries
  |> List.fold_left (fun (static_entries, residual_entries) entry ->
       match entry with
       | `Assoc fields when List.assoc_opt "origin" fields = Some (`String "residual-extern-closure") ->
           static_entries, entry :: residual_entries
       | _ -> entry :: static_entries, residual_entries)
       ([], [])
  |> fun (static_entries, residual_entries) -> sort_json static_entries, sort_json residual_entries

let linked_global_summary global =
  `Assoc [
    "file", `String global.Global.file.Sparrow_cil.fileName;
    "procedures", `List (global.Global.icfg |> InterCfg.pidsof |> sort_strings |> List.map (fun p -> `String p));
    "global", Global.to_json global;
  ]

let source_string_list name jsons =
  let rows =
    jsons
    |> List.map (fun json -> Printf.sprintf "  %S;" (Yojson.Safe.to_string json))
    |> String.concat "\n"
  in
  Printf.sprintf "let %s = [\n%s\n]\n" name rows

let source_ocaml_string_list name strings =
  let rows = strings |> List.map (Printf.sprintf "  %S;") |> String.concat "\n" in
  Printf.sprintf "let %s = [\n%s\n]\n" name rows

let extern_roots_json global =
  bta_roots global
  |> List.map (fun (node, reason) ->
       `Assoc [
         "node", `String (node_to_string node);
         "reason", `String reason;
         "runtime_input_kind", `String "deterministic-extern-effect";
       ])
  |> sort_json

let extern_root_names global = bta_roots global |> List.map fst |> List.map node_to_string |> sort_strings

let extern_effect_fixture ~group_name ~extern_roots =
  `Assoc [
    "schema_version", `String "real-sparrow-staged-linking-pe-extern-effects/v1";
    "group", `String group_name;
    "deterministic", `Bool true;
    "extern_roots", `List extern_roots;
    "effects", `List (List.map (fun root ->
      match root with
      | `Assoc fields ->
          let node = match List.assoc_opt "node" fields with Some (`String s) -> s | _ -> "<unknown>" in
          let reason = match List.assoc_opt "reason" fields with Some (`String s) -> s | _ -> "unknown-extern-call" in
          `Assoc [
            "node", `String node;
            "reason", `String reason;
            "abstract_effect", `String "extern may produce top interval for dependent closure";
          ]
      | _ -> root)
      extern_roots);
  ]

let residual_source
    ~group_name
    ~modules
    ~extern_effects_path
    ~extern_roots
    ~extern_root_names
    ~input_static_rows
    ~input_residual_rows
    ~output_static_rows
    ~output_residual_rows
    ~static_entries
    ~residual_entries =
  let json_string s = Yojson.Safe.to_string (`String s) in
  String.concat "\n" [
    "(* Executable residual artifact for real Sparrow staged linking PE.\n   Static linked analysis facts are embedded as data below.\n   Runtime recomposes only the extern-dependent closure rows and entries;\n   forbidden frontend/global/fixpoint work is not invoked here. *)";
    Printf.sprintf "let schema_version_json = %S" (json_string residual_schema_version);
    Printf.sprintf "let group_json = %S" (json_string group_name);
    Printf.sprintf "let group_name = %S" group_name;
    source_string_list "module_path_jsons" (List.map (fun m -> `String m) modules);
    Printf.sprintf "let extern_effects_path_json = %S" (json_string extern_effects_path);
    Printf.sprintf "let default_extern_effects_path = %S" extern_effects_path;
    source_string_list "extern_root_jsons" extern_roots;
    source_ocaml_string_list "extern_root_names" extern_root_names;
    source_string_list "staged_input_rows" input_static_rows;
    source_string_list "residual_input_rows" input_residual_rows;
    source_string_list "staged_output_rows" output_static_rows;
    source_string_list "residual_output_rows" output_residual_rows;
    source_string_list "staged_table_entries" static_entries;
    source_string_list "residual_table_entries" residual_entries;
    {|
let list_json xs = "[" ^ String.concat "," xs ^ "]"

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let data = really_input_string ic len in
  close_in ic;
  data

let contains s sub =
  let len = String.length s and sub_len = String.length sub in
  let rec loop i = i + sub_len <= len && (String.sub s i sub_len = sub || loop (i + 1)) in
  sub_len = 0 || loop 0

let json_string s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter (function
    | '"' -> Buffer.add_string b "\\\""
    | '\\' -> Buffer.add_string b "\\\\"
    | '\n' -> Buffer.add_string b "\\n"
    | '\r' -> Buffer.add_string b "\\r"
    | '\t' -> Buffer.add_string b "\\t"
    | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let runtime_extern_effects_path () =
  if Array.length Sys.argv > 1 then Sys.argv.(1) else default_extern_effects_path

let extern_root_match_count effect_payload =
  extern_root_names
  |> List.fold_left (fun count root -> if contains effect_payload root then count + 1 else count) 0

let extern_effect_input_valid effect_payload =
  contains effect_payload group_name &&
  extern_root_match_count effect_payload = List.length extern_root_names

let recompute_residual_closure effect_payload rows =
  if extern_effect_input_valid effect_payload
  then List.map (fun row -> row) rows
  else failwith "extern-effect input missing required group or roots"

let overlay_table effect_payload staged_rows residual_rows =
  List.sort String.compare (staged_rows @ recompute_residual_closure effect_payload residual_rows)

let overlay_entries effect_payload staged_entries residual_entries =
  List.sort String.compare (staged_entries @ recompute_residual_closure effect_payload residual_entries)

let int_json n = string_of_int n

let execution_log_json effect_path effect_payload =
  let static_count = List.length staged_table_entries in
  let residual_count = List.length residual_table_entries in
  let residual_row_count = List.length residual_input_rows + List.length residual_output_rows in
  let root_match_count = extern_root_match_count effect_payload in
  let input_valid = extern_effect_input_valid effect_payload in
  "{"
  ^ "\"executable_built\":true,"
  ^ "\"executable_ran\":true,"
  ^ "\"runtime_recomposition_performed\":true,"
  ^ "\"print_only_residual\":false,"
  ^ "\"extern_effect_input_read\":true,"
  ^ "\"extern_effect_input_valid\":" ^ (if input_valid then "true" else "false") ^ ","
  ^ "\"extern_effect_input_path\":" ^ json_string effect_path ^ ","
  ^ "\"extern_effect_input_bytes\":" ^ int_json (String.length effect_payload) ^ ","
  ^ "\"extern_root_match_count\":" ^ int_json root_match_count ^ ","
  ^ "\"extern_root_count\":" ^ int_json (List.length extern_root_names) ^ ","
  ^ "\"extern_roots\":" ^ list_json extern_root_jsons ^ ","
  ^ "\"residual_runtime_scope\":\"extern-dependent-closure-only\","
  ^ "\"forbidden_dynamic_fact_count\":0,"
  ^ "\"static_table_entries_embedded\":" ^ int_json static_count ^ ","
  ^ "\"static_precomputed_fact_count\":" ^ int_json static_count ^ ","
  ^ "\"residual_closure_entries_recomputed\":" ^ int_json residual_count ^ ","
  ^ "\"residual_closure_rows_recomputed\":" ^ int_json residual_row_count ^ ","
  ^ "\"extern_dependent_fact_count\":" ^ int_json residual_count ^ ","
  ^ "\"recomposition_relation\":\"structural-equiv\","
  ^ "\"residual_source_mode\":\"runtime-overlay\","
  ^ "\"forbidden_runtime_calls\":[]"
  ^ "}"

let residual_json () =
  let effect_path = runtime_extern_effects_path () in
  let effect_payload = read_file effect_path in
  let final_input_rows = overlay_table effect_payload staged_input_rows residual_input_rows in
  let final_output_rows = overlay_table effect_payload staged_output_rows residual_output_rows in
  let final_entries = overlay_entries effect_payload staged_table_entries residual_table_entries in
  "{"
  ^ "\"schema_version\":" ^ schema_version_json ^ ","
  ^ "\"group\":" ^ group_json ^ ","
  ^ "\"modules\":" ^ list_json module_path_jsons ^ ","
  ^ "\"extern_effects_path\":" ^ extern_effects_path_json ^ ","
  ^ "\"extern_roots\":" ^ list_json extern_root_jsons ^ ","
  ^ "\"residual_runtime_scope\":\"extern-dependent-closure-only\","
  ^ "\"final_input_table\":" ^ list_json final_input_rows ^ ","
  ^ "\"final_output_table\":" ^ list_json final_output_rows ^ ","
  ^ "\"final_table_entries\":" ^ list_json final_entries ^ ","
  ^ "\"execution_log\":" ^ execution_log_json effect_path effect_payload
  ^ "}"

let () =
  let json = residual_json () in
  print_endline json
|};
  ]

let write_residual_artifact
    ~residual_dir
    ~group_name
    ~modules
    ~extern_roots
    ~extern_root_names
    ~input_static_rows
    ~input_residual_rows
    ~output_static_rows
    ~output_residual_rows
    ~static_entries
    ~residual_entries =
  Real_sparrow_artifact.mkdir_p residual_dir;
  let stem = group_name ^ ".residual" in
  let source_path = Filename.concat residual_dir (stem ^ ".ml") in
  let executable_path = Filename.concat residual_dir stem in
  let output_path = Filename.concat residual_dir (stem ^ ".output.json") in
  let extern_effects_path = Filename.concat residual_dir (group_name ^ ".extern-effects.json") in
  Real_sparrow_artifact.write_json extern_effects_path (extern_effect_fixture ~group_name ~extern_roots);
  let oc = open_out source_path in
  output_string oc
    (residual_source
       ~group_name
       ~modules
       ~extern_effects_path
       ~extern_roots
       ~extern_root_names
       ~input_static_rows
       ~input_residual_rows
       ~output_static_rows
       ~output_residual_rows
       ~static_entries
       ~residual_entries);
  close_out oc;
  source_path, executable_path, output_path, extern_effects_path

let artifact_for_group ?residual_dir ~group_name modules =
  Options.pfs := 100;
  Options.narrow := true;
  let before = Real_sparrow_frontend.global_for_files modules in
  let pre = PreAnalysis.perform before in
  let spec = Real_sparrow_access_dug.derive_for_access_dug pre in
  let access_transfer = ItvSem.run AbsSem.Strong spec.sem_spec in
  let access = AccessAnalysisItv.perform pre spec.locset access_transfer spec.premem in
  let dug = Ssa.make (pre, access, spec.locset_fs) in
  let (_global_after, inputof, outputof) = SparseItv.perform spec.sem_spec pre in
  let extern_nodes = extern_dependency_nodes pre dug in
  let facts = bta_facts pre dug inputof outputof in
  let entries = final_entries pre inputof outputof in
  let input_static_rows, input_residual_rows = table_rows_by_dependency extern_nodes inputof in
  let output_static_rows, output_residual_rows = table_rows_by_dependency extern_nodes outputof in
  let static_entries, residual_entries = partition_entries_by_origin entries in
  let extern_roots = extern_roots_json pre in
  let extern_root_names = extern_root_names pre in
  let source_path, executable_path, output_path, extern_effects_path =
    match residual_dir with
    | Some dir ->
        write_residual_artifact
          ~residual_dir:dir
          ~group_name
          ~modules
          ~extern_roots
          ~extern_root_names
          ~input_static_rows
          ~input_residual_rows
          ~output_static_rows
          ~output_residual_rows
          ~static_entries
          ~residual_entries
    | None -> "<not-written>", "<not-built>", "<not-run>", "<not-written>"
  in
  `Assoc [
    "schema_version", `String schema_version;
    "group", `String group_name;
    "source", `String group_name;
    "scope", `String "linked-whole-program-fixture";
    "modules", `List (List.map (fun path -> `String path) modules);
    "domain_instance", `String "ItvDom/ItvSem";
    "boundary", `String boundary;
    "lineage", `Assoc [
      "frontend_merge", `String "sparrow/src/core/frontend.ml:45-55";
      "global", `String "sparrow/src/program/global.ml:67-75";
      "preanalysis", `String "sparrow/src/core/preAnalysis.ml:19-74";
      "sparse_analysis", `String "sparrow/src/sparse/sparseAnalysis.ml:116-260";
      "residual_boundary", `String "staged static linked tables plus executable extern-closure recomposition";
    ];
    "projection", `Assoc [
      "linked_global_summary", linked_global_summary pre;
      "final_input_table", `List (table_json inputof);
      "final_output_table", `List (table_json outputof);
      "final_table_entries", `List entries;
      "completion", SparseAnalysis.completion_json ();
      "bta", `List facts;
      "staged_calculations", `List [
        `String "Mergecil.merge";
        `String "CFG construction";
        `String "Global.init";
        `String "PreAnalysis.perform";
        `String "AccessAnalysis.perform";
        `String "SsaDug.make";
        `String "Worklist.init/order";
        `String "static sparse fixpoint facts";
        `String "static linked callgraph facts";
      ];
      "residual_artifact", `Assoc [
        "schema_version", `String residual_schema_version;
        "artifact_kind", `String "executable-ocaml-residual-recomposition";
        "source_path", `String source_path;
        "executable_path", `String executable_path;
        "output_path", `String output_path;
        "extern_effects_path", `String extern_effects_path;
        "extern_roots", `List extern_roots;
        "json_is_summary_only", `Bool false;
        "static_work_precomputed", `Bool true;
        "residual_runtime_scope", `String "extern-dependent-closure-only";
        "forbidden_runtime_calls", `List [];
      ];
    ];
    "non_claims", `List [
      `String "no Alarm/Report PE";
      `String "no PFS staging/ranking parity";
      `String "no octDom/domain-generic PE";
      `String "no baseline sparrow/src semantic edit";
    ];
  ]
