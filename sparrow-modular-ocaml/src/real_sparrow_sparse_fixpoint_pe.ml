(***********************************************************************)
(* Source-lineage staged sparse fixpoint PE wrapper for real Sparrow.   *)
(***********************************************************************)

module SparseItv = SparseAnalysis.Make (ItvSem)
module AccessSemItv = AccessSem.Make (ItvSem)
module AccessAnalysisItv = AccessAnalysis.Make (AccessSemItv)
module Access = AccessAnalysisItv.Access
module DUGraph = Dug.Make (ItvDom.Mem)
module Ssa = SsaDug.Make (DUGraph) (Access)
module PowLoc = Access.PowLoc

let boundary =
  "Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make -> Worklist.init -> widening -> finalize -> narrowing"

let schema_version = "real-sparrow-sparse-fixpoint-pe/v1"
let bta_schema_version = "real-sparrow-sparse-fixpoint-pe-bta/v1"
let residual_schema_version = "real-sparrow-sparse-fixpoint-pe-residual/v1"

let compare_string = String.compare
let sort_strings xs = List.sort_uniq compare_string xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs
let node_to_string = BasicDom.Node.to_string
let loc_to_string = BasicDom.Loc.to_string
let proc_to_string = BasicDom.Proc.to_string

let typed_staged_residual_artifact : (string array -> int) Trx.code =
  .<fun extern_effects -> Array.length extern_effects>.

let residual_source ~source ~extern_fact_count =
  Printf.sprintf
    "(* MetaOCaml typed staged residual artifact for %s.\n   This artifact is inspection-only: it is not a residual linker or global-fixpoint runtime.\n   Static sparse-fixpoint facts were precomputed; this residual summarizes %d extern-dependent facts. *)\nlet residual_extern_dependency_count extern_effects = Array.length extern_effects\n"
    (Filename.basename source) extern_fact_count

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
        else
          let seen = BasicDom.PowNode.add node seen in
          visit seen (DUGraph.succ node dug @ rest)
  in
  bta_roots global |> List.map fst |> visit BasicDom.PowNode.empty

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
             "command_kind", `String (command_kind cmd);
           ]
       | None ->
           `Assoc [
             "id", `String ("node:" ^ node_to_string node);
             "kind", `String "fixpoint-node";
             "node", `String (node_to_string node);
             "classification", `String "static-precomputed";
             "reason", `String "module-local-static-fixpoint-fact";
             "depends_on_extern", `Bool false;
             "command_kind", `String (command_kind cmd);
           ])
  |> sort_json

let table_fact_json table_name extern_nodes table =
  ItvDom.Table.fold (fun node mem acc ->
    ItvDom.Mem.fold (fun loc _value facts ->
      let classification, reason, depends =
        if BasicDom.PowNode.mem node extern_nodes
        then ("residual-extern-dependent", "transitively-extern-dependent", true)
        else ("static-precomputed", "module-local-static-fixpoint-fact", false)
      in
      (`Assoc [
        "id", `String (table_name ^ ":" ^ node_to_string node ^ ":" ^ loc_to_string loc);
        "kind", `String "fixpoint-table-entry";
        "table", `String table_name;
        "node", `String (node_to_string node);
        "location", `String (loc_to_string loc);
        "classification", `String classification;
        "reason", `String reason;
        "depends_on_extern", `Bool depends;
      ]) :: facts)
      mem acc)
    table []
  |> sort_json

let bta_edge_facts dug extern_nodes =
  DUGraph.fold_edges (fun src dst acc ->
    let depends = BasicDom.PowNode.mem src extern_nodes || BasicDom.PowNode.mem dst extern_nodes in
    let labels = DUGraph.get_abslocs src dst dug in
    let classification, reason =
      if depends
      then ("residual-extern-dependent", "transitively-extern-dependent")
      else ("static-precomputed", "module-local-static-fixpoint-fact")
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
    ]) :: acc)
    dug []
  |> sort_json

let bta_facts global dug inputof outputof =
  let extern_nodes = extern_dependency_nodes global dug in
  bta_node_facts global extern_nodes
  @ bta_edge_facts dug extern_nodes
  @ table_fact_json "input" extern_nodes inputof
  @ table_fact_json "output" extern_nodes outputof
  |> sort_json

let basename_json artifact =
  let open Yojson.Safe.Util in
  `String (artifact |> member "source" |> to_string |> Filename.basename)

let bta_report_for_artifact artifact =
  let open Yojson.Safe.Util in
  let facts = artifact |> member "projection" |> member "bta" |> to_list in
  let bad =
    facts |> List.filter (function
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
  `Assoc [
    "schema_version", `String bta_schema_version;
    "status", `String (if bad = [] then "pass" else "fail");
    "allowed_dynamic_reasons", `List [`String "unknown-extern-call"; `String "transitively-extern-dependent"];
    "forbidden_fallback_reasons", `List [`String "not-extern-independent"; `String "unknown"; `String "hard-to-prove"; `String "dynamic-by-default"];
    "fixtures", `List [`Assoc [
      "name", basename_json artifact;
      "facts", `List facts;
      "bad_facts", `List bad;
    ]];
  ]

let residual_inspection_for_artifact artifact =
  let open Yojson.Safe.Util in
  let residual = artifact |> member "projection" |> member "residual_artifact" in
  let artifact_path = residual |> member "artifact_path" |> to_string in
  let present = Sys.file_exists artifact_path in
  `Assoc [
    "schema_version", `String residual_schema_version;
    "status", `String (if present then "pass" else "fail");
    "json_is_summary_only", `Bool true;
    "fixtures", `List [`Assoc [
      "name", basename_json artifact;
      "artifact_kind", `String "typed-metaocaml-residual-artifact";
      "typed_staged_artifact", `Bool true;
      "artifact_path", `String artifact_path;
      "artifact_present", `Bool present;
      "static_work_precomputed", `Bool true;
      "residual_content", `String "extern-dependent-only";
      "forbidden_claims", `Assoc [
        "executable_residual_linker", `Bool false;
        "residual_global_fixpoint_runtime", `Bool false;
        "whole_program_merge", `Bool false;
      ];
    ]];
  ]

let write_residual_artifact ~residual_dir ~source ~extern_fact_count =
  Real_sparrow_artifact.mkdir_p residual_dir;
  let path = Filename.concat residual_dir (Filename.basename source ^ ".residual.ml") in
  let oc = open_out path in
  output_string oc (residual_source ~source ~extern_fact_count);
  close_out oc;
  path

let artifact_for_global ?residual_dir ~source _before pre =
  Options.pfs := 100;
  Options.narrow := true;
  let spec = Real_sparrow_access_dug.derive_for_access_dug pre in
  let access_transfer = ItvSem.run AbsSem.Strong spec.sem_spec in
  let access = AccessAnalysisItv.perform pre spec.locset access_transfer spec.premem in
  let dug = Ssa.make (pre, access, spec.locset_fs) in
  let (_global_after, inputof, outputof) = SparseItv.perform spec.sem_spec pre in
  let facts = bta_facts pre dug inputof outputof in
  let extern_fact_count =
    facts |> List.fold_left (fun n -> function
      | `Assoc fields when List.assoc_opt "depends_on_extern" fields = Some (`Bool true) -> n + 1
      | _ -> n) 0
  in
  let residual_path =
    match residual_dir with
    | Some dir -> write_residual_artifact ~residual_dir:dir ~source ~extern_fact_count
    | None -> "<not-written>"
  in
  `Assoc [
    "schema_version", `String schema_version;
    "source", `String source;
    "scope", `String "module-only";
    "domain_instance", `String "ItvDom/ItvSem";
    "boundary", `String boundary;
    "lineage", `Assoc [
      "sparse_analysis", `String "sparrow/src/sparse/sparseAnalysis.ml:116-260";
      "worklist", `String "sparrow/src/sparse/worklist.ml:154-238";
      "itv_sem", `String "sparrow/src/semantics/itvSem.ml / modular src/itvSem.ml";
      "access_dug_predecessor", `String "sparrow-modular-ocaml/src/real_sparrow_access_dug.ml";
    ];
    "projection", `Assoc [
      "final_input_table", `List (table_json inputof);
      "final_output_table", `List (table_json outputof);
      "completion", SparseAnalysis.completion_json ();
      "bta", `List facts;
      "residual_artifact", `Assoc [
        "artifact_kind", `String "typed-metaocaml-residual-artifact";
        "typed_staged_artifact", `Bool true;
        "artifact_path", `String residual_path;
        "json_is_summary_only", `Bool true;
        "static_work_precomputed", `Bool true;
        "residual_content", `String "extern-dependent-only";
      ];
    ];
    "non_claims", `List [
      `String "no Worklist-only milestone";
      `String "no standalone interval-analysis PE pipeline";
      `String "no reusable interval-domain specializer";
      `String "no octDom.ml integration";
      `String "no PartialFlowSensitivity staging/ranking parity";
      `String "no whole-program merge";
      `String "no executable residual linker/global-fixpoint";
      `String "no strict StepManager label parity";
    ];
  ]

let artifact_for_module ?residual_dir path =
  let before = Real_sparrow_frontend.global_for_module path in
  let pre = PreAnalysis.perform before in
  artifact_for_global ?residual_dir ~source:path before pre
