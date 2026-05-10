(***********************************************************************)
(* Source-lineage wrapper for real Sparrow AccessAnalysis + SsaDug.     *)
(***********************************************************************)

module AccessSemItv = AccessSem.Make (ItvSem)
module AccessAnalysisItv = AccessAnalysis.Make (AccessSemItv)
module Access = AccessAnalysisItv.Access
module DUGraph = Dug.Make (ItvDom.Mem)
module PowLoc = Access.PowLoc
module Loc = Access.Loc

let boundary = "Global.init -> PreAnalysis.perform -> AccessAnalysis.perform -> SsaDug.make / Dug construction"
let schema_version = "real-sparrow-access-dug/v1"

let compare_string = String.compare
let sort_strings xs = List.sort_uniq compare_string xs
let sort_json xs = List.sort (fun a b -> compare (Yojson.Safe.to_string a) (Yojson.Safe.to_string b)) xs

let loc_to_string = BasicDom.Loc.to_string
let node_to_string = BasicDom.Node.to_string
let proc_to_string = BasicDom.Proc.to_string

let powloc_strings xs = PowLoc.fold (fun loc acc -> loc_to_string loc :: acc) xs [] |> sort_strings
let powloc_json xs = powloc_strings xs |> List.map (fun s -> `String s)
let pownode_json xs = BasicDom.PowNode.fold (fun node acc -> node_to_string node :: acc) xs [] |> sort_strings |> List.map (fun s -> `String s)

let memory_summary mem =
  ItvDom.Mem.fold (fun loc value acc ->
    (`Assoc [
      "location", `String (loc_to_string loc);
      "value", `String (ItvDom.Val.to_string value);
      "bta", `String (if BasicDom.Loc.is_ext_allocsite loc then "dynamic" else "static");
    ]) :: acc)
    mem []
  |> sort_json

let get_locset mem =
  ItvDom.Mem.foldi (fun loc value locset ->
    locset
    |> PowLoc.add loc
    |> PowLoc.union (ItvDom.Val.pow_loc_of_val value)
    |> fun locset ->
       BatSet.fold (fun allocsite acc -> PowLoc.add (BasicDom.Loc.of_allocsite allocsite) acc)
         (ItvDom.Val.allocsites_of_val value) locset)
    mem PowLoc.empty

type access_dug_spec = {
  premem : ItvDom.Mem.t;
  locset : PowLoc.t;
  locset_fs : PowLoc.t;
  pfs : int;
  sem_spec : ItvSem.Spec.t;
}

let derive_for_access_dug global =
  Options.pfs := 100;
  let premem = global.Global.mem in
  let locset = get_locset premem in
  let locset_fs = locset in
  let sem_spec = { ItvSem.Spec.empty with ItvSem.Spec.locset; locset_fs; premem } in
  { premem; locset; locset_fs; pfs = 100; sem_spec }

let source_access_collection_transfer spec =
  ItvSem.run AbsSem.Strong spec.sem_spec

let sparse_spec_json spec =
  `Assoc [
    "premem_summary", `List (memory_summary spec.premem);
    "locset", `List (powloc_json spec.locset);
    "locset_fs", `List (powloc_json spec.locset_fs);
    "pfs", `Int spec.pfs;
    "derivation_mode", `String "default-full-flow-sensitive-input-contract";
    "access_collection_transfer", `Assoc [
      "adapter", `String "access_collection_transfer";
      "source_transfer", `String "ItvSem.run AbsSem.Strong";
      "allowed_call_path", `String "AccessAnalysis.perform -> AccessSem.accessof -> access_collection_transfer";
      "not_claims", `List [
        `String "not SparseAnalysis.perform";
        `String "not Worklist.init";
        `String "not widening";
        `String "not narrowing";
        `String "not convergence";
        `String "not Strong sparse fixpoint parity";
      ];
    ];
  ]

let info_json info =
  let use = Access.Info.useof info in
  let def = Access.Info.defof info in
  `Assoc [
    "use", `List (powloc_json use);
    "def", `List (powloc_json def);
    "access", `List (powloc_json (Access.Info.accessof info));
  ]

let access_node_summaries access =
  Access.fold (fun node info acc ->
    (`Assoc ["node", `String (node_to_string node); "info", info_json info]) :: acc)
    access []
  |> sort_json

let access_proc_summaries access =
  Access.fold_proc (fun pid info acc ->
    (`Assoc ["procedure", `String (proc_to_string pid); "info", info_json info]) :: acc)
    access []
  |> sort_json

let proc_local_summaries access =
  Access.fold_proc_local (fun pid locs acc ->
    (`Assoc ["procedure", `String (proc_to_string pid); "locations", `List (powloc_json locs)]) :: acc)
    access []
  |> sort_json

let proc_reach_summaries global access =
  InterCfg.pidsof global.Global.icfg
  |> List.map (fun pid ->
       `Assoc [
         "procedure", `String (proc_to_string pid);
         "reach", info_json (Access.find_proc_reach pid access);
         "reach_without_local", info_json (Access.find_proc_reach_wo_local pid access);
       ])
  |> sort_json

let loc_index_summaries access =
  PowLoc.fold (fun loc acc ->
    let loc_s = loc_to_string loc in
    (`Assoc [
      "location", `String loc_s;
      "def_nodes", `List (pownode_json (Access.find_def_nodes loc access));
      "use_nodes", `List (pownode_json (Access.find_use_nodes loc access));
    ]) :: acc)
    (Access.total_abslocs access) []
  |> sort_json

let access_json global access =
  `Assoc [
    "total_abslocs", `List (powloc_json (Access.total_abslocs access));
    "nodes", `List (access_node_summaries access);
    "procedures", `List (access_proc_summaries access);
    "proc_reach", `List (proc_reach_summaries global access);
    "proc_local", `List (proc_local_summaries access);
    "program_local", `List (powloc_json (Access.find_program_local access));
    "loc_indexes", `List (loc_index_summaries access);
  ]

let edge_kind src dst =
  if BasicDom.Node.get_pid src = BasicDom.Node.get_pid dst then "intra" else "inter"

let dug_json dug =
  let nodes =
    DUGraph.fold_node (fun node acc -> `String (node_to_string node) :: acc) dug []
    |> sort_json
  in
  let edges =
    DUGraph.fold_edges (fun src dst acc ->
      let labels = DUGraph.get_abslocs src dst dug in
      (`Assoc [
        "src", `String (node_to_string src);
        "dst", `String (node_to_string dst);
        "kind", `String (edge_kind src dst);
        "labels", `List (powloc_json labels);
      ]) :: acc)
      dug []
    |> sort_json
  in
  `Assoc [
    "nodes", `List nodes;
    "edges", `List edges;
    "node_count", `Int (DUGraph.nb_node dug);
    "label_count", `Int (DUGraph.nb_loc dug);
  ]

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

let bta_of_cmd cmd = match cmd with IntraCfg.Cmd.Cexternal _ -> "dynamic" | _ -> "static"

let bta_of_node global node cmd =
  match cmd with
  | IntraCfg.Cmd.Cexternal _ -> "dynamic"
  | IntraCfg.Cmd.Ccall (_, Sparrow_cil.Lval (Sparrow_cil.Var callee, Sparrow_cil.NoOffset), _, _) ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if not (InterCfg.ProcSet.is_empty callees) || List.mem callee.Sparrow_cil.vname (sorted_pids global)
      then "static" else "residual"
  | IntraCfg.Cmd.Ccall _ ->
      let callees = InterCfg.get_callees node global.Global.icfg in
      if InterCfg.ProcSet.is_empty callees then "residual" else "static"
  | _ -> bta_of_cmd cmd

let bta_reason = function
  | "dynamic" -> "external input command"
  | "residual" -> "unresolved external call at module-only boundary"
  | _ -> "module-local access/DUG fact"

let bta_facts global dug =
  let node_facts =
    global.Global.icfg
    |> InterCfg.nodesof
    |> List.map (fun node ->
         let cmd = InterCfg.cmdof global.Global.icfg node in
         let classification = bta_of_node global node cmd in
         `Assoc [
           "id", `String (node_to_string node);
           "kind", `String (command_kind cmd);
           "classification", `String classification;
           "reason", `String (bta_reason classification);
           "depends_on_extern", `Bool (classification <> "static");
         ])
  in
  let edge_facts =
    DUGraph.fold_edges (fun src dst acc ->
      (`Assoc [
        "id", `String (node_to_string src ^ "->" ^ node_to_string dst);
        "kind", `String ("dug-" ^ edge_kind src dst);
        "classification", `String "static";
        "reason", `String "constructed at Access+DUG boundary";
        "depends_on_extern", `Bool false;
      ]) :: acc)
      dug []
  in
  sort_json (node_facts @ edge_facts)

let artifact_for_global ~source _before pre =
  let spec = derive_for_access_dug pre in
  let transfer = source_access_collection_transfer spec in
  let access = AccessAnalysisItv.perform pre spec.locset transfer spec.premem in
  let module Ssa = SsaDug.Make (DUGraph) (Access) in
  let dug = Ssa.make (pre, access, spec.locset_fs) in
  `Assoc [
    "schema_version", `String schema_version;
    "source", `String source;
    "scope", `String "module-only";
    "boundary", `String boundary;
    "lineage", `Assoc [
      "access_sem", `String "sparrow/src/sparse/accessSem.ml:25-33";
      "access_analysis", `String "sparrow/src/sparse/accessAnalysis.ml:36-131";
      "dug", `String "sparrow/src/sparse/dug.ml:55-173";
      "ssa_dug", `String "sparrow/src/sparse/ssaDug.ml:176-352";
      "sparse_spec", `String "sparrow/src/instance/itvAnalysis.ml:319-336";
      "preanalysis", `String "sparrow/src/core/preAnalysis.ml:19-74";
    ];
    "projection", `Assoc [
      "pre_dug_spec_inputs", sparse_spec_json spec;
      "access_summaries", access_json pre access;
      "dug_structure", dug_json dug;
      "bta", `List (bta_facts pre dug);
    ];
    "non_claims", `List [
      `String "no Worklist.init";
      `String "no SparseAnalysis.perform";
      `String "no Strong sparse fixpoint";
      `String "no widening/narrowing/convergence";
      `String "no PartialFlowSensitivity staging/ranking parity";
      `String "no whole-program merge";
      `String "no executable residual linker/global-fixpoint";
    ];
  ]

let artifact_for_module path =
  let before = Real_sparrow_frontend.global_for_module path in
  let pre = PreAnalysis.perform before in
  artifact_for_global ~source:path before pre
