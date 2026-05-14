let witnesses = ref []
let out_dir = ref ""
let usage =
  "abstract_speculate_residual_linking_oracle_suite_dump --witness <id|category|residual_csv|oracle_csv>... --out <dir>"

module AS = Sparrow_modular_ocaml.Abstract_speculate_pe
module Linker = Sparrow_modular_ocaml.Abstract_speculate_residual_linker
module MetaSparse = Sparrow_modular_ocaml.Abstract_speculate_meta_sparse
module Artifact = Sparrow_modular_ocaml.Real_sparrow_artifact
module Oracle = Sparrow_modular_ocaml.Real_sparrow_premerge_linked_observer

let split ch s = String.split_on_char ch s |> List.filter ((<>) "")

let parse_witness spec =
  match String.split_on_char '|' spec with
  | [id; category; residual_csv; oracle_csv] ->
      let residual_modules = split ',' residual_csv in
      let oracle_modules = split ',' oracle_csv in
      if id = "" || category = "" || residual_modules = [] || oracle_modules = [] then
        failwith ("invalid witness: " ^ spec);
      witnesses := !witnesses @ [id, category, residual_modules, oracle_modules]
  | _ -> failwith ("invalid witness spec, expected id|category|residual_csv|oracle_csv: " ^ spec)

let artifact_path out_dir source =
  Filename.concat out_dir (Filename.basename source ^ ".abstract-speculate-pe.json")

let residual_dir out_dir = Filename.concat out_dir "residual"
let linked_artifact_path out_dir = Filename.concat out_dir "abstract-speculate-residual-linking-pe.linked.json"
let oracle_artifact_path out_dir id = Filename.concat out_dir (id ^ ".premerge-linked-observer.json")
let oracle_residual_dir out_dir = Filename.concat out_dir "oracle-residual"
let doc_path = "sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md"

let write_module_result out_dir source =
  let result = MetaSparse.run_stage1 source in
  let path = artifact_path out_dir source in
  let artifact = AS.artifact_for_stage1_result ~residual_dir:(residual_dir out_dir) result in
  Artifact.write_json path artifact;
  result, artifact, path

let write_residual_witness ~witness_dir ~witness_id modules =
  Artifact.mkdir_p witness_dir;
  Artifact.mkdir_p (residual_dir witness_dir);
  let module_results = List.map (write_module_result witness_dir) modules in
  let bundles =
    module_results
    |> List.map (fun (result, artifact, path) -> Linker.make_bundle ~artifact_path:path result artifact)
  in
  let analyzer = Linker.make_analyzer ~linked_id:("abstract-speculate-residual-linking-oracle-suite:" ^ witness_id) bundles in
  let inputs = List.map Linker.stage2_input_for_bundle bundles in
  let output = Linker.execute analyzer inputs in
  let linked_path = linked_artifact_path witness_dir in
  Linker.write_artifact ~path:linked_path ~doc_path analyzer output;
  let manifest = `Assoc [
    "schema_version", `String Linker.schema_version;
    "witness_id", `String witness_id;
    "boundary", `String "module-local PE artifacts followed by residual linking";
    "artifacts", `List (module_results |> List.map (fun (_, _, path) -> `String path));
    "linked_artifact", `String linked_path;
    "modules", `List (List.map (fun path -> `String path) modules);
    "count", `Int (List.length modules);
  ] in
  Artifact.write_json (Filename.concat witness_dir "residual-manifest.json") manifest;
  linked_path, Filename.concat witness_dir "residual-manifest.json"

let write_oracle_witness ~witness_dir ~witness_id modules =
  let oracle_dir = Filename.concat witness_dir "oracle" in
  Artifact.mkdir_p oracle_dir;
  let path = oracle_artifact_path oracle_dir witness_id in
  let json = Oracle.artifact_for_group ~residual_dir:(oracle_residual_dir oracle_dir) ~group_name:witness_id modules in
  Artifact.write_json path json;
  let manifest = `Assoc [
    "schema_version", `String Oracle.schema_version;
    "oracle_reference_kind", `String "premerge-linked-observer";
    "witness_id", `String witness_id;
    "artifacts", `List [`String path];
    "modules", `List (List.map (fun m -> `String m) modules);
    "count", `Int 1;
    "boundary", `String Oracle.boundary;
  ] in
  Artifact.write_json (Filename.concat oracle_dir "oracle-manifest.json") manifest;
  path, Filename.concat oracle_dir "oracle-manifest.json"

let write_witness (witness_id, category, residual_modules, oracle_modules) =
  let witness_dir = Filename.concat !out_dir witness_id in
  let residual_artifact, residual_manifest =
    write_residual_witness ~witness_dir ~witness_id residual_modules
  in
  let oracle_artifact, oracle_manifest =
    write_oracle_witness ~witness_dir ~witness_id oracle_modules
  in
  `Assoc [
    "witness_id", `String witness_id;
    "category", `String category;
    "residual_modules", `List (List.map (fun m -> `String m) residual_modules);
    "oracle_modules", `List (List.map (fun m -> `String m) oracle_modules);
    "residual_linked_artifact", `String residual_artifact;
    "residual_manifest", `String residual_manifest;
    "premerge_observer_artifact", `String oracle_artifact;
    "premerge_manifest", `String oracle_manifest;
  ]

let () =
  Arg.parse
    ["--witness", Arg.String parse_witness, "witness spec id|category|residual_csv|oracle_csv";
     "--out", Arg.Set_string out_dir, "output directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !witnesses = [] then failwith "at least one --witness is required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  Artifact.mkdir_p !out_dir;
  let witness_json = List.map write_witness !witnesses in
  let manifest = `Assoc [
    "schema_version", `String "abstract-speculate-residual-linking-oracle-suite/v1";
    "suite_id", `String "abstract-speculate-residual-linking-oracle-suite";
    "oracle_reference_kind", `String "premerge-linked-observer";
    "artifact_schema_status", `String "prototype-non-public";
    "witnesses", `List witness_json;
    "count", `Int (List.length witness_json);
    "non_claims", `List [
      `String "no proof assistant mechanization";
      `String "no final artifact schema freeze";
      `String "no broad arbitrary-C coverage";
    ];
  ] in
  let manifest_path = Filename.concat !out_dir "manifest.json" in
  Artifact.write_json manifest_path manifest;
  print_endline manifest_path
