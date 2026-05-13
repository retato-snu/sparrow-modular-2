let modules = ref []
let out_dir = ref ""
let add_module path = modules := !modules @ [path]
let usage = "abstract_speculate_residual_linking_pe_dump --module <file.c>... --out <dir>"

module AS = Sparrow_modular_ocaml.Abstract_speculate_pe
module Linker = Sparrow_modular_ocaml.Abstract_speculate_residual_linker
module MetaSparse = Sparrow_modular_ocaml.Abstract_speculate_meta_sparse
module Artifact = Sparrow_modular_ocaml.Real_sparrow_artifact

let artifact_path out_dir source =
  Filename.concat out_dir (Filename.basename source ^ ".abstract-speculate-pe.json")

let residual_dir out_dir = Filename.concat out_dir "residual"
let linked_artifact_path out_dir = Filename.concat out_dir "abstract-speculate-residual-linking-pe.linked.json"
let doc_path = "sparrow-modular-ocaml/doc/experiments/abstract-speculate-residual-linking-pe.md"

let write_module_result source =
  let result = MetaSparse.run_stage1 source in
  let path = artifact_path !out_dir source in
  let artifact = AS.artifact_for_stage1_result ~residual_dir:(residual_dir !out_dir) result in
  Artifact.write_json path artifact;
  result, artifact, path

let () =
  Arg.parse
    ["--module", Arg.String add_module, "C source module to run through Abstract Speculate residual-linking PE";
     "--out", Arg.Set_string out_dir, "output directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if List.length !modules < 2 then failwith "at least two --module inputs are required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  Artifact.mkdir_p !out_dir;
  Artifact.mkdir_p (residual_dir !out_dir);
  let module_results = List.map write_module_result !modules in
  let bundles =
    module_results
    |> List.map (fun (result, artifact, path) -> Linker.make_bundle ~artifact_path:path result artifact)
  in
  let linked_id = "abstract-speculate-residual-linking-pe" in
  let analyzer = Linker.make_analyzer ~linked_id bundles in
  let inputs = List.map Linker.stage2_input_for_bundle bundles in
  let output = Linker.execute analyzer inputs in
  let linked_path = linked_artifact_path !out_dir in
  Linker.write_artifact ~path:linked_path ~doc_path analyzer output;
  let manifest = `Assoc [
    "schema_version", `String Linker.schema_version;
    "boundary", `String "module-local PE artifacts followed by residual linking";
    "artifacts", `List (module_results |> List.map (fun (_, _, path) -> `String path));
    "linked_artifact", `String linked_path;
    "modules", `List (List.map (fun path -> `String path) !modules);
    "count", `Int (List.length !modules);
  ] in
  Artifact.write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter (fun (_, _, path) -> print_endline path) module_results;
  print_endline linked_path
