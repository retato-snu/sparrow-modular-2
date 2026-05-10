let modules = ref []
let out_dir = ref ""
let add_module path = modules := !modules @ [path]
let usage = "real_sparrow_access_dug_dump --module <file.c>... --out <dir>"

let artifact_path out_dir source =
  Filename.concat out_dir (Filename.basename source ^ ".access-dug.json")

let write_module source =
  let path = artifact_path !out_dir source in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json path
    (Sparrow_modular_ocaml.Real_sparrow_access_dug.artifact_for_module source);
  path

let () =
  Arg.parse
    [
      "--module", Arg.String add_module, "C source module to run through active real Access+DUG";
      "--out", Arg.Set_string out_dir, "output directory";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !modules = [] then failwith "at least one --module is required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  let paths = List.map write_module !modules in
  let manifest = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_access_dug.schema_version;
    "modules", `List (List.map (fun path -> `String path) paths);
    "count", `Int (List.length paths);
    "boundary", `String Sparrow_modular_ocaml.Real_sparrow_access_dug.boundary;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter print_endline paths
