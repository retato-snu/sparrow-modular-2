let modules = ref []
let out_dir = ref ""

let add_module path = modules := !modules @ [path]

let usage = "real_sparrow_frontend_dump --module <file.c>... --out <dir>"

let () =
  Arg.parse
    [
      "--module", Arg.String add_module, "C source module to parse with the real Sparrow frontend path";
      "--out", Arg.Set_string out_dir, "output directory";
    ]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg)))
    usage;
  if !modules = [] then failwith "at least one --module is required";
  if !out_dir = "" then failwith "--out is required";
  let paths = List.map (Sparrow_modular_ocaml.Real_sparrow_artifact.write_module_artifact ~out_dir:!out_dir) !modules in
  let manifest =
    `Assoc [
      "modules", `List (List.map (fun path -> `String path) paths);
      "count", `Int (List.length paths);
      "path", `String "parseOneFile -> makeCFGinfo -> Global.init";
    ]
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter print_endline paths
