let groups = ref []
let out_dir = ref ""
let usage = "real_sparrow_premerge_linked_observer_dump --group <name:file.c,file.c>... --out <dir>"

let split_once s ch =
  match String.index_opt s ch with
  | None -> failwith ("expected separator in group: " ^ s)
  | Some i -> String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1)

let parse_group spec =
  let name, files = split_once spec ':' in
  let modules = String.split_on_char ',' files |> List.filter ((<>) "") in
  if name = "" || modules = [] then failwith ("invalid group: " ^ spec);
  groups := !groups @ [name, modules]

let artifact_path out_dir name = Filename.concat out_dir (name ^ ".premerge-linked-observer.json")
let residual_dir out_dir = Filename.concat out_dir "residual"

let write_group (name, modules) =
  let path = artifact_path !out_dir name in
  let json =
    Sparrow_modular_ocaml.Real_sparrow_premerge_linked_observer.artifact_for_group
      ~residual_dir:(residual_dir !out_dir) ~group_name:name modules
  in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json path json;
  path

let () =
  Arg.parse
    ["--group", Arg.String parse_group, "linked fixture group";
     "--out", Arg.Set_string out_dir, "output directory"]
    (fun arg -> raise (Arg.Bad ("unexpected argument: " ^ arg))) usage;
  if !groups = [] then failwith "at least one --group is required";
  if !out_dir = "" then failwith "--out is required";
  Sparrow_modular_ocaml.Sparrow_cil.initCIL ();
  let paths = List.map write_group !groups in
  let manifest = `Assoc [
    "schema_version", `String Sparrow_modular_ocaml.Real_sparrow_premerge_linked_observer.schema_version;
    "groups", `List (List.map (fun (name, modules) -> `Assoc ["name", `String name; "modules", `List (List.map (fun m -> `String m) modules)]) !groups);
    "artifacts", `List (List.map (fun path -> `String path) paths);
    "count", `Int (List.length paths);
    "boundary", `String Sparrow_modular_ocaml.Real_sparrow_premerge_linked_observer.boundary;
  ] in
  Sparrow_modular_ocaml.Real_sparrow_artifact.write_json (Filename.concat !out_dir "manifest.json") manifest;
  List.iter print_endline paths
