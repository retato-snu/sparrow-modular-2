let ensure_dir path = if not (Sys.file_exists path) then Unix.mkdir path 0o755

let rec ensure_parent path =
  let dir = Filename.dirname path in
  if dir <> "." && dir <> path then begin
    if not (Sys.file_exists dir) then (ensure_parent dir; Unix.mkdir dir 0o755)
  end

let write_file path content = ensure_parent path; let oc = open_out path in output_string oc content; close_out oc

let arg_value name argv default =
  let rec loop = function
    | [] -> default
    | x :: y :: _ when x = name -> y
    | _ :: xs -> loop xs
  in loop argv

let has_flag name argv = List.exists ((=) name) argv
let split_comma s = if s = "" then [] else String.split_on_char ',' s
