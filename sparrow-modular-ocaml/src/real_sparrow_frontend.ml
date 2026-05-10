(***********************************************************************)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(** Source-lineage extraction of Sparrow frontend operations.

    Derived from `sparrow/src/core/frontend.ml`.  The per-module acceptance
    path is `parse_one_file -> make_cfg_info -> Real_sparrow_global.init`.
    Whole-program merge is exposed only for reference comparison with the
    original Sparrow pipeline shape. *)

module C = Sparrow_cil
module F = C.Frontc
module E = C.Errormsg

let parser_identity = "Sparrow Frontend.parseOneFile via GoblintCIL Frontc"

let args file =
  if Filename.check_suffix file ".i" || Filename.check_suffix file ".c" then file
  else failwith ("input must be a C source/preprocessed file: " ^ file)

let parse_one_file file =
  let file = args file in
  E.log "Parsing %s\n" file;
  let parsed = F.parse file () in
  C.Rmtmps.removeUnusedTemps parsed;
  parsed

let parse files =
  match List.map parse_one_file files with
  | [] -> failwith "no input modules"
  | [one] -> one
  | hd :: tl -> C.Mergecil.merge (hd :: tl) "sparrow_merged"

let make_cfg_info file =
  C.Partial.calls_end_basic_blocks file;
  C.Partial.globally_unique_vids file;
  C.iterGlobals file (function
    | C.GFun (fd, _) ->
        C.prepareCFG fd;
        C.computeCFGInfo fd true
    | _ -> ());
  file

let global_for_module path =
  path |> parse_one_file |> make_cfg_info |> Real_sparrow_global.init

let artifact_for_module path =
  let global = global_for_module path in
  `Assoc [
    "source", `String path;
    "parser", `String parser_identity;
    "lineage", `Assoc [
      "frontend", `String "sparrow/src/core/frontend.ml";
      "global", `String "sparrow/src/program/global.ml";
      "inter_cfg", `String "sparrow/src/program/interCfg.ml";
      "path", `String "parseOneFile -> makeCFGinfo -> Global.init";
    ];
    "global", Real_sparrow_global.to_json global;
  ]
