(***********************************************************************)
(*                                                                     *)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(*                                                                     *)
(***********************************************************************)
(** Wrapper around the extracted real Sparrow `Global.init` boundary. *)

type t = Global.t

let init = Global.init

let to_json (g : t) =
  `Assoc [
    "lineage", `Assoc [
      "source", `String "sparrow/src/program/global.ml";
      "boundary", `String "Global.init";
      "callgraph", `String "CallGraph.empty at this boundary";
      "analysis_fields", `String "support-only bottoms; not analysis PE evidence";
      "inter_cfg", `String "extracted sparrow/src/program/interCfg.ml with IntraCfg.init";
    ];
    "file", `String g.file.Sparrow_cil.fileName;
    "global_json", Global.to_json g;
  ]
