(***********************************************************************)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(***********************************************************************)
(** Support-only extraction for frontend/global milestone. *)

module Proc = InterCfg.Proc
module Node = InterCfg.Node
module NodeSet = InterCfg.NodeSet
module PowNode = InterCfg.NodeSet

module PowProc = struct
  include InterCfg.ProcSet
  let bot = empty
  let to_string (set : t) : string = "{" ^ fold (fun x acc -> if acc = "" then x else acc ^ "," ^ x) set "" ^ "}"
end

module Dump = struct
  type t = unit
  let empty = ()
  let remove _ t = t
end
