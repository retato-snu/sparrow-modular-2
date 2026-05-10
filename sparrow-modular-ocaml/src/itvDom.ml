(***********************************************************************)
(* Copyright (c) 2007-present.                                         *)
(* Programming Research Laboratory (ROPAS), Seoul National University. *)
(* All rights reserved.                                                *)
(*                                                                     *)
(* This software is distributed under the term of the BSD license.     *)
(* See the LICENSE file for details.                                   *)
(***********************************************************************)
(** Support-only bottoms for `Global.t` at the frontend/global milestone. *)
module Mem = struct type t = unit let bot = () end
module Table = struct type t = unit let bot = () end
