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

val s_exps : Sparrow_cil.exp list -> string
val s_exp : Sparrow_cil.exp -> string
val s_exp_paren : Sparrow_cil.exp -> string
val s_const : Sparrow_cil.constant -> string
val s_type : Sparrow_cil.typ -> string
val s_stmt : Sparrow_cil.stmt -> string
val s_lv : Sparrow_cil.lval -> string
val s_lhost : Sparrow_cil.lhost -> string
val s_exp_paren2 : Sparrow_cil.exp -> string
val s_offset : Sparrow_cil.offset -> string
val s_uop : Sparrow_cil.unop -> string
val s_bop : Sparrow_cil.binop -> string
val s_instr : Sparrow_cil.instr -> string
val s_instrs : Sparrow_cil.instr list -> string
val s_location : Sparrow_cil.location -> string
val eq_lval : Sparrow_cil.lval -> Sparrow_cil.lval -> bool

val rev_binop : Sparrow_cil.binop -> Sparrow_cil.binop
val not_binop : Sparrow_cil.binop -> Sparrow_cil.binop
val make_cond_simple : Sparrow_cil.exp -> Sparrow_cil.exp option
val remove_cast : Sparrow_cil.exp -> Sparrow_cil.exp
val remove_coeff : Sparrow_cil.exp -> Sparrow_cil.exp
val is_unsigned : Sparrow_cil.typ -> bool
val byteSizeOf : Sparrow_cil.typ -> int
