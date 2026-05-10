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
(** Intra-procedural CFG *)
module Node : sig
  include AbsDom.HASHABLE_SET
  val entry : t
  val exit : t
  val id : t -> int
end

module NodeSet : BatSet.S with type elt = Node.t

module Cmd : sig
  type t =
  | Cinstr of Sparrow_cil.instr list
  | Cif of Sparrow_cil.exp * Sparrow_cil.block * Sparrow_cil.block * Sparrow_cil.location
  | CLoop of Sparrow_cil.location
  (* final graph has the following cmds only *)
  | Cset of Sparrow_cil.lval * Sparrow_cil.exp * Sparrow_cil.location
  | Cexternal of Sparrow_cil.lval * Sparrow_cil.location
  | Calloc of Sparrow_cil.lval * alloc * static * Sparrow_cil.location
  | Csalloc of Sparrow_cil.lval * string * Sparrow_cil.location
  | Cfalloc of Sparrow_cil.lval * Sparrow_cil.fundec * Sparrow_cil.location
  | Cassume of Sparrow_cil.exp * Sparrow_cil.location
  | Ccall of Sparrow_cil.lval option * Sparrow_cil.exp * Sparrow_cil.exp list * Sparrow_cil.location
  | Creturn of Sparrow_cil.exp option * Sparrow_cil.location
  | Casm of Sparrow_cil.attributes * string list *
            (string option * string * Sparrow_cil.lval) list *
            (string option * string * Sparrow_cil.exp) list *
            string list * Sparrow_cil.location
  | Cskip
  and alloc = Array of Sparrow_cil.exp | Struct of Sparrow_cil.compinfo
  and static = bool

  val fromCilStmt : Sparrow_cil.stmtkind -> t
  val to_string : t -> string
end

(** Abstract type of intra-procedural CFG *)
type t
and node = Node.t
and cmd = Cmd.t

val init : Sparrow_cil.fundec -> Sparrow_cil.location -> t
val generate_global_proc : Sparrow_cil.global list -> Sparrow_cil.fundec -> t

val get_pid : t -> string
val get_formals : t -> Sparrow_cil.varinfo list
val get_scc_list : t -> node list list

val nodesof : t -> node list
val entryof : t -> node
val exitof : t -> node
val callof : node -> t -> node
val returnof : node -> t -> node

val is_entry : node -> bool
val is_exit : node -> bool
val is_callnode : node -> t -> bool
val is_returnnode : node -> t -> bool
val is_inside_loop : node -> t -> bool

val find_cmd : node -> t ->  cmd

val unreachable_node : t -> NodeSet.t

val compute_scc : t -> t

val optimize : t -> t

val fold_node : (node -> 'a -> 'a) -> t -> 'a -> 'a
val fold_edges : (node -> node -> 'a -> 'a) -> t -> 'a -> 'a

(** {2 Predecessors and Successors } *)

val pred : node -> t -> node list
val succ : node -> t -> node list

(** {2 Graph Manipulation } *)

val add_cmd : node -> cmd -> t -> t
val add_new_node : node -> cmd -> node -> t -> t
val add_node_with_cmd : node -> cmd -> t -> t
val add_edge : node -> node -> t -> t
val remove_node : node -> t -> t

(** {2 Dominators } *)

val compute_dom : t -> t

(** [dom_fronts n g] returns dominance frontiers of node [n] in graph [g] *)
val dom_fronts : node -> t -> NodeSet.t
val children_of_dom_tree : node -> t -> NodeSet.t
val parent_of_dom_tree : node -> t -> node option

(** {2 Print } *)

val print_dot : out_channel -> t -> unit
val to_json : t -> Yojson.Safe.t
