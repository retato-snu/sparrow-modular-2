(***********************************************************************)
(* Typed ExternalSummary projection helpers.                           *)
(***********************************************************************)

module Algebra = Abstract_speculate_effect_algebra

let observe_return eff = Algebra.observe Algebra.Return_observation eff
let observe_global eff = Algebra.observe Algebra.Global_observation eff
let observe_pointer eff = Algebra.observe Algebra.Pointer_observation eff
let observe_taint eff = Algebra.observe Algebra.Taint_observation eff
let observe_product_pair eff = Algebra.observe Algebra.Product_pair_observation eff
let evidence_paths projection = Algebra.projection_evidence_path projection
