module Blind = Sparrow_modular_ocaml.Abstract_speculate_blind_equal
module T = Sparrow_modular_ocaml.Abstract_speculate_stage_types

let expect cond msg = if not cond then failwith msg

let row node value =
  `Assoc [
    "node", `String node;
    "memory", `List [`Assoc ["location", `String "x"; "value", `String value]];
  ]

let () =
  let static_rows = [row "n1" "[0,1]"] in
  let grown_residual_a = .<1>. in
  let grown_residual_b = .<let x = 1 in x + 41>. in
  let left = { Blind.static_rows; residual_values = [T.D grown_residual_a] } in
  let right = { Blind.static_rows = List.rev static_rows; residual_values = [T.D grown_residual_b; T.S 7] } in
  expect (Runcode.run grown_residual_a <> Runcode.run grown_residual_b)
    "test setup error: residual code values should have different structures/results";
  expect (Blind.equal_static_projection left right)
    "blind equality must ignore residual-code structure when static sparse projection is stable";
  let changed = { right with static_rows = [row "n1" "[0,2]"] } in
  expect (not (Blind.equal_static_projection left changed))
    "blind equality must still reject changed static abstract-domain projection";
  let witness = Blind.convergence_witness left right in
  expect (Yojson.Safe.Util.member "ignores_residual_code_structure" witness = `Bool true)
    "blind-equality witness did not record code-structure blindness";
  print_endline "abstract_speculate_metaocaml_sparse_blind_equal: PASS"
