module T = Sparrow_modular_ocaml.Abstract_speculate_stage_types

let expect cond msg = if not cond then failwith msg
let dynamic n =
  T.make_dynamic_cell
    ~location:"x"
    ~abstract_value:"dynamic-test-value"
    ~semantic_source:"unit-test-dynamic"
    ~code:.<fun _ -> n>.
let static n =
  T.make_static_cell
    ~location:"x"
    ~abstract_value:"static-test-value"
    ~semantic_source:"unit-test-static"
    ~ordinal:n
let event_ok event op =
  event.T.op = op &&
  event.after_stage = "D" &&
  event.typed_code_present &&
  not event.approximated_to_top

let () =
  let d = dynamic 7 in
  let s = static 1 in
  let joined, join_event = T.staged_join d s in
  expect (T.stage_of_cell joined = "D") "join must preserve D, not erase it";
  let dummy_input = { T.extern_effects = `Assoc [] } in
  expect (Runcode.run (Option.get (T.dynamic_code joined)) dummy_input = 7) "join must preserve typed code";
  expect (event_ok join_event "join") "join event must retain typed D evidence";
  let widened, widen_event = T.staged_widen d s in
  expect (T.stage_of_cell widened = "D") "widen must preserve D";
  expect (event_ok widen_event "widen") "widen event must reject top approximation";
  let narrowed, narrow_event = T.staged_narrow d s in
  expect (T.stage_of_cell narrowed = "D") "narrow must preserve D";
  expect (event_ok narrow_event "narrow") "narrow event must reject top approximation";
  expect (T.staged_order d (dynamic 99)) "order must compare dynamic static projection without forcing code equality";
  let unstable, unstable_event = T.staged_unstable (static 0) d in
  expect unstable "unstable detection must observe staged dynamic change";
  expect (event_ok unstable_event "unstable") "unstable event must carry D";
  let propagated, propagate_event = T.staged_propagate d in
  expect (T.stage_of_cell propagated = "D") "propagation must carry D through worklist";
  expect (event_ok propagate_event "propagate") "propagate event must carry typed D";
  print_endline "abstract_speculate_staged_lattice: PASS"
