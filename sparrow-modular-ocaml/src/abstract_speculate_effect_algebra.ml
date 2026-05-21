let schema_id = "abstract-speculate-external-summary-effect-algebra/v1"

type memory_kind =
  | Memory_read
  | Memory_write
  | Memory_read_write

type domain =
  | Return
  | Memory of memory_kind
  | Alias
  | Heap
  | Struct_field
  | Array_segment
  | Taint
  | Product_pair

type observation =
  | Return_observation
  | Global_observation
  | Pointer_observation
  | Taint_observation
  | Product_pair_observation

type undefined_reason =
  | Incompatible_domain
  | Incompatible_provenance
  | Incompatible_path
  | Missing_alias_evidence
  | Lossy_heap_projection
  | Unsupported_observation
  | Invalid_composition_order
  | Taint_product_mismatch

type 'a operation_result =
  | Defined of 'a
  | Undefined of undefined_reason

type provenance = {
  provider_module : string;
  provider_source_hash : string;
  provider_artifact_path : string;
  provider_phase_index : int;
  export_name : string;
}

type t = {
  effect_id_ : string;
  domain : domain;
  provenance : provenance;
  path : string list;
  payload : (string * string) list;
  evidence_path : string list;
  order : int;
  identity : bool;
}

type projection = {
  projection_id_ : string;
  observation : observation;
  source_effect_id : string;
  source_provenance_id : string;
  evidence_path_ : string list;
}

let non_empty s = String.length s > 0
let all_non_empty xs = List.for_all non_empty xs

let memory_kind_to_string = function
  | Memory_read -> "memory-read"
  | Memory_write -> "memory-write"
  | Memory_read_write -> "memory-read-write"

let domain_to_string = function
  | Return -> "return"
  | Memory kind -> "memory:" ^ memory_kind_to_string kind
  | Alias -> "alias"
  | Heap -> "heap"
  | Struct_field -> "struct-field"
  | Array_segment -> "array-segment"
  | Taint -> "taint"
  | Product_pair -> "product-pair"

let observation_to_string = function
  | Return_observation -> "return"
  | Global_observation -> "global"
  | Pointer_observation -> "pointer"
  | Taint_observation -> "taint"
  | Product_pair_observation -> "product-pair"

let undefined_reason_to_string = function
  | Incompatible_domain -> "incompatible_domain"
  | Incompatible_provenance -> "incompatible_provenance"
  | Incompatible_path -> "incompatible_path"
  | Missing_alias_evidence -> "missing_alias_evidence"
  | Lossy_heap_projection -> "lossy_heap_projection"
  | Unsupported_observation -> "unsupported_observation"
  | Invalid_composition_order -> "invalid_composition_order"
  | Taint_product_mismatch -> "taint_product_mismatch"

let path_id path = String.concat "/" path

let provenance_id p =
  String.concat ":"
    [ p.provider_module; p.provider_source_hash; string_of_int p.provider_phase_index;
      p.export_name ]

let provenance_module p = p.provider_module
let provenance_source_hash p = p.provider_source_hash
let provenance_artifact_path p = p.provider_artifact_path
let provenance_phase_index p = p.provider_phase_index
let provenance_export_name p = p.export_name

let make_provenance ~provider_module ~provider_source_hash ~provider_artifact_path
    ~provider_phase_index ~export_name =
  if provider_phase_index < 0 then Undefined Incompatible_provenance
  else if
    not
      (all_non_empty
         [ provider_module; provider_source_hash; provider_artifact_path; export_name ])
  then Undefined Incompatible_provenance
  else
    Defined
      { provider_module; provider_source_hash; provider_artifact_path;
        provider_phase_index; export_name }

let make_effect ~domain ~provenance ~path ~payload ~evidence_path ~order =
  if not (all_non_empty path) then Undefined Incompatible_path
  else if not (all_non_empty evidence_path) then Undefined Incompatible_provenance
  else
    let effect_id_ =
      match (domain, path) with
      | Return, [ return_location ] ->
          String.concat ":"
            [ provenance.provider_module; provenance.export_name; "return"; return_location ]
      | _ -> String.concat ":" [ provenance_id provenance; domain_to_string domain; path_id path ]
    in
    Defined
      { effect_id_; domain; provenance; path; payload; evidence_path; order;
        identity = false }

let make_return ~provenance ~return_location ~abstract_value ~evidence_path =
  if not (non_empty abstract_value) then Undefined Incompatible_domain
  else
    make_effect ~domain:Return ~provenance ~path:[ return_location ]
      ~payload:[ "abstract_value", abstract_value ] ~evidence_path ~order:0

let make_memory_transition ~provenance ~kind ~location ~value ~alias_evidence
    ~evidence_path =
  let payload =
    ("value", value)
    :: (match alias_evidence with None -> [] | Some alias -> [ "alias_evidence", alias ])
  in
  if not (non_empty value) then Undefined Incompatible_domain
  else if kind <> Memory_read && alias_evidence = None && String.contains location '*' then
    Undefined Missing_alias_evidence
  else
    make_effect ~domain:(Memory kind) ~provenance ~path:[ location ] ~payload
      ~evidence_path ~order:1

let make_alias ~provenance ~source ~target ~evidence_path =
  make_effect ~domain:Alias ~provenance ~path:[ source; target ]
    ~payload:[ "source", source; "target", target ] ~evidence_path ~order:1

let make_heap ~provenance ~allocation ~location ~precise ~evidence_path =
  if not precise then Undefined Lossy_heap_projection
  else
    make_effect ~domain:Heap ~provenance ~path:[ allocation; location ]
      ~payload:[ "allocation", allocation; "location", location ] ~evidence_path ~order:1

let make_struct_field ~provenance ~symbol ~location ~value ~related_effect_ids =
  make_effect ~domain:Struct_field ~provenance ~path:[ symbol; location ]
    ~payload:
      ([ "symbol", symbol; "location", location;
         "value", Yojson.Safe.to_string value ]
      @ List.map (fun id -> ("related_effect_id", id)) related_effect_ids)
    ~evidence_path:[ "provider_row.struct_field" ] ~order:1

let make_taint ~provenance ~source ~sink ~taint_state ~evidence_path =
  make_effect ~domain:Taint ~provenance ~path:[ source; sink ]
    ~payload:[ "source", source; "sink", sink; "taint_state", taint_state ]
    ~evidence_path ~order:2

let make_product_pair ~provenance ~left_effect ~right_effect ~evidence_path =
  if left_effect.domain <> Taint && right_effect.domain <> Taint then
    Undefined Taint_product_mismatch
  else if not (String.equal (provenance_id left_effect.provenance) (provenance_id provenance))
          || not (String.equal (provenance_id right_effect.provenance) (provenance_id provenance))
  then Undefined Incompatible_provenance
  else
    make_effect ~domain:Product_pair ~provenance
      ~path:(left_effect.path @ right_effect.path)
      ~payload:
        [ "left_effect_id", left_effect.effect_id_;
          "right_effect_id", right_effect.effect_id_ ]
      ~evidence_path ~order:3

let identity ~provenance =
  { effect_id_ = provenance_id provenance ^ ":identity"; domain = Return; provenance;
    path = [ "identity" ]; payload = []; evidence_path = [ "identity" ]; order = min_int;
    identity = true }

let effect_id eff = eff.effect_id_
let effect_domain eff = eff.domain
let effect_provenance eff = eff.provenance
let effect_path eff = eff.path
let effect_evidence_path eff = eff.evidence_path
let effect_payload eff = eff.payload
let projection_id p = p.projection_id_
let projection_observation p = p.observation
let projection_source_effect_id p = p.source_effect_id
let projection_source_provenance_id p = p.source_provenance_id
let projection_evidence_path p = p.evidence_path_

let same_provenance a b =
  String.equal (provenance_id a.provenance) (provenance_id b.provenance)

let same_path a b = a.path = b.path

let compose a b =
  if a.identity then Defined b
  else if b.identity then Defined a
  else if not (same_provenance a b) then Undefined Incompatible_provenance
  else if a.order > b.order then Undefined Invalid_composition_order
  else if a.domain <> b.domain then Undefined Incompatible_domain
  else if not (same_path a b) then Undefined Incompatible_path
  else
    Defined
      { b with
        effect_id_ = a.effect_id_ ^ "+" ^ b.effect_id_;
        payload = a.payload @ b.payload;
        evidence_path = a.evidence_path @ b.evidence_path;
        order = max a.order b.order }

let join a b =
  if a.identity then Defined b
  else if b.identity then Defined a
  else if not (same_provenance a b) then Undefined Incompatible_provenance
  else if a.domain <> b.domain then Undefined Incompatible_domain
  else if not (same_path a b) then Undefined Incompatible_path
  else
    let effect_id_ =
      if String.compare a.effect_id_ b.effect_id_ <= 0 then a.effect_id_ ^ "|" ^ b.effect_id_
      else b.effect_id_ ^ "|" ^ a.effect_id_
    in
    let evidence_path = List.sort_uniq String.compare (a.evidence_path @ b.evidence_path) in
    Defined
      { a with effect_id_; payload = List.sort_uniq compare (a.payload @ b.payload);
        evidence_path }

let is_prefix prefix xs =
  let rec loop p x =
    match (p, x) with
    | [], _ -> true
    | ph :: pt, xh :: xt -> String.equal ph xh && loop pt xt
    | _ :: _, [] -> false
  in
  loop prefix xs

let restrict ~path eff =
  if path = [] || is_prefix path eff.path then Defined { eff with path }
  else Undefined Incompatible_path

let observation_supported observation domain =
  match (observation, domain) with
  | Return_observation, Return -> true
  | Global_observation, Memory _ -> true
  | Pointer_observation, Memory _ -> true
  | Taint_observation, Taint -> true
  | Product_pair_observation, Product_pair -> true
  | _ -> false

let observe observation eff =
  if observation_supported observation eff.domain then
    let projection_id_ =
      String.concat ":" [ eff.effect_id_; "projection"; observation_to_string observation ]
    in
    Defined
      { projection_id_; observation; source_effect_id = eff.effect_id_;
        source_provenance_id = provenance_id eff.provenance;
        evidence_path_ = eff.evidence_path }
  else if observation = Pointer_observation then Undefined Missing_alias_evidence
  else Undefined Unsupported_observation

let same_defined_effect x y =
  match (x, y) with
  | Defined a, Defined b ->
      String.equal a.effect_id_ b.effect_id_ && a.path = b.path && a.domain = b.domain
  | Undefined a, Undefined b -> a = b
  | _ -> false

let same_defined_projection x y =
  match (x, y) with
  | Defined a, Defined b -> String.equal a.projection_id_ b.projection_id_
  | Undefined a, Undefined b -> a = b
  | _ -> false

let compose_identity_holds eff =
  same_defined_effect (compose (identity ~provenance:eff.provenance) eff) (Defined eff)
  && same_defined_effect (compose eff (identity ~provenance:eff.provenance)) (Defined eff)

let compose_associative_holds a b c =
  match compose a b with
  | Undefined _ -> false
  | Defined ab ->
      (match compose b c with
      | Undefined _ -> false
      | Defined bc -> same_defined_effect (compose ab c) (compose a bc))

let join_idempotent_holds eff =
  match join eff eff with
  | Defined joined ->
      joined.domain = eff.domain && joined.path = eff.path && same_provenance joined eff
  | Undefined _ -> false

let join_commutative_holds a b = same_defined_effect (join a b) (join b a)

let restrict_idempotent_holds ~path eff =
  match restrict ~path eff with
  | Undefined _ -> false
  | Defined once -> same_defined_effect (restrict ~path once) (Defined once)

let projection_stable observation eff =
  same_defined_projection (observe observation eff) (observe observation eff)
