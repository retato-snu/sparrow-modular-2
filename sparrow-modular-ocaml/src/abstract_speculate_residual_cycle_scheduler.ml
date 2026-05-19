(***********************************************************************)
(* Deterministic residual-cycle scheduling evidence.                    *)
(***********************************************************************)

type edge_kind =
  | Direct_program_call
  | Residual_call_binding
  | Residual_dependency

type provenance_level =
  | Direct_program_callgraph
  | Residual_call_binding_provenance
  | Residual_dependency_only

type edge = {
  edge_kind : edge_kind;
  source : string;
  importer_or_caller : string;
  provider_or_callee : string;
  symbol : string;
  provenance_level : provenance_level;
  evidence_id : string;
}

type scc = {
  scc_id : string;
  members : string list;
  edges : edge list;
  is_cyclic : bool;
  callgraph_backed : bool;
}

type t = {
  scc_id : string;
  nodes : string list;
  edges : edge list;
  sccs : scc list;
  worklist_order : string list;
  callgraph_backed : bool;
  source : string;
  diagnostics : string list;
}

let sort_strings xs = List.sort_uniq String.compare xs

let compare_edge left right =
  compare
    (left.importer_or_caller, left.provider_or_callee, left.symbol,
     left.evidence_id, left.source)
    (right.importer_or_caller, right.provider_or_callee, right.symbol,
     right.evidence_id, right.source)

let sort_edges edges = List.sort_uniq compare_edge edges

let edge_kind_to_string = function
  | Direct_program_call -> "direct-program-call"
  | Residual_call_binding -> "residual-call-binding"
  | Residual_dependency -> "residual-dependency"

let provenance_level_to_string = function
  | Direct_program_callgraph -> "direct-program-callgraph"
  | Residual_call_binding_provenance -> "residual-call-binding"
  | Residual_dependency_only -> "residual-dependency-only"

let edge_has_callgraph_provenance edge =
  match edge.provenance_level with
  | Direct_program_callgraph | Residual_call_binding_provenance -> true
  | Residual_dependency_only -> false

let residual_call_binding_edge ~importer_module ~provider_module ~import_name ~export_name =
  {
    edge_kind = Residual_call_binding;
    source = "linked-import-export-declaration";
    importer_or_caller = importer_module;
    provider_or_callee = provider_module;
    symbol = import_name ^ "<-" ^ export_name;
    provenance_level = Residual_call_binding_provenance;
    evidence_id =
      "residual-call-binding:" ^ importer_module ^ ":" ^ import_name ^
      "<-" ^ provider_module ^ ":" ^ export_name;
  }

let nodes_for_edges edges =
  edges
  |> List.concat_map (fun edge -> [edge.importer_or_caller; edge.provider_or_callee])
  |> sort_strings

let normalized_nodes ~nodes ~edges =
  sort_strings (nodes @ nodes_for_edges edges)

let outgoing edges node =
  edges
  |> List.filter_map (fun edge ->
       if edge.importer_or_caller = node then Some edge.provider_or_callee else None)
  |> sort_strings

let reachable edges source target =
  let rec visit seen node =
    node = target ||
    if List.mem node seen then false
    else outgoing edges node |> List.exists (visit (node :: seen))
  in
  visit [] source

let scc_groups ~nodes ~edges =
  let edges = sort_edges edges in
  let ids = normalized_nodes ~nodes ~edges in
  let rec loop assigned groups = function
    | [] -> List.rev groups
    | id :: rest when List.mem id assigned -> loop assigned groups rest
    | id :: rest ->
        let group =
          ids
          |> List.filter (fun other -> reachable edges id other && reachable edges other id)
          |> sort_strings
        in
        loop (assigned @ group) (group :: groups) rest
  in
  loop [] [] ids

let edges_for_group group edges =
  edges
  |> List.filter (fun edge ->
       List.mem edge.importer_or_caller group && List.mem edge.provider_or_callee group)
  |> sort_edges

let group_is_cyclic group edges =
  List.length group > 1 ||
  edges |> List.exists (fun edge -> edge.importer_or_caller = edge.provider_or_callee)

let scc_callgraph_backed edges =
  edges <> [] && List.for_all edge_has_callgraph_provenance edges

let source_for_schedule ~has_cycles ~callgraph_backed =
  if not has_cycles then "acyclic-residual-scheduler"
  else if callgraph_backed then "residual-call-binding-callgraph"
  else "residual-dependency-scheduler"

let compute ~scc_id ~nodes ~edges =
  let edges = sort_edges edges in
  let nodes = normalized_nodes ~nodes ~edges in
  let groups = scc_groups ~nodes ~edges in
  let sccs =
    groups
    |> List.mapi (fun index members ->
         let group_edges = edges_for_group members edges in
         let is_cyclic = group_is_cyclic members group_edges in
         {
           scc_id = scc_id ^ ":" ^ string_of_int (index + 1);
           members;
           edges = group_edges;
           is_cyclic;
           callgraph_backed = (not is_cyclic) || scc_callgraph_backed group_edges;
         })
  in
  let cyclic_sccs = List.filter (fun (component : scc) -> component.is_cyclic) sccs in
  let scc_is_callgraph_backed (component : scc) =
    let ({ callgraph_backed; _ } : scc) = component in
    callgraph_backed
  in
  let scc_identifier (component : scc) =
    let ({ scc_id; _ } : scc) = component in
    scc_id
  in
  let callgraph_backed =
    cyclic_sccs <> [] && List.for_all scc_is_callgraph_backed cyclic_sccs
  in
  let diagnostics =
    if cyclic_sccs = [] then ["no-cyclic-scc"]
    else
      cyclic_sccs
      |> List.filter (fun component -> not (scc_is_callgraph_backed component))
      |> List.map (fun component -> "cyclic-scc-lacks-callgraph-provenance:" ^ scc_identifier component)
  in
  let worklist_order =
    sccs
    |> List.concat_map (fun scc -> scc.members)
    |> sort_strings
  in
  {
    scc_id;
    nodes;
    edges;
    sccs;
    worklist_order;
    callgraph_backed;
    source = source_for_schedule ~has_cycles:(cyclic_sccs <> []) ~callgraph_backed;
    diagnostics;
  }

let callgraph_backed schedule = schedule.callgraph_backed
let source schedule = schedule.source
let edges schedule = schedule.edges
let sccs schedule = schedule.sccs

let edge_to_json edge =
  `Assoc [
    "edge_kind", `String (edge_kind_to_string edge.edge_kind);
    "source", `String edge.source;
    "importer_or_caller", `String edge.importer_or_caller;
    "provider_or_callee", `String edge.provider_or_callee;
    "symbol", `String edge.symbol;
    "provenance_level", `String (provenance_level_to_string edge.provenance_level);
    "evidence_id", `String edge.evidence_id;
  ]

let scc_to_json (component : scc) =
  `Assoc [
    "scc_id", `String component.scc_id;
    "members", `List (List.map (fun member -> `String member) component.members);
    "edges", `List (List.map edge_to_json component.edges);
    "is_cyclic", `Bool component.is_cyclic;
    "callgraph_backed", `Bool component.callgraph_backed;
  ]

let to_json schedule =
  `Assoc [
    "scheduler_schema", `String "abstract-speculate-residual-cycle-scheduler/v1";
    "scheduler_scc_id", `String schedule.scc_id;
    "scheduler_source", `String schedule.source;
    "callgraph_backed_schedule", `Bool schedule.callgraph_backed;
    "nodes", `List (List.map (fun node -> `String node) schedule.nodes);
    "edges", `List (List.map edge_to_json schedule.edges);
    "sccs", `List (List.map scc_to_json schedule.sccs);
    "worklist_order", `List (List.map (fun node -> `String node) schedule.worklist_order);
    "diagnostics", `List (List.map (fun diagnostic -> `String diagnostic) schedule.diagnostics);
  ]
