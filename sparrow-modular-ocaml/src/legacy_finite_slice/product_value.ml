type unsupported = Unsupported of string

type t = {
  itv : Interval.t;
  powloc : string list;
  array_blk : unsupported;
  struct_blk : unsupported;
  powproc : string list;
}

let unsupported name = Unsupported name
let of_itv itv = { itv; powloc = []; array_blk = unsupported "array-block-not-in-first-slice"; struct_blk = unsupported "struct-block-not-in-first-slice"; powproc = [] }
let of_int n = of_itv (Interval.of_int n)
let bot = of_itv Interval.bot
let top = of_itv Interval.top
let join a b = { a with itv = Interval.join a.itv b.itv; powloc = List.sort_uniq String.compare (a.powloc @ b.powloc); powproc = List.sort_uniq String.compare (a.powproc @ b.powproc) }
let widen a b = { (join a b) with itv = Interval.widen a.itv b.itv }
let narrow a b = { (join a b) with itv = Interval.narrow a.itv b.itv }
let le a b = Interval.le a.itv b.itv
let equal a b = a = b

let unsupported_to_json (Unsupported s) = `String s
let unsupported_of_json = function `String s -> Unsupported s | _ -> failwith "bad unsupported"

let to_yojson v =
  `Assoc [
    "itv", Interval.to_yojson v.itv;
    "powloc", `List (List.map (fun s -> `String s) v.powloc);
    "array_blk", unsupported_to_json v.array_blk;
    "struct_blk", unsupported_to_json v.struct_blk;
    "powproc", `List (List.map (fun s -> `String s) v.powproc)
  ]

let of_yojson = function
  | `Assoc fields ->
      let get name = List.assoc name fields in
      let strings = function `List xs -> List.map (function `String s -> s | _ -> failwith "bad string") xs | _ -> failwith "bad list" in
      { itv = Interval.of_yojson (get "itv"); powloc = strings (get "powloc"); array_blk = unsupported_of_json (get "array_blk"); struct_blk = unsupported_of_json (get "struct_blk"); powproc = strings (get "powproc") }
  | _ -> failwith "bad value json"

let to_string v = Interval.to_string v.itv
