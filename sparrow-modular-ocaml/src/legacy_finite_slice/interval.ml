type bound = Neg_inf | Finite of int | Pos_inf

type t = Bot | Itv of bound * bound

let bot = Bot
let top = Itv (Neg_inf, Pos_inf)
let of_int n = Itv (Finite n, Finite n)
let of_bounds lo hi = Itv (Finite lo, Finite hi)

let min_bound a b =
  match a, b with
  | Neg_inf, _ | _, Neg_inf -> Neg_inf
  | Pos_inf, x | x, Pos_inf -> x
  | Finite x, Finite y -> Finite (min x y)

let max_bound a b =
  match a, b with
  | Pos_inf, _ | _, Pos_inf -> Pos_inf
  | Neg_inf, x | x, Neg_inf -> x
  | Finite x, Finite y -> Finite (max x y)

let compare_bound a b =
  match a, b with
  | Neg_inf, Neg_inf | Pos_inf, Pos_inf -> 0
  | Neg_inf, _ | _, Pos_inf -> -1
  | Pos_inf, _ | _, Neg_inf -> 1
  | Finite x, Finite y -> compare x y

let add_const_bound b n =
  match b with
  | Neg_inf -> Neg_inf
  | Pos_inf -> Pos_inf
  | Finite x -> Finite (x + n)

let add_const = function
  | Bot -> Bot
  | Itv (lo, hi) -> Itv (add_const_bound lo 1, add_const_bound hi 1)

let meet x y =
  match x, y with
  | Bot, _ | _, Bot -> Bot
  | Itv (l1, u1), Itv (l2, u2) ->
      let lo = max_bound l1 l2 in
      let hi = min_bound u1 u2 in
      if compare_bound lo hi > 0 then Bot else Itv (lo, hi)

let join x y =
  match x, y with
  | Bot, z | z, Bot -> z
  | Itv (l1, u1), Itv (l2, u2) -> Itv (min_bound l1 l2, max_bound u1 u2)

let widen old_v new_v =
  match old_v, new_v with
  | Bot, z -> z
  | z, Bot -> z
  | Itv (l1, u1), Itv (l2, u2) ->
      let lo = if l2 = l1 then l1 else if min_bound l2 l1 = l2 then Neg_inf else l1 in
      let hi = if u2 = u1 then u1 else if max_bound u2 u1 = u2 then Pos_inf else u1 in
      Itv (lo, hi)

let narrow old_v new_v =
  match old_v, new_v with
  | Itv (l1, u1), Itv (l2, u2) ->
      let lo = match l1 with Neg_inf -> l2 | _ -> l1 in
      let hi = match u1 with Pos_inf -> u2 | _ -> u1 in
      Itv (lo, hi)
  | _, z -> z

let le x y =
  match x, y with
  | Bot, _ -> true
  | _, Bot -> x = Bot
  | Itv (l1, u1), Itv (l2, u2) ->
      min_bound l1 l2 = l2 && max_bound u1 u2 = u2

let to_yojson = function
  | Bot -> `Assoc ["kind", `String "bot"]
  | Itv (lo, hi) ->
      let b = function Neg_inf -> `String "-inf" | Pos_inf -> `String "+inf" | Finite n -> `Int n in
      `Assoc ["kind", `String "itv"; "lo", b lo; "hi", b hi]

let of_yojson = function
  | `Assoc fields ->
      let get name = List.assoc name fields in
      begin match get "kind" with
      | `String "bot" -> Bot
      | `String "itv" ->
          let b = function `String "-inf" -> Neg_inf | `String "+inf" -> Pos_inf | `Int n -> Finite n | _ -> failwith "bad bound" in
          Itv (b (get "lo"), b (get "hi"))
      | _ -> failwith "bad interval kind"
      end
  | _ -> failwith "bad interval json"

let singleton = function Itv (Finite n, Finite m) when n = m -> Some n | _ -> None

let to_string = function
  | Bot -> "⊥"
  | Itv (lo, hi) ->
      let b = function Neg_inf -> "-∞" | Pos_inf -> "+∞" | Finite n -> string_of_int n in
      "[" ^ b lo ^ "," ^ b hi ^ "]"
