type env = (string * Product_value.t) list

type shape = Loop | If | Expr

type executable = {
  id : string;
  shape : shape;
  approx : Product_value.t;
  source : string;
  artifact : string;
  code : (env -> Product_value.t) Trx.code;
}

type ps = S of Product_value.t | D of executable

let shape_to_string = function Loop -> "loop" | If -> "if" | Expr -> "expr"
let shape_of_string = function "loop" -> Loop | "if" -> If | "expr" -> Expr | s -> failwith ("bad residual shape: " ^ s)

let value_of_int = Product_value.of_int
let lookup name env = try List.assoc name env with Not_found -> failwith ("missing dynamic input: " ^ name)
let itv_of_value v = v.Product_value.itv
let interval_of_int = Interval.of_int
let interval_add_const = Interval.add_const
let interval_join = Interval.join
let interval_widen = Interval.widen
let product_of_itv = Product_value.of_itv
let product_bot = Product_value.bot
let product_join = Product_value.join
let product_of_int = Product_value.of_int
let is_bot = function Interval.Bot -> true | _ -> false

let guard_i_lt_n i n =
  match n with
  | Interval.Bot -> Interval.Bot
  | Interval.Itv (_, upper) ->
      Interval.meet i (Interval.Itv (Interval.Neg_inf, Interval.add_const_bound upper (-1)))

let guard_i_ge_n i n =
  match n with
  | Interval.Bot -> Interval.Bot
  | Interval.Itv (lower, _) -> Interval.meet i (Interval.Itv (lower, Interval.Pos_inf))

let finite_loop_budget = function
  | Interval.Itv (Interval.Finite lo, Interval.Finite hi) when hi >= lo -> Some (hi - lo + 2)
  | _ -> None

let then_reachable n =
  not (is_bot (Interval.meet n (Interval.Itv (Interval.Finite 1, Interval.Pos_inf))))

let else_reachable n =
  not (is_bot (Interval.meet n (Interval.Itv (Interval.Neg_inf, Interval.Finite 0))))

let code_for_shape = function
  | Loop ->
      .<fun env ->
        let n_value = lookup "n" env in
        let n = itv_of_value n_value in
        let init = interval_of_int 0 in
        let budget = finite_loop_budget n in
        let rec iterate step header =
          let body_out = interval_add_const (guard_i_lt_n header n) in
          let joined = interval_join init body_out in
          let next =
            match budget with
            | Some remaining when step <= remaining -> joined
            | _ -> interval_widen header joined
          in
          if next = header then header else iterate (step + 1) next
        in
        product_of_itv (guard_i_ge_n (iterate 0 init) n)>.
  | If ->
      .<fun env ->
        let n_value = lookup "n" env in
        let n = itv_of_value n_value in
        if is_bot n then product_bot
        else
          let then_path = then_reachable n in
          let else_path = else_reachable n in
          if then_path && not else_path then product_of_int 1
          else if else_path && not then_path then product_of_int 0
          else product_join (product_of_int 1) (product_of_int 0)>.
  | Expr -> .<fun env -> lookup "n" env>.

let source_for_shape = function
  | Loop -> "let residual env =\n  let n_value = Runtime.lookup \"n\" env in\n  let n = n_value.Product_value.itv in\n  let rec iterate step header =\n    let body_out = Runtime.guard_i_lt_n header n |> Interval.add_const in\n    let joined = Interval.join (Interval.of_int 0) body_out in\n    let next = match Runtime.finite_loop_budget n with Some remaining when step <= remaining -> joined | _ -> Interval.widen header joined in\n    if next = header then header else iterate (step + 1) next\n  in\n  Product_value.of_itv (Runtime.guard_i_ge_n (iterate 0 (Interval.of_int 0)) n)\n"
  | If -> "let residual env =\n  let n_value = Runtime.lookup \"n\" env in\n  match n_value.Product_value.itv with\n  | Interval.Bot -> Product_value.bot\n  | n ->\n      let then_reachable = Interval.meet n (Interval.Itv (Interval.Finite 1, Interval.Pos_inf)) <> Interval.Bot in\n      let else_reachable = Interval.meet n (Interval.Itv (Interval.Neg_inf, Interval.Finite 0)) <> Interval.Bot in\n      if then_reachable && not else_reachable then Product_value.of_int 1\n      else if else_reachable && not then_reachable then Product_value.of_int 0\n      else Product_value.join (Product_value.of_int 1) (Product_value.of_int 0)\n"
  | Expr -> "let residual env = Runtime.lookup \"n\" env\n"

let make_with_source ~id ~shape ~artifact ~approx ~source =
  { id; shape; approx; artifact; source; code = code_for_shape shape }

let make ~id ~shape ~artifact ~approx =
  make_with_source ~id ~shape ~artifact ~approx ~source:(source_for_shape shape)

let execute d env = (Runcode.run d.code) env

let ps_value = function S v -> v | D d -> d.approx
let is_dynamic = function D _ -> true | S _ -> false
