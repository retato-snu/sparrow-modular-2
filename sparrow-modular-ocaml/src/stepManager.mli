val step : bool -> string -> 'a -> ('a -> 'b) -> 'b
val stepf : bool -> string -> ('a -> 'b) -> 'a -> 'b
val stepf_opt : bool -> bool -> string -> ('a -> 'a) -> 'a -> 'a
val stepf_cond : bool -> bool -> string -> ('a -> 'b) -> ('a -> 'b) -> 'a -> 'b
val stepf_switch : bool -> string -> (bool * ('a -> 'b)) list -> 'a -> 'b
val stepf_opt_unit : bool -> bool -> string -> ('a -> unit) -> 'a -> unit
