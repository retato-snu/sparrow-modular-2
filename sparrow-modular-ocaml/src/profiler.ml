(* No-op profiler wrapper for extracted DUG construction. *)
let start_event (_ : string) = ()
let finish_event (_ : string) = ()
let event (_ : string) f x = f x
