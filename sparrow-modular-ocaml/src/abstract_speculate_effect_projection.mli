val observe_return :
  Abstract_speculate_effect_algebra.t ->
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result

val observe_global :
  Abstract_speculate_effect_algebra.t ->
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result

val observe_pointer :
  Abstract_speculate_effect_algebra.t ->
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result

val observe_taint :
  Abstract_speculate_effect_algebra.t ->
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result

val observe_product_pair :
  Abstract_speculate_effect_algebra.t ->
  Abstract_speculate_effect_algebra.projection Abstract_speculate_effect_algebra.operation_result

val evidence_paths : Abstract_speculate_effect_algebra.projection -> string list
