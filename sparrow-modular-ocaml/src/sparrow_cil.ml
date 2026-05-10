include GoblintCil.Cil
module Cil = GoblintCil.Cil
module Errormsg = GoblintCil.Errormsg
module Alpha = GoblintCil.Alpha
module Pretty = GoblintCil.Pretty
module Inthash = GoblintCil.Inthash
module Util = GoblintCil.Util
module Feature = GoblintCil.Feature
module Cfg = GoblintCil.Cfg
module Escape = GoblintCil.Escape
module Cilutil = GoblintCil.Cilutil
module Frontc = GoblintCil.Frontc
module Rmtmps = struct
  let removeUnusedTemps = GoblintCil.RmUnused.removeUnused
end
module Mergecil = GoblintCil.Mergecil
module Stats = GoblintCil.Stats
module Partial = Makecfg.MakeCFG

module Cilint = struct
  include GoblintCil.Cilint
  let to_int = int_of_cilint
  let is_int i = is_int_cilint i
  let is_int2 i n = (compare_cilint i (cilint_of_int n)) = 0
  let compare = compare_cilint
  let plus = add_cilint
  let minus = sub_cilint
  let times = mul_cilint
  let div = div_cilint
  let rem = mod_cilint
  let zero = zero_cilint
  let one = one_cilint
  let mone = mone_cilint
end
