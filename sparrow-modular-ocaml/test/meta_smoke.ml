let () =
  let code = .<1 + 2>. in
  let got = Runcode.run code in
  if got <> 3 then failwith "MetaOCaml smoke failed";
  print_endline "meta_smoke: PASS"
