let () =
  Gen_testlib.run_all ~descr:"gen"
    [ T_gen.get () ]
