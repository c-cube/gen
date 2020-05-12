module C = Configurator.V1

let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

let shims_pre_407 = "module Stdlib = Pervasives"

let shims_post_407 = "module Stdlib = Stdlib"

let () =
  C.main ~name:"mkshims" (fun c ->
    let version = C.ocaml_config_var_exn c "version" in
    let major, minor = Scanf.sscanf version "%u.%u" (fun maj min -> maj, min) in
    write_file "GenShims_.ml" (if (major, minor) >= (4,7) then shims_post_407 else shims_pre_407);
    )
