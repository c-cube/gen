let write_file f s =
  let out = open_out f in
  output_string out s; flush out; close_out out

let shims_pre_407 = "module Stdlib = Pervasives"

let shims_post_407 = "module Stdlib = Stdlib"

let () =
  let major, minor = Scanf.sscanf Sys.ocaml_version "%u.%u" (fun maj min -> maj, min) in
  write_file "GenShims_.ml" (if (major, minor) >= (4,7) then shims_post_407 else shims_pre_407);
