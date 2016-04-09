
(* This file is free software, part of gen. See file "license" for more details. *)

(** {1 Monadic Interface}

    {b status: experimental}

    @since 0.4 *)

module type MONAD = GenM_intf.MONAD

module Make(M : MONAD) : GenM_intf.S with module M = M
