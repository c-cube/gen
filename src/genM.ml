
(* This file is free software, part of gen. See file "license" for more details. *)

(** {1 Monadic Interface} *)

module type MONAD = GenM_intf.MONAD

module Make(M : MONAD) = struct
  module M = M

  let (>>=) = M.(>>=)
  let (>|=) = M.(>|=)

  type +'a t = unit -> 'a option M.t

  let return x =
    let first = ref true in
    fun () ->
      if !first then (
        first := false;
        M.return (Some x)
      ) else M.return None

  let sequence_m g () = match g() with
    | None -> M.return None
    | Some act ->
        act >|= fun x -> Some x

  let map f g () =
    g() >|= function
    | None -> None
    | Some x -> Some (f x)

  let flat_map f g =
    let rec next f g () =
      g() >>= function
      | None -> M.return None (* done *)
      | Some x ->
          let cur = f x in
          map_from f g cur ()
    and map_from f g cur () =
      let res = cur() in
      res >>= function
      | None -> next f g ()
      | Some _ -> res
    in
    next f g

  let rec fold f acc g =
    g() >>= function
    | None -> M.return acc
    | Some x ->
        let acc = f acc x in
        fold f acc g

  let rec fold_m f acc g =
    g() >>= function
    | None -> M.return acc
    | Some x ->
        f acc x >>= fun acc -> fold_m f acc g

  let rec iter f g =
    g() >>= function
    | None -> M.return ()
    | Some x -> f x; iter f g

  let rec iter_s f g =
    g() >>= function
    | None -> M.return ()
    | Some x -> f x >>= fun () -> iter_s f g

  let rec iter_p f g =
    g() >>= function
    | None -> M.return ()
    | Some x ->
        let _ = f x in
        iter_p f g

  module Infix = struct
    let (>|=) x f = map f x
    let (>>=) x f = flat_map f x
  end

  include Infix
end
