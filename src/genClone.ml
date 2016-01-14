
(* This file is free software, part of gen. See file "license" for more details. *)

(** {1 Clonable Generators} *)

type 'a gen = unit -> 'a option

class virtual ['a] t = object
  method virtual gen : 'a gen  (** Generator of values tied to this copy *)
  method virtual clone : 'a t  (** Clone the internal state *)
end
(** A generator that can be cloned as many times as required. *)

type 'a clonable = 'a t
(** Alias to {!'a t} *)

(** {2 Prepend method} *)

type 'a prependable = <
  gen : 'a gen;
  clone : 'a prependable;
  prepend : 'a -> unit (** Add value at front position *)
>

(* helper function for {!to_prependable} *)
let rec to_prependable c =
  let g = c#gen in
  let st = ref `Fwd in  (* state: forward *)
  let next () = match !st with
    | `Fwd -> g()
    | `Yield [] -> assert false
    | `Yield [x] -> st := `Fwd; Some x
    | `Yield (x::l) -> st := `Yield l; Some x
  in
  object
    method gen = next
    method clone = to_prependable (c#clone)
    method prepend x =
      st := match !st with
        | `Fwd -> `Yield [x]
        | `Yield l -> `Yield (x::l)
  end

(** {2 Misc} *)

let rec map f c =
  let g = c#gen in
  let next () = match g() with
    | None -> None
    | Some x -> Some (f x)
  in
  object
    method gen = next
    method clone = map f c#clone
  end

(** {2 Basic IO} *)

module IO = struct
  let with_in ?(mode=0o644) ?(flags=[]) filename f =
    let ic = open_in_gen flags mode filename in
    let timestamp = ref 0 in
    (* make a generator at offset [i] *)
    let rec make i : _ clonable =
      let state = ref `Not_started in
      let rec next() =
        match !state with
        | `Not_started ->
            (* initialize by restoring state *)
            seek_in ic i;
            incr timestamp;
            state := `Started !timestamp;
            next()
        | `Started t ->
            (* check whether another iterator was used more recently *)
            if t < !timestamp then failwith "invalidated iterator";
            try Some (input_char ic)
            with End_of_file -> None
      in
      object
        method clone =
          let i = pos_in ic in
          make i
        method gen = next
      end
    in
    try
      let x = f (make 0) in
      close_in_noerr ic;
      x
    with e ->
      close_in_noerr ic;
      raise e
end
