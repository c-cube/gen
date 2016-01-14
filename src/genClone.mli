
(* This file is free software, part of gen. See file "license" for more details. *)

(** {1 Clonable Generators}

    Utils to save the internal state of a generator, and restart from this
    state. This will and should not work on {i any} iterator, but for
    some of them (e.g. reading from a file, see {!IO}) it makes a lot of sense.

    @since 0.2.3 *)

type 'a gen = unit -> 'a option

type 'a t = <
  gen : 'a gen;  (** Generator of values tied to this copy *)
  clone : 'a t;  (** Clone the internal state *)
>
(** A generator that can be cloned as many times as required. *)

type 'a clonable = 'a t
(** Alias to {!'a t} *)

(** {2 Prepend method} *)

type 'a prependable = <
  gen : 'a gen;
  clone : 'a prependable;
  prepend : 'a -> unit (** Add value at front position *)
>

val to_prependable : 'a t -> 'a prependable

(** {2 Misc} *)

val map : ('a -> 'b) -> 'a t -> 'b t

(** {2 Low-level Persistency}

    Example:
    {[
      let g = 1 -- 1000 ;;
      val g : int t = <fun>

      let c = g |> MList.of_gen_lazy |> MList.to_clonable;;
      val c : int clonable = <obj>

                c#next |> take 500 |> to_list;;
      - : int list = [1; 2; 3; .....; 500]

      let c' = c#clone ;;
      val c' : int clonable = <obj>

                 c |> to_list;;
      - : int list = [501; 502; ....; 1000]

        c'#gen |> to_list;;   (* c consumed, but not c' *)
      - : int list = [501; 502; ....; 1000]

        c#gen |> to_list;;
      - : int list = []
    ]}*)

(** {2 IO} *)

module IO : sig
  val with_in : ?mode:int -> ?flags:open_flag list ->
    string ->
    (char t -> 'a) -> 'a
  (** [read filename f] opens [filename] and calls [f g],
      where [g] is a clonable generator of characters from the file.
      It can be cloned by calling [g#save] (which saves the position
      in the file), and used with [g#next]. Distinct clones of [g] shouldn't
      be used at the same time (otherwise [Failure _] will be raised).
      Both the generator and save points are only valid within
      the scope in which [f] is called. *)

end
