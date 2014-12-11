(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Clonable Generators}

Utils to save the internal state of a generator, and restart from this
state. This will and should not work on {i any} iterator, but for
some of them (e.g. reading from a file, see {!IO}) it makes a lot of sense.

@since NEXT_RELEASE *)

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
