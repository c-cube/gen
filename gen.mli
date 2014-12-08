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

(** {1 Generators}

Values of type ['a Gen.t] represent a possibly infinite sequence of values
of type 'a. One can only iterate once on the sequence, as it is consumed
by iteration/deconstruction/access. [None] is returned when the generator
is exhausted.

The submodule {!Restart} provides utilities to work with
{b restartable generators}, that is, functions [unit -> 'a Gen.t] that
allow to build as many generators from the same source as needed.
*)

(** {2 Global type declarations} *)

type 'a t = unit -> 'a option
  (** A generator may be called several times, yielding the next value
      each time. It returns [None] when no elements remain *)

type 'a gen = 'a t

module type S = Gen_intf.S

(** {2 Transient generators} *)

val get : 'a t -> 'a option
  (** Get the next value *)

val next : 'a t -> 'a option
  (** Synonym for {!get} *)

val get_exn : 'a t -> 'a
  (** Get the next value, or fails
      @raise Invalid_argument if no element remains *)

val junk : 'a t -> unit
  (** Drop the next value, discarding it. *)

val repeatedly : (unit -> 'a) -> 'a t
  (** Call the same function an infinite number of times (useful for instance
      if the function is a random generator). *)

include S with type 'a t := 'a gen
  (** Operations on {b transient} generators *)

(** {2 Restartable generators} *)

module Restart : sig
  type 'a t = unit -> 'a gen

  type 'a restartable = 'a t

  include S with type 'a t := 'a restartable

  val cycle : 'a t -> 'a t
    (** Cycle through the enum, endlessly. The enum must not be empty. *)

  val lift : ('a gen -> 'b) -> 'a t -> 'b

  val lift2 : ('a gen -> 'b gen -> 'c) -> 'a t -> 'b t -> 'c
end

(** {2 Utils} *)

val persistent : 'a t -> 'a Restart.t
  (** Store content of the transient generator in memory, to be able to iterate
      on it several times later. If possible, consider using combinators
      from {!Restart} directly instead. *)

val persistent_lazy : 'a t -> 'a Restart.t
  (** Same as {!persistent}, but consumes the generator on demand (by chunks).
      This allows to make a restartable generator out of an ephemeral one,
      without paying a big cost upfront (nor even consuming it fully).
      @since 0.2.2 *)

val start : 'a Restart.t -> 'a t
  (** Create a new transient generator.
      [start gen] is the same as [gen ()] but is included for readability. *)

(** {2 Save/Restore}

Utils to save the internal state of a generator, and restart from this
state. This will and should not work on {i any} iterator, but for
some of them (e.g. reading from a file, see {!IO}) it makes a lot of sense.

Once restore is called from a state, all previous iterators that share this
state are invalidated and must not be used any more.

@since NEXT_RELEASE *)

type 'a clonable = <
  next : unit -> 'a option; (** Use as a generator *)
  clone : 'a clonable;  (** Clone into a distinct clonable gen *)
>

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

c' |> to_list;;   (* c consumed, but not c' *)
- : int list = [501; 502; ....; 1000]

c |> to_list;;
- : int list = []
]}*)

module MList : sig
  type 'a t
  (** An internal append-only storage of elements of type 'a, produced from
      a generator *)

  val of_gen : 'a gen -> 'a t
  (** [of_gen g] consumes [g] to build a mlist *)

  val of_gen_lazy : 'a gen -> 'a t
  (** [of_gen_lazy g] makes a mlist that will read from [g] as required,
      until [g] is exhausted. Do not use [g] directly after this, or
      some elements will be absent from the mlist! *)

  val to_gen : 'a t -> 'a Restart.t
  (** Iterate on the mlist. This function can be called many times without
      any problem, the mlist isn't consumable! *)

  val to_clonable : 'a t -> 'a clonable
  (** [to_gen_save l] returns a pair [g, save] such taht [save] allows to
      save the current state of [g] so that it can be restored later. *)
end

(** {2 Basic IO}

Very basic interface to manipulate files as sequence of chunks/lines.
Iterators are not duplicable, but save/restore can be used for input.
@since NEXT_RELEASE *)

module IO : sig
  val with_in : ?mode:int -> ?flags:open_flag list ->
                string ->
                (char clonable -> 'a) -> 'a
  (** [read filename f] opens [filename] and calls [f g],
      where [g] is a clonable generator of characters from the file.
      It can be cloned by calling [g#save] (which saves the position
      in the file), and used with [g#next]. Distinct clones of [g] shouldn't
      be used at the same time (otherwise [Failure _] will be raised).
      Both the generator and save points are only valid within
      the scope in which [f] is called. *)

  val write_str : ?mode:int -> ?flags:open_flag list ->  ?sep:string ->
                 string -> string t -> unit
  (** [write_to filename g] writes all strings from [g] into the given
      file. It takes care of opening and closing the file.
      @param mode default [0o644]
      @param flags used by [open_out_gen]. Default: [[Open_creat;Open_wronly]].
      @param sep separator between each string (e.g. newline) *)

  val write : ?mode:int -> ?flags:open_flag list ->
              string -> char t -> unit
  (** Same as {!write_str} but with individual characters *)
end
