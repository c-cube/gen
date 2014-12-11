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

(** {1 Efficient Mutable Lists}

Unrolled lists, append-only, used for storing the content of a generator.

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
]}

@since NEXT_RELEASE *)

type 'a gen = unit -> 'a option
type 'a clonable = <
  gen : 'a gen;  (** Generator of values tied to this copy *)
  clone : 'a clonable;  (** Clone the internal state *)
>

type 'a t
(** An internal append-only storage of elements of type 'a, produced from
    a generator *)

val of_gen : 'a gen -> 'a t
(** [of_gen g] consumes [g] to build a mlist *)

val of_gen_lazy : 'a gen -> 'a t
(** [of_gen_lazy g] makes a mlist that will read from [g] as required,
    until [g] is exhausted. Do not use [g] directly after this, or
    some elements will be absent from the mlist! *)

val to_gen : 'a t -> 'a gen
(** Iterate on the mlist. This function can be called many times without
    any problem, the mlist isn't consumable! *)

val to_clonable : 'a t -> 'a clonable
