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

(** {1 Efficient Mutable Lists} *)

type 'a gen = unit -> 'a option
type 'a clonable = <
  gen : 'a gen;  (** Generator of values tied to this copy *)
  clone : 'a clonable;  (** Clone the internal state *)
>

type 'a node =
  | Nil
  | Cons of 'a array * int ref * 'a node ref
  | Suspend of 'a gen

type 'a t = {
  start : 'a node ref; (* first node. *)
  mutable chunk_size : int;
  max_chunk_size : int;
}

let _make ~max_chunk_size gen = {
  start = ref (Suspend gen);
  chunk_size = 8;
  max_chunk_size;
}

(* increment the size of chunks *)
let _incr_chunk_size mlist =
  if mlist.chunk_size < mlist.max_chunk_size
    then mlist.chunk_size <- 2 * mlist.chunk_size

(* read one chunk of input; return the corresponding node.
  will potentially change [mlist.chunk_size]. *)
let _read_chunk mlist gen =
  match gen() with
  | None -> Nil  (* done *)
  | Some x ->
    (* new list node *)
      let r = ref 1 in
      let a = Array.make mlist.chunk_size x in
      let tail = ref (Suspend gen) in
      let stop = ref false in
      let node = Cons (a, r, tail) in
      (* read the rest of the chunk *)
      while not !stop && !r < mlist.chunk_size do
        match gen() with
        | None ->
            tail := Nil;
            stop := true
        | Some x ->
            a.(!r) <- x;
            incr r;
      done;
      _incr_chunk_size mlist;
      node

(* eager construction *)
let of_gen gen =
  let mlist = _make ~max_chunk_size:4096 gen in
  let rec _fill prev = match _read_chunk mlist gen with
    | Nil -> prev := Nil
    | Suspend _ -> assert false
    | Cons (_, _, prev') as node ->
        prev := node;
        _fill prev'
  in
  _fill mlist.start;
  mlist

(* lazy construction *)
let of_gen_lazy gen =
  let mlist = _make ~max_chunk_size:2048 gen in
  mlist

let to_gen l =
  let cur = ref l.start in
  let i = ref 0 in
  let rec next() = match ! !cur with
  | Nil -> None
  | Cons (a,n,l') ->
      if !i = !n
      then begin
        cur := l';
        i := 0;
        next()
      end else begin
        let y = a.(!i) in
        incr i;
        Some y
      end
  | Suspend gen ->
      let node = _read_chunk l gen in
      !cur := node;
      next()
  in
  next

let to_clonable l : 'a clonable =
  let rec make node i =
    let cur = ref node and i = ref i in
    let rec next() = match ! !cur with
      | Nil -> None
      | Cons (a,n,l') ->
          if !i = !n
          then begin
            cur := l';
            i := 0;
            next()
          end else begin
            let y = a.(!i) in
            i := !i+1;
            Some y
          end
      | Suspend gen ->
          let node = _read_chunk l gen in
          (!cur) := node;
          next()
    in
    object
      method gen = next
      method clone = make !cur !i
    end
  in
  make l.start 0


