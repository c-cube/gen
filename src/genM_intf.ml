
(* This file is free software, part of gen. See file "license" for more details. *)

type 'a gen = unit -> 'a option

module type MONAD = sig
  type +'a t

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  module M : MONAD

  type +'a t = unit -> 'a option M.t
  (** A value of type ['a t] is an iterator over values of type ['a]
      that live in the monad [M.t]. For instance, if [M] is [Lwt], accessing
      each element might require some IO operation (reading a file, etc.) *)

  val return : 'a -> 'a t

  val sequence_m : 'a M.t gen -> 'a t
  (** From a generator of actions, return an effectful generator *)

  val map : ('a -> 'b) -> 'a t -> 'b t

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t

  val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a M.t

  val fold_m : ('a -> 'b -> 'a M.t) -> 'a -> 'b t -> 'a M.t

  val iter : ('a -> unit) -> 'a t -> unit M.t

  val iter_s : ('a -> unit M.t) -> 'a t -> unit M.t

  val iter_p : ('a -> unit M.t) -> 'a t -> unit M.t

  module Infix : sig
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  end

  include module type of Infix
end
