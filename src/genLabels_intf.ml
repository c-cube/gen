
(* This file is free software, part of gen. See file "license" for more details. *)

(** {1 Common signature for transient and restartable generators}

    The signature {!S} abstracts on a type ['a t], where the [t] can be
    the type of transient or restartable generators. Some functions specify
    explicitely that they use ['a gen] (transient generators). *)

type 'a gen = unit -> 'a option

module type S = sig
  type 'a t

  val empty : 'a t
  (** Empty generator, with no elements *)

  val singleton : 'a -> 'a t
  (** One-element generator *)

  val return : 'a -> 'a t
  (** Alias to {!singleton}
      @since 0.3 *)

  val repeat : 'a -> 'a t
  (** Repeat same element endlessly *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
  (** [iterate x f] is [[x; f x; f (f x); f (f (f x)); ...]] *)

  val unfold : ('b -> ('a * 'b) option) -> 'b -> 'a t
  (** Dual of {!fold}, with a deconstructing operation. It keeps on
      unfolding the ['b] value into a new ['b], and a ['a] which is yielded,
      until [None] is returned. *)

  val init : ?limit:int -> (int -> 'a) -> 'a t
  (** Calls the function, starting from 0, on increasing indices.
      If [limit] is provided and is a positive int, iteration will
      stop at the limit (excluded).
      For instance [init ~limit:4 id] will yield 0, 1, 2, and 3. *)

  (** {2 Basic combinators}

      {b Note}: those combinators, applied to generators (not restartable
      generators) {i consume} their argument. Sometimes they consume it lazily,
      sometimes eagerly, but in any case once [f gen] has been called (with [f] a
      combinator), [gen] shouldn't be used anymore. *)

  val is_empty : _ t -> bool
  (** Check whether the gen is empty. Pops an element, if any *)

  val fold : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b
  (** Fold on the generator, tail-recursively. Consumes the generator. *)

  val reduce : f:('a -> 'a -> 'a) -> 'a t -> 'a
  (** Fold on non-empty sequences. Consumes the generator.
      @raise Invalid_argument on an empty gen *)

  val scan : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b t
  (** Like {!fold}, but keeping successive values of the accumulator.
      Consumes the generator. *)

  val unfold_scan : ('b -> 'a -> 'b * 'c) -> 'b -> 'a t -> 'c t
  (** A mix of {!unfold} and {!scan}. The current state is combined with
      the current element to produce a new state, and an output value
      of type 'c.
      @since 0.2.2 *)

  val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate on the gen, consumes it. *)

  val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
  (** Iterate on elements with their index in the gen, from 0, consuming it. *)

  val length : _ t -> int
  (** Length of an gen (linear time), consuming it *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** Lazy map. No iteration is performed now, the function will be called
      when the result is traversed. *)

  val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
  (** Lazy map with indexing starting from 0. No iteration is performed now,
      the function will be called when the result is traversed.
      @since 0.5 *)

  val fold_map : f:('b -> 'a -> 'b) -> init:'b -> 'a t -> 'b t
  (** Lazy fold and map. No iteration is performed now, the function will be
      called when the result is traversed. The result is
      an iterator over the successive states of the fold.
      @since 0.2.4 *)

  val append : 'a t -> 'a t -> 'a t
  (** Append the two gens; the result contains the elements of the first,
      then the elements of the second gen. *)

  val flatten : 'a gen t -> 'a t
  (** Flatten the generator of generators *)

  val flat_map : f:('a -> 'b gen) -> 'a t -> 'b t
  (** Monadic bind; each element is transformed to a sub-gen
      which is then iterated on, before the next element is processed,
      and so on. *)

  val mem : ?eq:('a -> 'a -> bool) -> x:'a -> 'a t -> bool
  (** Is the given element, member of the gen? *)

  val take : int -> 'a t -> 'a t
  (** Take at most n elements *)

  val drop : int -> 'a t -> 'a t
  (** Drop n elements *)

  val nth : int -> 'a t -> 'a
  (** n-th element, or Not_found
      @raise Not_found if the generator contains less than [n] arguments *)

  val take_nth : int -> 'a t -> 'a t
  (** [take_nth n g] returns every element of [g] whose index
      is a multiple of [n]. For instance [take_nth 2 (1--10) |> to_list]
      will return [1;3;5;7;9] *)

  val filter : f:('a -> bool) -> 'a t -> 'a t
  (** Filter out elements that do not satisfy the predicate.  *)

  val take_while : f:('a -> bool) -> 'a t -> 'a t
  (** Take elements while they satisfy the predicate. The initial generator
      itself is not to be used anymore after this. *)

  val fold_while : f:('a -> 'b -> 'a * [`Stop | `Continue]) -> init:'a -> 'b t -> 'a
  (** Fold elements until (['a, `Stop]) is indicated by the accumulator.
      @since 0.2.4 *)

  val drop_while : f:('a -> bool) -> 'a t -> 'a t
  (** Drop elements while they satisfy the predicate. The initial generator
      itself should not be used anymore, only the result of [drop_while]. *)

  val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
  (** Maps some elements to 'b, drop the other ones *)

  val zip_index : 'a t -> (int * 'a) t
  (** Zip elements with their index in the gen *)

  val unzip : ('a * 'b) t -> 'a t * 'b t
  (** Unzip into two sequences, splitting each pair *)

  val partition : f:('a -> bool) -> 'a t -> 'a t * 'a t
  (** [partition p l] returns the elements that satisfy [p],
      and the elements that do not satisfy [p] *)

  val for_all : f:('a -> bool) -> 'a t -> bool
  (** Is the predicate true for all elements? *)

  val exists : f:('a -> bool) -> 'a t -> bool
  (** Is the predicate true for at least one element? *)

  val min : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
  (** Minimum element, according to the given comparison function.
      @raise Invalid_argument if the generator is empty *)

  val max : ?lt:('a -> 'a -> bool) -> 'a t -> 'a
  (** Maximum element, see {!min}
      @raise Invalid_argument if the generator is empty *)

  val eq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Equality of generators. *)

  val lexico : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Lexicographic comparison of generators. If a generator is a prefix
      of the other one, it is considered smaller. *)

  val compare : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Synonym for {! lexico} *)

  val find : f:('a -> bool) -> 'a t -> 'a option
  (** [find p e] returns the first element of [e] to satisfy [p],
      or None. *)

  val sum : int t -> int
  (** Sum of all elements *)

  (** {2 Multiple iterators} *)

  val map2 : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Map on the two sequences. Stops once one of them is exhausted.*)

  val iter2 : f:('a -> 'b -> unit) -> 'a t -> 'b t -> unit
  (** Iterate on the two sequences. Stops once one of them is exhausted.*)

  val fold2 : f:('acc -> 'a -> 'b -> 'acc) -> init:'acc -> 'a t -> 'b t -> 'acc
  (** Fold the common prefix of the two iterators *)

  val for_all2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Succeeds if all pairs of elements satisfy the predicate.
      Ignores elements of an iterator if the other runs dry. *)

  val exists2 : f:('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** Succeeds if some pair of elements satisfy the predicate.
      Ignores elements of an iterator if the other runs dry. *)

  val zip_with : f:('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Combine common part of the gens (stops when one is exhausted) *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
  (** Zip together the common part of the gens *)

  (** {2 Complex combinators} *)

  val merge : 'a gen t -> 'a t
  (** Pick elements fairly in each sub-generator. The merge of gens
      [e1, e2, ... ] picks elements in [e1], [e2],
      in [e3], [e1], [e2] .... Once a generator is empty, it is skipped;
      when they are all empty, and none remains in the input,
      their merge is also empty.
      For instance, [merge [1;3;5] [2;4;6]] will be, in disorder, [1;2;3;4;5;6]. *)

  val intersection : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  (** Intersection of two sorted sequences. Only elements that occur in both
      inputs appear in the output *)

  val sorted_merge : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t
  (** Merge two sorted sequences into a sorted sequence *)

  val sorted_merge_n : ?cmp:('a -> 'a -> int) -> 'a t list -> 'a t
  (** Sorted merge of multiple sorted sequences *)

  val tee : ?n:int -> 'a t -> 'a gen list
  (** Duplicate the gen into [n] generators (default 2). The generators
      share the same underlying instance of the gen, so the optimal case is
      when they are consumed evenly *)

  val round_robin : ?n:int -> 'a t -> 'a gen list
  (** Split the gen into [n] generators in a fair way. Elements with
      [index = k mod n] with go to the k-th gen. [n] default value
      is 2. *)

  val interleave : 'a t -> 'a t -> 'a t
  (** [interleave a b] yields an element of [a], then an element of [b],
      and so on. When a generator is exhausted, this behaves like the
      other generator. *)

  val intersperse : 'a -> 'a t -> 'a t
  (** Put the separator element between all elements of the given gen *)

  val product : 'a t -> 'b t -> ('a * 'b) t
  (** Cartesian product, in no predictable order. Works even if some of the
      arguments are infinite. *)

  val group : ?eq:('a -> 'a -> bool) -> 'a t -> 'a list t
  (** Group equal consecutive elements together. *)

  val uniq : ?eq:('a -> 'a -> bool) -> 'a t -> 'a t
  (** Remove consecutive duplicate elements. Basically this is
      like [fun e -> map List.hd (group e)]. *)

  val sort : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort according to the given comparison function. The gen must be finite. *)

  val sort_uniq : ?cmp:('a -> 'a -> int) -> 'a t -> 'a t
  (** Sort and remove duplicates. The gen must be finite. *)

  val chunks : int -> 'a t -> 'a array t
  (** [chunks n e] returns a generator of arrays of length [n], composed
      of successive elements of [e]. The last array may be smaller
      than [n] *)

  val permutations : 'a t -> 'a list t
  (** Permutations of the gen.
      @since 0.2.2 *)

  val permutations_heap : 'a t -> 'a array t
  (** Permutations of the gen, using Heap's algorithm.
      @since 0.2.3 *)

  val combinations : int -> 'a t -> 'a list t
  (** Combinations of given length. The ordering of the elements within
      each combination is unspecified.
      Example (ignoring ordering):
        [combinations 2 (1--3) |> to_list = [[1;2]; [1;3]; [2;3]]]
      @since 0.2.2 *)

  val power_set : 'a t -> 'a list t
  (** All subsets of the gen (in no particular order). The ordering of
      the elements within each subset is unspecified.
      @since 0.2.2 *)

  (** {2 Basic conversion functions} *)

  val of_list : 'a list -> 'a t
  (** Enumerate elements of the list *)

  val to_list : 'a t -> 'a list
  (** non tail-call trasnformation to list, in the same order *)

  val to_rev_list : 'a t -> 'a list
  (** Tail call conversion to list, in reverse order (more efficient) *)

  val to_array : 'a t -> 'a array
  (** Convert the gen to an array (not very efficient) *)

  val of_array : ?start:int -> ?len:int -> 'a array -> 'a t
  (** Iterate on (a slice of) the given array *)

  val of_string : ?start:int -> ?len:int -> string -> char t
  (** Iterate on bytes of the string *)

  val to_string : char t -> string
  (** Convert into a string *)

  val to_buffer : Buffer.t -> char t -> unit
  (** Consumes the iterator and writes to the buffer *)

  val rand_int : int -> int t
  (** Random ints in the given range. *)

  val int_range : ?step:int -> int -> int -> int t
  (** [int_range ~step a b] generates integers between [a] and [b], included,
      with steps of length [step] (1 if omitted). [a] is assumed to be smaller
      than [b]. [step] must not be null, but it can be negative for decreasing
      integers. *)

  val lines : char t -> string t
  (** Group together chars belonging to the same line
      @since 0.3 *)

  val unlines : string t -> char t
  (** Explode lines into their chars, adding a ['\n'] after each one
      @since 0.3 *)

  module Infix : sig
    val (--) : int -> int -> int t
    (** Synonym for {! int_range ~by:1} *)

    val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
    (** Monadic bind operator *)

    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    (** Infix map operator
        @since 0.2.3 *)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (** Infix map operator
        @since 0.2.3 *)
  end

  val (--) : int -> int -> int t
  (** Synonym for {! int_range ~by:1} *)

  val (>>=) : 'a t -> ('a -> 'b gen) -> 'b t
  (** Monadic bind operator *)

  val (>>|) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix map operator
      @since 0.2.3 *)

  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  (** Infix map operator
      @since 0.2.3 *)

  val pp : ?start:string -> ?stop:string -> ?sep:string -> ?horizontal:bool ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty print the content of the generator on a formatter. *)
end

