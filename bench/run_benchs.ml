
module B = Benchmark

(* benchmark the "persistent" function *)
module Persistent = struct
  let _sum g =
    Gen.Restart.fold (+) 0 g

  module MList = struct
    type 'a t = 'a node option ref
    and 'a node = {
      content : 'a;
      mutable prev : 'a node;
      mutable next : 'a node;
    }

    let create () = ref None

    let is_empty d =
      match !d with
      | None -> true
      | Some _ -> false

    let push_back d x =
      match !d with
      | None ->
        let rec elt = {
          content = x; prev = elt; next = elt; } in
        d := Some elt
      | Some first ->
        let elt = { content = x; next=first; prev=first.prev; } in
        first.prev.next <- elt;
        first.prev <- elt

    (* conversion to gen *)
    let to_gen d =
      fun () ->
        match !d with
        | None -> (fun () -> None)
        | Some first ->
          let cur = ref first in (* current element of the list *)
          let stop = ref false in (* are we done yet? *)
          fun () ->
            if !stop then None
            else begin
              let x = (!cur).content in
              cur := (!cur).next;
              (if !cur == first then stop := true); (* EOG, we made a full cycle *)
              Some x
            end
  end

  (** Store content of the generator in an enum *)
  let persistent_mlist gen =
    let l = MList.create () in
    Gen.iter (MList.push_back l) gen;
    MList.to_gen l

  let bench_mlist n =
    let g = persistent_mlist Gen.(1 -- n) in
    ignore (_sum g)

  (** {6 Unrolled mutable list} *)
  module UnrolledList = struct
  type 'a node =
    | Nil
    | Partial of 'a array * int
    | Cons of 'a array * 'a node ref

  let of_gen gen =
    let start = ref Nil in
    let chunk_size = ref 16 in
    let rec fill prev cur =
      match cur, gen() with
      | Partial (a,n), None ->
          prev := Cons (Array.sub a 0 n, ref Nil); ()  (* done *)
      | _, None -> prev := cur; ()  (* done *)
      | Nil, Some x ->
          let n = !chunk_size in
          if n < 4096 then chunk_size := 2 * !chunk_size;
          fill prev (Partial (Array.make n x, 1))
      | Partial (a, n), Some x ->
          assert (n < Array.length a);
          a.(n) <- x;
          if n+1 = Array.length a
          then begin
            let r = ref Nil in
            prev := Cons(a, r);
            fill r Nil
          end else fill prev (Partial (a, n+1))
      | Cons _, _ -> assert false
    in
    fill start !start ;
    !start

  let to_gen l () =
    let cur = ref l in
    let i = ref 0 in
    let rec next() = match !cur with
    | Nil -> None
    | Cons (a,l') ->
        if !i = Array.length a
        then begin
          cur := !l';
          i := 0;
          next()
        end else begin
          let y = a.(!i) in
          incr i;
          Some y
        end
    | Partial _ -> assert false
    in
    next
  end

  (** Store content of the generator in an enum *)
  let persistent_unrolled gen =
  let l = UnrolledList.of_gen gen in
  UnrolledList.to_gen l

  let bench_unrolled n =
  let g = persistent_unrolled Gen.(1 -- n) in
  ignore (_sum g)

  let bench_naive n =
    let l = Gen.to_rev_list Gen.(1 -- n) in
    let g = Gen.Restart.of_list (List.rev l) in
    ignore (_sum g)

  let bench_current n =
    let g = Gen.persistent Gen.(1 -- n) in
    ignore (_sum g)

  let bench_current_lazy n =
    let g = Gen.persistent_lazy Gen.(1 -- n) in
    ignore (_sum g)

  let bench_current_lazy_no_cache n =
    let g = Gen.persistent_lazy ~max_chunk_size:16 ~caching:false Gen.(1 -- n) in
    ignore (_sum g)

  let () =
    let open B.Tree in
    let bench_n n =
      B.throughputN 2 ~repeat:3
        [ "mlist", bench_mlist, n
        ; "naive", bench_naive, n
        ; "unrolled", bench_unrolled, n
        ; "current", bench_current, n
        ; "current_lazy", bench_current_lazy, n
        ; "current_lazy_no_cache", bench_current_lazy_no_cache, n
        ]
    in
    let app_int f n = string_of_int n @> lazy (f n) in
    let app_ints f l = B.Tree.concat (List.map (app_int f) l) in
    B.Tree.register (
      "persistent" @>>
      app_ints bench_n [100; 1_000; 10_000; 100_000]
    )
end

(* benchmark the "permutation" function *)
module Perm = struct
  module PermState = struct
    type 'a state =
      | Done
      | Base (* bottom machine, yield [] *)
      | Insert of 'a insert_state
    and 'a insert_state = {
      x : 'a;
      mutable l : 'a list;
      mutable n : int; (* idx for insertion *)
      len : int; (* len of [l] *)
      sub : 'a t;
    }
    and 'a t = {
      mutable st : 'a state;
    }
  end

  let permutations_rec g =
    let open PermState in
    (* make a machine for n elements. Invariant: n=len(l) *)
    let rec make_machine n l = match l with
      | [] -> assert (n=0); {st=Base}
      | x :: tail ->
          let sub = make_machine (n-1) tail in
          let st = match next sub () with
            | None -> Done
            | Some l -> Insert {x;n=0;l;len=n;sub}
          in
          {st;}
    (* next element of the machine *)
    and next m () = match m.st with
      | Done -> None
      | Base -> m.st <- Done; Some []
      | Insert ({x;len;n;l;sub} as state) ->
          if n=len
            then match next sub () with
              | None -> m.st <- Done; None
              | Some l ->
                  state.l <- l;
                  state.n <- 0;
                  next m ()
            else (
              state.n <- state.n + 1;
              Some (insert x n l)
            )
    and insert x n l = match n, l with
      | 0, _ -> x::l
      | _, [] -> assert false
      | _, y::tail -> y :: insert x (n-1) tail
    in
    let l = Gen.fold (fun acc x->x::acc) [] g in
    next (make_machine (List.length l) l)

  (*
  Credits to Bernardo Freitas Paulo da Costa for [permutations_heap]!

  B.R.Heap's algorithm for permutations,
  cf http://en.wikipedia.org/wiki/Heap%27s_algorithm.

  Continuation-based recursive formula, model for the state manipulations
  below:
  {[
  let rec heap_perm k a n =
    match n with
    | 0 -> k a
    | n ->
        for i = 0 to n-1 do
          heap_perm k a (n-1);
          let j = (if n mod 2 = 1 then 0 else i) in
          let t = a.(j) in
          a.(j) <- a.(n-1);
          a.(n-1) <- t
        done
  ]}
  *)

  (* The state of the permutation machine, containing
     - the array [a] we're permuting, in the "current permutation";
     - the level of recursion [n]: we can permute elements with index < [n]
     - the stack of values of indices to permute [i] in the list [is]
     The permutation stops when we have no more elements in the stack [is].
  *)
  module HeapPermState = struct
    type 'a state = {
      elts : 'a array;
      mutable n : int;
      mutable is : int list;
    }
  end

  let permutations_heap g =
    let open HeapPermState in
    let l = Gen.fold (fun acc x->x::acc) [] g in
    let a = Array.of_list l in
    let rec next st () = match st.n with
      | 0 ->
          begin match st.is with
          | [] | _::[] -> assert false
          | 0::i::is' -> (* "Pop state" before returning next element *)
              st.is <- (i+1)::is';
              st.n <- 1;
              Some (Array.copy a)
          | _::_::_ -> assert false
          end
      | n ->
          match st.is with
            | [] -> None
            | i::is' when i = n -> (* Pop state at end of loop *)
                st.is <- is';
                st.n <- n+1;
                begin match st.is with
                | [] -> None (* last loop *)
                | i::is' ->
                    let j = (if st.n mod 2 = 1 then 0 else i) in
                    let tmp = st.elts.(j) in
                    st.elts.(j) <- st.elts.(n);
                    st.elts.(n) <- tmp;
                    st.is <- (i+1)::is';
                    next st ()
                end
            | _::_ -> (* Recurse down and start new loop *)
                st.n <- n-1;
                st.is <- 0 :: st.is;
                next st ()
    in
    let n = Array.length a in
    if n = 0 then Gen.empty
    else next {elts = a; n=n; is=[0]}

  (* take [len] permutations of [1..n] *)
  let bench_it n len =
    Printf.printf "\ntake %d permutations out of [1...%d]\n" len n;
    let run perm () =
      let open Gen in
      perm (1--n) |> take len |> iter (fun _ -> ())
    in
    let res = Benchmark.throughputN 2
      [ "perm_rec", run permutations_rec, ()
      ; "perm_heap", run permutations_heap, ()
      ; "current", run Gen.permutations, ()
      ]
    in
    Benchmark.tabulate res

  let bench_n len n =
    let run perm () =
      let open Gen in
      perm (1--n) |> take len |> iter (fun _ -> ())
    in
    B.throughputN 2 ~repeat:3
      [ "perm_rec", run permutations_rec, ()
      ; "perm_heap", run permutations_heap, ()
      ; "current", run Gen.permutations, ()
      ]

  let () =
    let open B.Tree in
    let app_int f n = string_of_int n @> lazy (f n) in
    let app_ints f l = B.Tree.concat (List.map (app_int f) l) in
    B.Tree.register (
      "perm" @>>>
        [ "len=100" @>> app_ints (bench_n 100) [5; 100; 1_000]
        ; "len=50_000" @>> app_ints (bench_n 50_000) [100; 1_000]
        ])
end

let () =
  try B.Tree.run_global ()
  with Arg.Help msg -> print_endline msg
