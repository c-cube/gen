open Gen

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
  let l = fold (fun acc x->x::acc) [] g in
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
  let l = fold (fun acc x->x::acc) [] g in
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
  if n = 0 then empty
  else next {elts = a; n=n; is=[0]}

(* take [len] permutations of [1..n] *)
let bench_it n len =
  Printf.printf "\ntake %d permutations out of [1...%d]\n" len n;
  let run perm () =
    perm (1--n) |> take len |> iter (fun _ -> ())
  in
  let res = Benchmark.throughputN 2
    [ "perm_rec", run permutations_rec, ()
    ; "perm_heap", run permutations_heap, ()
    ; "current", run Gen.permutations, ()
    ]
  in
  Benchmark.tabulate res

let () =
  bench_it 5 100;
  bench_it 100 100;
  bench_it 1000 100;
  bench_it 10 500_000;
  bench_it 10 10_000_000;
  bench_it 20 500_000;
  bench_it 100 500_000;
  ()

