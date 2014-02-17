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
  for i = 0 to 100 do
    let _ = persistent_mlist Gen.(1 -- n) in
    ()
  done

let bench_naive n =
  for i = 0 to 100 do
    let l = Gen.to_rev_list Gen.(1 -- n) in
    let _ = Gen.Restart.of_list (List.rev l) in
    ()
  done

let bench_current n =
  for i = 0 to 100 do
    let _ = Gen.persistent Gen.(1 -- n) in
    ()
  done

let () =
  let bench_n n = 
    Printf.printf "BENCH for %d\n" n;
    let res = Benchmark.throughputN 5
      [ "mlist", bench_mlist, n
      ; "naive", bench_naive, n
      ; "current", bench_current, n
      ]
    in Benchmark.tabulate res
  in
  bench_n 100;
  bench_n 100_000;
  ()

(* vim:Use benchmark: *)
