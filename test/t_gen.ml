module T = (val Gen_testlib.make ~__FILE__ ())
include T

[@@@ocaml.warning "-26-33-52"]

open Gen

  let pint i = string_of_int i
  let pilist l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp Format.pp_print_int) (Gen.of_list l);
    Buffer.contents b
  let pi2list l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp (fun fmt (a,b) -> Format.fprintf fmt "%d,%d" a b))
      (Gen.of_list l);
    Buffer.contents b
  let pstrlist l =
    let b = Buffer.create 15 in
    let fmt = Format.formatter_of_buffer b in
    Format.fprintf fmt "%a@?"
      (Gen.pp Format.pp_print_string) (Gen.of_list l);
    Buffer.contents b

let rec seq_take i seq () =
    if i=0 then Seq.Nil
    else match seq() with
      | Seq.Nil -> Seq.Nil
      | Seq.Cons (x,tl) -> Seq.Cons (x, seq_take (i-1) tl)
  let seq_to_list seq =
    let rec aux acc s = match s() with
      | Seq.Nil -> List.rev acc
      | Seq.Cons (x,tl) -> aux (x::acc) tl
    in
    aux [] seq

(* Tests converted from qtest annotations *)

let () = t ~name:"empty" (fun () -> (empty |> to_list = []))

let () = t ~name:"singleton_1" (fun () -> (singleton 1 |> to_list = [1]))
let () = t ~name:"singleton_2" (fun () -> (singleton "foo" |> to_list = ["foo"]))

let () = t ~name:"r1" (fun () ->
  let gen = Gen.singleton 42 in
  assert_equal (Some 42) (Gen.get gen); assert_equal None (Gen.get gen);
  let gen = Gen.singleton 42 in
  assert_equal 1 (Gen.length gen);
  true
)

let () = t ~name:"repeat" (fun () -> (repeat 42 |> take 3 |> to_list = [42; 42; 42]))

let () = t ~name:"repeatedly" (fun () -> (repeatedly (let r = ref 0 in fun () -> incr r; !r)    |> take 5 |> to_list = [1;2;3;4;5]))

let () = t ~name:"iterate" (fun () -> (iterate 0 ((+)1) |> take 5 |> to_list = [0;1;2;3;4]))

let () = t ~name:"get_exn" (fun () ->
  let g = of_list [1;2;3] in
  assert_equal 1 (get_exn g); assert_equal 2 (get_exn g); assert_equal 3 (get_exn g); assert_raises (function Invalid_argument "Gen.get_exn" -> true | _ -> false) (fun () -> get_exn g);
  true
)

let () = q ~name:"q1" (Q.list Q.nat_small) (fun l -> of_list l |> fold (fun l x->x::l) [] = List.rev l)

let () = t ~name:"unfold" (fun () -> (unfold (fun (prev,cur) -> Some (prev, (cur,prev+cur))) (0,1)    |> take 7 |> to_list = [0; 1; 1; 2; 3; 5; 8]))

let () = t ~name:"init" (fun () -> (init ~limit:5 (fun i->i) |> to_list = [0;1;2;3;4]))

let () = t ~name:"iter" (fun () ->
  let e = Restart.(1 -- 10) in
  assert_equal ~printer:pint 10 (Restart.length e); assert_equal [1;2] Restart.(to_list (1 -- 2)); assert_equal [1;2;3;4;5] (Restart.to_list (Restart.take 5 e));
  true
)

let () = t ~name:"t8_1" (fun () -> (is_empty empty))
let () = t ~name:"t8_2" (fun () -> (not (is_empty (singleton 2))))

let () = q ~name:"q2" (Q.list Q.nat_small) (fun l -> of_list l |> length = List.length l)

let () = t ~name:"scan" (fun () -> (scan (fun acc x -> x+1::acc) [] (1--5) |> to_list    = [[]; [2]; [3;2]; [4;3;2]; [5;4;3;2]; [6;5;4;3;2]]))

let () = t ~name:"unfold_scan" (fun () -> (unfold_scan (fun acc x -> x+acc,acc) 0 (1--5) |> to_list    = [0; 1; 3; 6; 10]))

let () = q ~name:"q3" (Q.list Q.nat_small) (fun l -> let f x = x*2 in of_list l |> map f |> to_list = List.map f l)

let () = t ~name:"r4" (fun () ->
  let e = 1 -- 10 in
  let e' = e >>| string_of_int in
  assert_equal ~printer:pstrlist ["9"; "10"] (Gen.to_list (Gen.drop 8 e'));
  true
)

let () = q ~name:"q4" (Q.list Q.nat_small) (fun l -> let len = List.length l in let f i x = i+x+1 in of_list l |> mapi f |> to_list |> fun l' -> List.fold_left (+) 0 l'= len*(len+1)/2 + List.fold_left (+) 0 l)

let () = t ~name:"t11" (fun () -> (fold_map (+) 0 (1--3) |> to_list = [1;3;6]))

let () = q ~name:"q5" (Q.pair (Q.list Q.nat_small)(Q.list Q.nat_small)) (fun (l1,l2) -> append (of_list l1) (of_list l2) |> to_list = l1 @ l2)

let () = t ~name:"r5" (fun () ->
  let e = Gen.append (1 -- 5) (6 -- 10) in
  assert_equal [10;9;8;7;6;5;4;3;2;1] (Gen.to_rev_list e);
  true
)

let () = q ~name:"q6" (Q.list Q.nat_small) (fun l -> let f x = of_list [x;x*2] in eq (map f (of_list l) |> flatten) (flat_map f (of_list l)))

let () = t ~name:"t12" (fun () -> (flat_map (fun x -> if x mod 1_500_000=0 then singleton x else empty) (1 -- 6_000_000)    |> to_list = [1_500_000; 3_000_000; 4_500_000; 6_000_000]))

let () = t ~name:"r6" (fun () ->
  let e = 1 -- 3 in
  let e' = e >>= (fun x -> x -- (x+1)) in
  assert_equal [1;2;2;3;3;4] (Gen.to_list e');
  true
)

let () = q ~name:"q7" (Q.pair Q.nat_small (Q.list Q.nat_small)) (fun (n,l) -> of_list l |> take n |> length = GenShims_.Stdlib.min n (List.length l))

let () = q ~name:"q8" (Q.pair Q.nat_small (Q.list Q.nat_small)) (fun (n,l) -> let g1,g2 = take n (of_list l), drop n (of_list l) in append g1 g2 |> to_list = l)

let () = t ~name:"nth" (fun () ->
  assert_equal ~printer:string_of_int 4 (nth 4 (0--10));
  assert_equal ~printer:string_of_int 8 (nth 8 (0--10));
  true
)

let () = t ~name:"t13" (fun () -> ((try ignore (nth 11 (1--10)); false with Not_found -> true)))

let () = t ~name:"t14" (fun () -> (filter (fun x ->x mod 2 = 0) (1--10) |> to_list = [2;4;6;8;10]))

let () = t ~name:"t15" (fun () -> (take_while (fun x ->x<10) (1--1000) |> eq (1--9)))

let () = t ~name:"t16" (fun () -> (fold_while (fun acc b -> if b then acc+1, `Continue else acc, `Stop) 0    (of_list [true;true;false;true]) = 2))

let () = t ~name:"t17" (fun () -> (drop_while (fun x-> x<10) (1--20) |> eq (10--20)))

let () = t ~name:"t18" (fun () -> (filter_map (fun x-> if x mod 2 = 0 then Some (string_of_int x) else None) (1--10)    |> to_list = List.map string_of_int [2;4;6;8;10]))

let () = t ~name:"r7" (fun () ->
  let f x = if x mod 2 = 0 then Some (string_of_int x) else None in
  let e = Gen.filter_map f (1 -- 10) in
  assert_equal ["2"; "4"; "6"; "8"; "10"] (Gen.to_list e);
  true
)

let () = t ~name:"t19" (fun () -> (zip_index (1--5) |> to_list = [0,1; 1,2; 2,3; 3,4; 4,5]))

let () = t ~name:"t20" (fun () -> (unzip (of_list [1,2;3,4]) |> (fun (x,y)-> to_list x, to_list y)    = ([1;3], [2;4])))

let () = q ~name:"q9" (Q.list (Q.pair Q.nat_small Q.nat_small)) (fun l -> of_list l |> unzip |> (fun (x,y) -> to_list x,to_list y) = List.split l)

let () = t ~name:"t21" (fun () -> (partition (fun x -> x mod 2 = 0) (1--10) |>    (fun (x,y)->to_list x, to_list y) = ([2;4;6;8;10], [1;3;5;7;9])))

let () = t ~name:"t22_1" (fun () -> (min (of_list [1;4;6;0;11; -2]) = ~-2))
let () = t ~name:"t22_2" (fun () -> ((try ignore (min empty); false with Invalid_argument _ -> true)))

let () = t ~name:"t23_1" (fun () -> (max (of_list [1;4;6;0;11; -2]) = 11))
let () = t ~name:"t23_2" (fun () -> ((try ignore (max empty); false with Invalid_argument _ -> true)))

let () = q ~name:"q10" (Q.pair (Q.list Q.nat_small)(Q.list Q.nat_small)) (fun (l1,l2) -> eq (of_list l1)(of_list l2) = (l1 = l2))

let () = q ~name:"q11" (Q.pair (Q.list Q.nat_small)(Q.list Q.nat_small)) (fun (l1,l2) -> let sign x = if x < 0 then -1 else if x=0 then 0 else 1 in sign (compare (of_list l1)(of_list l2)) = sign (GenShims_.Stdlib.compare l1 l2))

let () = t ~name:"t24_1" (fun () -> (find (fun x -> x>=5) (1--10) = Some 5))
let () = t ~name:"t24_2" (fun () -> (find (fun x -> x>5) (1--4) = None))

let () = t ~name:"t25" (fun () -> (sum (1--10) = 55))

let () = t ~name:"t26_1" (fun () -> (map2 (+) (1--5) (1--4) |> eq (of_list [2;4;6;8])))
let () = t ~name:"t26_2" (fun () -> (map2 (+) (1--5) (repeat 0) |> eq (1--5)))

let () = t ~name:"iter2" (fun () -> (let r = ref 0 in iter2 (fun _ _ -> incr r) (1--10) (4--6); !r = 3))

let () = q ~name:"q12" (Q.list Q.nat_small) (fun l -> zip_with (fun x y->x,y) (of_list l) (of_list l) |> unzip |> fst |> to_list = l)

let () = t ~name:"r8" (fun () ->
  let e = Gen.zip_with (+) (Gen.repeat 1) (4--7) in
  assert_equal [5;6;7;8] (Gen.to_list e);
  true
)

let () = t ~name:"t28" (fun () -> (merge (of_list [of_list [1;3;5]; of_list [2;4;6]; of_list [7;8;9]])    |> to_list |> List.sort GenShims_.Stdlib.compare = [1;2;3;4;5;6;7;8;9]))

let () = t ~name:"r9" (fun () ->
  let e = of_list [1--3; 4--6; 7--9] in
  let e' = merge e in
  assert_equal [1;2;3;4;5;6;7;8;9] (to_list e' |> List.sort GenShims_.Stdlib.compare);
  true
)

let () = t ~name:"t29" (fun () -> (intersection (of_list [1;1;2;3;4;8]) (of_list [1;2;4;5;6;7;8;9])    |> to_list = [1;2;4;8]))

let () = t ~name:"t30" (fun () -> (sorted_merge (of_list [1;2;2;3;5;10;100]) (of_list [2;4;5;6;11])    |> to_list = [1;2;2;2;3;4;5;5;6;10;11;100]))

let () = t ~name:"r10" (fun () ->
  [Gen.of_list [1;3;5]; Gen.of_list [0;1;1;3;4;6;10]; Gen.of_list [2;2;11]]
|> Gen.sorted_merge_n ?cmp:None
|> Gen.to_list
|> assert_equal ~printer:pilist [0;1;1;1;2;2;3;3;4;5;6;10;11];
  true
)

let () = t ~name:"t31" (fun () -> (sorted_merge_n [of_list [1;2;2;3;5;10;100]; of_list [2;4;5;6;11]; (6--10)]    |> to_list = [1;2;2;2;3;4;5;5;6;6;7;8;9;10;10;11;100]))

let () = t ~name:"t32" (fun () -> (round_robin ~n:3 (1--12) |> List.map to_list =    [[1;4;7;10]; [2;5;8;11]; [3;6;9;12]]))

let () = t ~name:"r11" (fun () ->
  let e = Restart.round_robin ~n:2 Restart.(1--10) in
  (match e with
  | [a;b] ->
    assert_equal [1;3;5;7;9] (Gen.to_list a);
    assert_equal [2;4;6;8;10] (Gen.to_list b)
  | _ -> assert_failure "wrong list length");
  true
)

let () = t ~name:"r12" (fun () ->
  let e = Restart.round_robin ~n:3 Restart.(1 -- 999) in
  let l = List.map Gen.length e in
  assert_equal [333;333;333] l;
  true
)

let () = t ~name:"t33" (fun () -> (tee ~n:3 (1--12) |> List.map to_list =    [to_list (1--12); to_list (1--12); to_list (1--12)]))

let () = t ~name:"t34" (fun () -> (interleave (repeat 0) (1--5) |> take 10 |> to_list =    [0;1;0;2;0;3;0;4;0;5]))

let () = t ~name:"r13" (fun () ->
  let e1 = Gen.of_list [1;3;5;7;9] in
  let e2 = Gen.of_list [2;4;6;8;10] in
  let e = Gen.interleave e1 e2 in
  assert_equal [1;2;3;4;5;6;7;8;9;10] (Gen.to_list e);
  true
)

let () = t ~name:"t35" (fun () -> (intersperse 0 (1--5) |> to_list = [1;0;2;0;3;0;4;0;5]))

let () = t ~name:"r14" (fun () ->
  let e = 1 -- 5 in
  let e' = Gen.intersperse 0 e in
  assert_equal [1;0;2;0;3;0;4;0;5] (Gen.to_list e');
  true
)

let () = t ~name:"t36" (fun () -> (product (1--3) (of_list ["a"; "b"]) |> to_list    |> List.sort GenShims_.Stdlib.compare =      [1, "a"; 1, "b"; 2, "a"; 2, "b"; 3, "a"; 3, "b"]))

let () = t ~name:"r15" (fun () ->
  let printer = pi2list in
  let e = Gen.product (1--3) (4--5) in
  assert_equal ~printer [1,4; 1,5; 2,4; 2,5; 3,4; 3,5] (List.sort GenShims_.Stdlib.compare (Gen.to_list e));
  true
)

let () = t ~name:"t37" (fun () -> (group (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list =    [[0;0;0];[1];[0];[2;2];[3];[4];[5;5;5;5];[10]]))

let () = t ~name:"t38" (fun () -> (uniq (of_list [0;0;0;1;0;2;2;3;4;5;5;5;5;10]) |> to_list =    [0;1;0;2;3;4;5;10]))

let () = t ~name:"t39" (fun () -> (sort (of_list [0;0;0;1;0;2;2;3;4;5;5;5;-42;5;10]) |> to_list =    [-42;0;0;0;0;1;2;2;3;4;5;5;5;5;10]))

let () = t ~name:"t40" (fun () -> (sort_uniq (of_list [0;0;0;1;0;2;2;3;4;5;42;5;5;42;5;10]) |> to_list =    [0;1;2;3;4;5;10;42]))

let () = t ~name:"t41" (fun () -> (chunks 25 (0--100) |> map Array.to_list |> to_list =    List.map to_list [(0--24); (25--49);(50--74);(75--99);(100--100)]))

let () = q ~name:"q13" (Q.(list int)) (fun l -> of_list l |> chunks 25 |> flat_map of_array |> to_list = l)

let () = t ~name:"permutations_1" (fun () -> (permutations (1--3) |> to_list |> List.sort GenShims_.Stdlib.compare =    [[1;2;3]; [1;3;2]; [2;1;3]; [2;3;1]; [3;1;2]; [3;2;1]]))
let () = t ~name:"permutations_2" (fun () -> (permutations empty |> to_list = [[]]))
let () = t ~name:"permutations_3" (fun () -> (permutations (singleton 1) |> to_list = [[1]]))

let () = t ~name:"permutations_heap_1" (fun () -> (permutations_heap (1--3) |> to_list |> List.sort GenShims_.Stdlib.compare =    [[|1;2;3|]; [|1;3;2|]; [|2;1;3|]; [|2;3;1|]; [|3;1;2|]; [|3;2;1|]]))
let () = t ~name:"permutations_heap_2" (fun () -> (permutations_heap empty |> to_list = []))
let () = t ~name:"permutations_heap_3" (fun () -> (permutations_heap (singleton 1) |> to_list = [[|1|]]))

let () = t ~name:"t44_1" (fun () -> (combinations 2 (1--4) |> map (List.sort GenShims_.Stdlib.compare)    |> to_list |> List.sort GenShims_.Stdlib.compare =    [[1;2]; [1;3]; [1;4]; [2;3]; [2;4]; [3;4]]))
let () = t ~name:"t44_2" (fun () -> (combinations 0 (1--4) |> to_list = [[]]))
let () = t ~name:"t44_3" (fun () -> (combinations 1 (singleton 1) |> to_list = [[1]]))

let () = t ~name:"t45_1" (fun () -> (power_set (1--3) |> map (List.sort GenShims_.Stdlib.compare)    |> to_list |> List.sort GenShims_.Stdlib.compare =    [[]; [1]; [1;2]; [1;2;3]; [1;3]; [2]; [2;3]; [3]]))
let () = t ~name:"t45_2" (fun () -> (power_set empty |> to_list = [[]]))
let () = t ~name:"t45_3" (fun () -> (power_set (singleton 1) |> map (List.sort GenShims_.Stdlib.compare)    |> to_list |> List.sort GenShims_.Stdlib.compare = [[]; [1]]))

let () = q ~name:"q14" (Q.list Q.nat_small) (fun l -> to_rev_list (of_list l) = List.rev l)

let () = q ~name:"q15" (Q.array Q.nat_small) (fun a -> of_array a |> to_array = a)

let () = t ~name:"" (fun () ->
  assert_equal ~printer:Q.Print.(list int) [1;2;3;4] (int_range 1 4 |> to_list);
  assert_equal ~printer:Q.Print.(list int) [4;3;2;1] (int_range ~step:~-1 4 1 |> to_list);
  assert_equal ~printer:Q.Print.(list int) [6;4;2] (int_range 6 1 ~step:~-2 |> to_list);
  assert_equal ~printer:Q.Print.(list int) [] (int_range 4 1 |> to_list);
  true
)

let () = t ~name:"" (fun () ->
  assert_equal ~printer:Q.Print.(list string) ["abc"; "de"; ""] (lines (of_string "abc\nde\n\n") |> to_list);
  true
)

let () = q ~name:"q16" Q.string_printable (fun s -> of_string s |> lines |> unlines |> to_string |> String.trim = String.trim s)

let () = t ~name:"t46_1" (fun () -> (let g = 1--10 in let g' = persistent g in    Restart.to_list g' = Restart.to_list g'))
let () = t ~name:"t46_2" (fun () -> (let g = 1--10 in let g' = persistent g in    Restart.to_list g' = [1;2;3;4;5;6;7;8;9;10]))

let () = t ~name:"t47" (fun () -> (let g = 1--100_000 in  let seq = persistent_to_seq g in    (seq |> seq_take 100 |> seq_to_list = (1--100 |> to_list)) &&    (seq |> seq_take 200 |> seq_to_list = (1--200 |> to_list)) &&    (seq |> seq_take 80_000 |> seq_to_list = (1--80_000 |> to_list)) &&    (seq |> seq_take 50_000 |> seq_to_list = (1--50_000 |> to_list))))

let () = t ~name:"r16" (fun () ->
  let i = ref 0 in
  let gen () =
  let j = !i in
  if j > 5 then None else (incr i; Some j) in
  let e = Gen.persistent gen in
  assert_equal [0;1;2;3;4;5] (Restart.to_list e); assert_equal [0;1;2;3;4;5] (Restart.to_list e); assert_equal [0;1;2;3;4;5] (Restart.to_list e);
  true
)

let () = t ~name:"t48" (fun () -> (let g = 1--1_000_000_000 in let g' = persistent_lazy g in    (g' () |> take 100 |> to_list = (1--100 |> to_list)) &&    (g' () |> take 200 |> to_list = (1--200 |> to_list))))

let () = t ~name:"t49" (fun () -> (let g = 1--1_000_000_000 in  let seq = persistent_lazy_to_seq g in    (seq |> seq_take 100 |> seq_to_list = (1--100 |> to_list)) &&    (seq |> seq_take 200 |> seq_to_list = (1--200 |> to_list)) &&    (seq |> seq_take 80_000 |> seq_to_list = (1--80_000 |> to_list)) &&    (seq |> seq_take 50_000 |> seq_to_list = (1--50_000 |> to_list))))

let () = t ~name:"" (fun () ->
  assert_equal ~printer:Q.Print.(list (pair int (option int))) [] (peek (of_list []) |> to_list);
  assert_equal ~printer:Q.Print.(list (pair int (option int))) [1, Some 2; 2, Some 3; 3, Some 4; 4, None] (peek (1 -- 4) |> to_list);
  true
)

let () = q ~name:"q17" (Q.(list int)) (fun l -> l = [] || (of_list l |> peek |> filter_map snd |> to_list = List.tl l))

let () = t ~name:"" (fun () ->
  assert_equal ~printer:Q.Print.(list (pair int (array int))) [] (peek_n 1 (of_list []) |> to_list);
  assert_equal ~printer:Q.Print.(list (pair int (array int))) [1, [|2;3|]; 2, [|3;4|]; 3, [|4|]; 4, [||]] (peek_n 2 (1 -- 4) |> to_list);
  assert_equal ~printer:Q.Print.(list (pair int (array int))) [1, [|2;3;4|]; 2, [|3;4;5|]; 3, [|4;5|]; 4, [|5|]; 5,[||]]    (peek_n 3 (1 -- 5) |> to_list);
  true
)

let () = q ~name:"qr1" Q.(list nat_small) (fun l -> let l' = of_list l |> peek_n 10 |> filter_map (fun (_,a) -> if a=[||] then None else Some a.(0)) |> to_list in l = [] || l' = List.tl l)
