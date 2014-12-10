(* zadanie 1 *)

let rec fix f x = f (fix f) x;;

let fact = fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n - 1)));;

assert(fact 5 = 2 * 3 * 4 * 5);;
assert(fact 4 = 2 * 3 * 4);;
assert(fact 8 = 2 * 3 * 4 * 5 * 6 * 7 * 8);;

let rec fix_ref f x = f (fix f) !x;;

let fact_ref n = 
  let res = ref 1 in
  for i = 2 to n do
    res := !res * i
  done;
  !res
;;

assert(fact_ref 6 = 1 * 2 * 3 * 4 * 5 * 6);;

(* zadanie 2 *)

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref;;

let rec concat_copy l1 l2 = match l1 with
  | LMnil -> l2
  | LMcons (x, xs) -> LMcons(x, ref (concat_copy !xs l2))
;;

let rec concat_share xs ys = 
  match !xs with
  | LMnil -> xs := !ys
  | LMcons (_, xs) -> concat_share xs ys
;;

let a1 = concat_copy LMnil LMnil;;
let a2 = ref LMnil;;
concat_share a2 (ref LMnil);;
assert (!a2 = a1);;

let a1 = concat_copy (LMcons(1, ref LMnil)) LMnil;;
let a2 = ref (LMcons(1, ref LMnil));;
concat_share a2 (ref LMnil);;
assert (!a2 = a1);;

let rec list_to_mutable = function
  | h :: t -> ref (LMcons (h, list_to_mutable t))
  | [] -> ref LMnil
;;

let l1 = list_to_mutable [1; 2; 3; 4];;
let l2 = list_to_mutable [5; 6; 7; 8];;

let a1 = concat_copy !l1 !l2;;
let a2 = l1;;
concat_share a2 l2;;
assert (!a2 = a1);;

(* zadanie 3 *)

type ('a, 'b) memo = Empty | Table of 'a * 'b * ('a, 'b) memo ref;;

let create_memo () = ref Empty;;

let rec memo_find table x = 
  match !table with
  | Empty -> None
  | Table(x', y, _) when x = x' -> Some(y)
  | Table(_, _, xs) -> memo_find xs x
;;

let rec memo_add table x y = 
  match !table with
  | Empty -> 
       table := Table (x, y, ref Empty)
  | Table (x', _, xs) ->
      if x = x' then ()
      else memo_add xs x y
;;

let rec memo_fib =
  let fib_table = create_memo () in
  fun n ->
    if n = 1 then 1
    else if n = 2 then 1
    else
      match memo_find fib_table n with
      | None ->
          let result = memo_fib (n - 1) + memo_fib (n - 2) in
          memo_add fib_table n result;
          result
      | Some(y) -> y
;;

let rec fib = function
  | 0 | 1 as i -> i
  | i -> fib (i - 1) + fib (i - 2)
;;

let time f x = 
  let start = Sys.time() in
  let y = f x in
  let finish = Sys.time() in 
  Printf.printf "Time: %f seconds\n" (finish -. start);
  y
;;

time fib 40;;
time memo_fib 40;;
time memo_fib 60;;

assert (memo_fib 40 = fib 40);;

(* zadanie 4 *)

let (fresh, reset) =
  let value = ref 0 in
  let merge str =
    value := !value + 1;
    str ^ string_of_int (!value)
  in (merge, (fun x -> value := x))
;;

reset 1;;
assert (fresh "x" = "x2");;
assert (fresh "x" = "x3");;
assert (fresh "y" = "y4");;
reset 12;;
assert (fresh "x" = "x13");;

(* zadanie 5 *)

type 'a lnode = {item: 'a; mutable next: 'a lnode};;

let mk_circular_list e =
  let rec x = {item = e; next = x} in 
    x
;;

let insert_head e l =
  let x = {item = e; next = l.next} in 
    l.next <- x 
;;

let insert_tail e l =
  let x = {item = e; next = l.next} in
    l.next <- x
;;

let first ln = (ln.next).item;;

let last ln = ln.item;;

let elim_head l = l.next <- (l.next).next;;

let create n =
  let rec aux l = function
    | 0 -> l
    | n -> 
        insert_tail n l;
        aux l (n - 1)
  in aux (mk_circular_list n) (n - 1)
;;

let joseph n m  = 
  let result = ref [] in
  let rec eraseNth m l =
    if m = 1 then 
    (
      result := (l.next).item :: !result;
      elim_head l;
      l
    )
    else eraseNth (m - 1) (l.next) in
  let aux l =
    for i = 1 to n do l := eraseNth m !l done;
    List.rev (!result)
  in aux (ref (create n))
;;

assert(joseph 7 3 = [3; 6; 2; 7; 5; 1; 4]);;
assert(joseph 7 1 = [1; 2; 3; 4; 5; 6; 7]);;

