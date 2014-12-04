(* zadanie 1 *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec concat l1 l2 = match l1 with
  | LNil -> l2
  | LCons(a, f) -> LCons (a, (function () -> concat (f()) l2))
;;

let rec createLazyList x = function
  | 0 -> LNil
  | n -> LCons (x, (function () -> createLazyList x (n-1)))
;;

let rec change k = function
  | LNil -> LNil
  | LCons(x, f) -> 
      concat (createLazyList x (k - 1)) 
        (LCons(x, (fun ()  -> change k (f()))))
;;

(* testy *)
let rec toLazy = function
  | [] -> LNil
  | x :: xs ->  LCons(x, (function () -> toLazy xs))
;;

let result = change 2 (toLazy [1; 2; 3; 4; 5]);;

let rec ltake n l = match (n, l) with
  | (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, f)) -> x :: (ltake (n-1) (f ()))
;;

assert (ltake 10 result = [1; 1; 2; 2; 3; 3; 4; 4; 5; 5]);;

(* zadanie 2 *)
let rec sum_fib first second = 
  LCons(first, (function () -> sum_fib second (first + second)))
;;

let lfib = sum_fib 0 1;;

(* zadanie 3 *)
type 'a lBT = LEmpty | LNode of 'a * (unit -> 'a lBT) * (unit -> 'a lBT);;

let rec itr n =
  if n = 0 then LEmpty
  else
    LNode (n, (function () -> itr(2 * n)), (function () -> itr (2 * n + 1)))
;;

let lbfs =
  let rec aux queue = function
    | LEmpty -> (match queue with
      | [] -> LNil
      | h :: t -> aux t h)
    | LNode (x, left, right) -> (match queue with 
      | [] -> LCons(x, function () -> aux [right ()] (left()))
      | h :: t -> LCons (x, function () -> aux (t @ (left() :: [right ()])) h))
  in aux []
;;

ltake 10 (lbfs (itr 1));;

(* zadanie 4 *)
let breadthFirst


(* zadanie 5 *)
