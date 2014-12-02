type 'a llist = LEmpty | LCons of 'a * (unit -> ('a llist));;

let lhd = function
	| LEmpty -> failwith "lhd"
	| LCons(x, _) -> x
;;

let ltl = function
	| LEmpty -> failwith "ltl"
	| LCons(_, xf) -> xf()
;;

let leibniz =
	let rec next prev n sign = LCons(4. *. prev, function () -> next (prev +. ((1./.n) *. sign)) (n +. 2.) (-. sign))
	in next 1. 3. (-.1.);;
	
let rec take l = function
	| 0 -> 0.
	| 1 -> (lhd l)
	| n -> take (ltl l) (n-1);;
	
let rec takel l = function
	| [] -> []
	| h::t -> (take l h) :: takel l t
;;

List.rev (takel leibniz [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);;

let rec fmap_three f l = LCons(f (take l 1) (take l 2) (take l 3), function () -> fmap_three f (ltl l));;

let euler x y z = z -. (y -. z) ** 2. /. (x -. 2. *. y +. z);;

let better_solution = fmap_three euler leibniz;;
List.rev (takel better_solution [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);;

type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let lhd = function
	| LNil -> failwith "lhd"
	| LCons(x, _) -> x
;;

let ltl = function
	| LNil -> failwith "ltl"
	| LCons(_, lazy xs) -> xs
;;

let rec take l = function
	| 0 -> 0.
	| 1 -> (lhd l)
	| n -> take (ltl l) (n-1);;

let rec takel l = function
	| [] -> []
	| h::t -> (take l h) :: takel l t
;;

let leibniz = 
	let rec next prev n sign = LCons(4. *. prev, lazy (next (prev +. ((1./.n) *. sign)) (n +. 2.) (-. sign)))
	in next 1. 3. (-.1.);;
	
let rec fmap_three f l = LCons(f (take l 1) (take l 2) (take l 3), lazy(fmap_three f (ltl l)));;

let better_solution = fmap_three euler leibniz;;

(* zadanie 2 *)

(* zadanie 3 *)

(* zadanie 4 *)
(* kodowanie Huffmana *)

type 'a codetree = Leaf of 'a * int | Node of int * 'a codetree * 'a codetree;;

let merge t1 t2 = match (t1, t2) with
  | (Leaf(_, n1), Leaf(_, n2)) -> Node((n1 + n2), t1, t2)
  | (Leaf(_, n1), Node(n2, _, _)) -> Node((n1 + n2), t1, t2)
  | (Node(n1, _, _), Leaf(_, n2)) -> Node((n1 + n2), t1, t2)
  | (Node(n1, _, _), Node(n2, _, _)) -> Node((n1 + n2), t1, t2)
;;

let build_tree = function
  | [] -> []
  | [x] -> [x]
  | h1::h2::t -> []
;;


