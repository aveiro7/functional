(* zadanie 1 *)
let f1 x = x 1 1;;
(* (int -> int -> 'a) -> 'a *)

let f2 x y z = x (y ^ z);;
(* (string -> 'a) -> string -> string -> 'a *)

let f3 x y z = x y z;;
(* ('a -> 'b -> 'c) -> 'a -> 'b -> 'c *)

let f4 x y = function z -> x::y;;
(* 'a -> 'a list -> 'b -> 'a list *)


(* zadanie 2 *)
let uncurry3 f (x, y, z) = f x y z;;
(* ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd *)

let curry3 f x y z = f (x, y, z);;
(* ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd *)

(* zadanie 3 *)
let sumprod = List.fold_left (fun (acc1, acc2) h -> (acc1 + h, acc2 * h)) (0, 1);;

(* zadanie 4 *)
let rec quicksort = function
  | [] -> []
  | [x] -> [x]
  | xs -> let small = List.filter (fun y -> y < List.hd xs) xs
          and large = List.filter (fun y -> y >= List.hd xs) xs
          in quicksort small @ quicksort large
;;
(* problem pojawia sie, gdy wszystkie elementy w liscie sa nie mniejsze od
 * glowy. Wtedy funkcja zapetla sie, bo large = xs. *)

let rec quicksort' = function
  | [] -> []
  | x::xs -> let small = List.filter (fun y -> y < x) xs
            and large = List.filter (fun y -> y > x) xs
            in quicksort' small @ (x :: quicksort' large)
;;
(* ta implementacja eliminuje powtorzenia elementow *)

let rec divide x small large equal = function
  | h :: t when h < x -> divide x (h :: small) large equal t
  | h :: t when h > x -> divide x small (h :: large) equal t
  | h :: t -> divide x small large (h :: equal) t
  | [] -> (small, large, equal)
;;

let rec my_quicksort = function
  | h :: t -> 
      let (s, l, e) = divide h [] [] [] t in
      (my_quicksort s) @ (h :: e) @ (my_quicksort l)
  | [] -> []
;;

assert (my_quicksort [1; 4; 3; 2; 3; 5; 1] = [1; 1; 2; 3; 3; 4; 5]);;

(* zadanie 5 *)
let rec insert comp x = function
  | [] -> [x]
  | h :: t when comp h x <= 0 -> h :: (insert comp x t)
  | h :: t -> x :: h :: t
;;

let insort comp = 
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (insert comp  h acc) t
  in aux []
;;

let pair_comp (a, _) (b, _) =
  if a < b then -1
  else if a > b then 1
  else 0
;;

assert(insort pair_comp [(1,2); (1,3); (1,4)] = [(1,2); (1,3); (1,4)]);;

let split l = 
  let rec aux acc1 acc2 = function
    | [] | [_] -> ((List.rev acc1), acc2)
    | _ :: _ :: t -> 
        let (x :: xs) = acc2 in
        aux (x :: acc1) (xs) t
  in aux [] l l
;;

let rec merge comp t1 t2 = match (t1, t2) with
  | ([], _) -> t2
  | (_, []) -> t1
  | (x1 :: xs1, x2 :: xs2) ->
      if comp x1 x2 < 0 then x1 :: (merge comp xs1 t2)
      else if comp x1 x2 = 0 then x1 :: x2 :: (merge comp xs1 xs2)
      else x2 :: (merge comp t1 xs2)
;;

let rec mergesort comp = function
  | [] -> []
  | [x] -> [x]
  | xs ->
      let (left, right) = split xs in
        merge comp (mergesort comp left) (mergesort comp right)
;;

mergesort pair_comp [(1,2);(1,3);(1,4)];;
