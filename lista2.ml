(* zadanie 1  [dla SPOJNEJ podlisty]*)

let prefix l =
  let rec pref_acc acc = function
    | [] -> []
    | h::t -> List.rev(h::acc) :: pref_acc (h::acc) t
  in pref_acc [] l;;

let rec sublists = function
  | [] -> []
  | h::t -> (prefix (h::t)) @ (sublists t);;

(* dla niespojnych *)

let rec insert_head x = function
  | [] -> [[x]]
  | h :: t -> (x :: h) :: (h :: insert_head x t);;

let rec general_sublists acc = function 
  | [] -> acc
  | h :: t -> insert_head h (general_sublists acc t);;

(* zadanie 2 *)

let rec calc = function
  | 0 -> 1
  | 1 -> 2
  | n -> 2 * (calc (n-2)) - (calc (n-1)) + 1;;

let calc2 n =
  let rec calc_tail acc1 acc2 = function
    | 0 -> acc1
    | 1 -> acc2
    | n -> calc_tail acc2 (2 * acc1 - acc2 + 1) (n-1)
  in calc_tail 1 2 n;;

(* zadanie 3 *)

let rec reverse f = function
  |[] -> []
  |h::t -> (reverse f  t) @ [f h];;

let reverse2 l =
  let rec reverse_tail acc f = function
    | [] -> acc
    | h::t -> reverse_tail ((f h) :: acc) f  t
  in reverse_tail [] l;;



(* sublists [1; 2; 3; 4; 5; 6];;

reverse (fun x -> x) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];;
reverse (fun x -> x)  [1; 2; 3; 4; 5; 6; 7; 8; 9; 10];; *)
