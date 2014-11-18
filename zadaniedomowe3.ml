(* zadanie 4 *)

let check_matrix m = List.fold_left 
  (fun acc l -> if List.length l = List.length m then acc else false) true m;;

assert (check_matrix [] = true);;
assert (check_matrix [[1;2];[1;2]] = true);;
assert (check_matrix [[1; 2; 3]; [1; 2; 3]; [1; 2; 3]; [1; 2; 3]] = false);;
assert (check_matrix [[1; 2; 3]; [1; 2; 3; 4]; [1; 2; 3]] = false);;

let get_column m n = List.fold_right (fun l acc -> (List.nth l (n-1)) :: acc) m [];;

assert (get_column [[1;2;3];[4;5;6];[7;8;9]] 1 = [1;4;7]);;

let transpose m = List.fold_left 
  (fun acc l -> ((get_column m ((List.length m) - (List.length acc))) :: acc)) [] m;;

assert (transpose [[1;2;3];[4;5;6];[7;8;9]] = [[1;4;7];[2;5;8];[3;6;9]]);;

let zip a b = List.combine a b;;

assert (zip [1.;2.;3.] ["a";"b";"c"] = [(1.,"a");(2.,"b");(3.,"c")]);;

let zipf f a b = List.fold_right (fun (b, c) a -> (f b c) :: a) (zip a b) [];; 

assert (zipf (+.) [1.; 2.;3.] [4.; 5.; 6.] = [5.; 7.; 9.]);;

let get_products v m i = zipf ( *. ) v (get_column m (List.length m - i));;

let mult_vec v m = List.fold_left 
  (fun acc l -> (List.fold_left (+.) 0. (get_products v m (List.length acc))) :: acc) 
  [] m;;

assert (mult_vec [1.;2.] [[2.;1.];[4.;5.]] = [10.; 11.]);;

let mult m1 m2 = List.fold_right (fun v acc -> (mult_vec v m2) :: acc) m1 [];;

assert (mult [[1.; 2.];[1.; 2.]] [[2.; 1.]; [2.; 1.]] = [[6.;3.];[6.;3.]]);;


(* zadanie 6 *)

let is_prime x =
  if x = 1 then false
  else if x = 2 then true
  else begin
    let rec test acc = match x mod acc with
      | 0 -> false
      | _ when acc * acc > x -> true
      | _ -> test (acc + 1)
    in test 2
  end;;

let is_sum_of_primes x = 
  if x mod 2 = 0 then true (* Goldbach's Conjecture *)
  else if is_prime (x-2) then true
  else false;;
     
let not_sum_of_primes n = 
  let rec temp acc = function
    | x when x > n -> acc
    | x when is_sum_of_primes x -> temp acc (x+2)
    | x -> temp (x::acc) (x+2)
  in temp [] 5;; (* x + y > 4 and  x + y - odd *)

let get_sums n =
  let rec get_sums_acc acc = function
    | x when (x < (n-x) && (is_prime (n - x) = false || is_prime x = false)) -> 
      get_sums_acc ((x, (n-x), n, (x * (n-x))) :: acc) (x+1)
    | x when x < (n-x) -> get_sums_acc acc (x+1)
    | x -> acc
  in get_sums_acc [] 2;;


let order_by_last (_, _, _, a) (_, _, _, b) = 
  if (a > b) then 1
  else if (a = b) then 0
  else -1;;

let order_by_third (_, _, a, _) (_, _, b, _) =
  if (a > b) then 1
  else if (a = b) then 0
  else -1;;

let first_part_of_solution l = 
  let rec get_first_part acc = function
    | h::t -> get_first_part ((get_sums h) @ acc) t
    | [] -> acc
  in List.sort order_by_last (get_first_part [] l);;


let eliminate_repeated_product l = 
  let rec eliminate_last acc = function
    | [] -> []
    | [(_, _, _, a)] when a = acc -> []
    | [h] -> [h]
    | (_, _, _, a) :: (_, _, _, b) :: t when a = b ->
        eliminate_last a t
    | (_, _, _, a) :: (_, _, _, b) :: t when acc = a -> 
        eliminate_last b t
    | h1 :: (_, _, _, b) :: t -> h1 :: (eliminate_last b t)
  in eliminate_last 0 l;;

let second_part_of_solution l = List.sort order_by_third 
      (eliminate_repeated_product (first_part_of_solution l));;

let eliminate_repeated_sum l =
  let rec eliminate_third acc = function
    | [] -> []
    | [(_, _, a, _)] when a = acc -> []
    | [h] -> [h]
    | (_, _, a, _) :: (_, _, b, _) :: t when a = b ->
        eliminate_third a t
    | (_, _, a, _) :: (_, _, b, _) :: t when acc = a -> 
        eliminate_third b t
    | h1 :: (_, _, b, _) :: t -> h1 :: (eliminate_third b t)
  in eliminate_third 0 l;;

let solution = let [(x, y, _, _)] = 
  eliminate_repeated_sum (second_part_of_solution (not_sum_of_primes 100))
in (x,y);;
