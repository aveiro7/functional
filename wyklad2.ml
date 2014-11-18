(* zadanie 1 *)
let rec fib = function
    | 0 -> 0
      | 1 -> 1
        | n -> fib(n-1) + fib(n-2);;
  
let fib_tail n =
    let rec fib_tail_acc acc1 acc2 = function
          | 0 -> acc1
              | 1 -> acc2
                  | n -> fib_tail_acc acc2 (acc1 + acc2) (n-1)
                      in fib_tail_acc 0 1 n;;
    
(* zadanie 2 *) 
  
let abs = function
    |x when x > 0. -> x
      |x -> -.x;;
  
let root3 a =
    let rec root3tail a eps = function
          | x when (abs((x ** 3.) -. a) <= eps *. (abs a)) -> x
              | x -> root3tail a eps (x +. ( (a /. (x ** 2.)) -. x) /. 3. )
                in 
                    if (a >= 1.) then root3tail a 1e-15 (a /. 3.)
                        else root3tail a 1e-15 a;;
  
(* zadanie 4 *)
let l1 = [-2; -1; 0; 1; 2];;
let l2 = [(1, 2); (0,1)]  ;;

let [_; _; x; _; _] = l1;;

let [(_, _); (x, _)] = l2;;

(* zadanie 5 *)
let rec initsegment = function
    | ([], l) -> true
      | (h1::t1, h2::t2) when h1 = h2 -> initsegment (t1, t2)
        | _ -> false;;
  
(* zadanie 6 *) 
let rec replace_nth (l, n, x) = match (l,n) with
  | (h::t, 0) -> x::t
    | (h::t, n) -> h::(replace_nth (t, (n-1), x))
      | _ -> failwith "Index out of range"
        
        (* zadanie 7 *)
let split l = 
    let rec split_tail acc = function
          | [] -> [(acc, [])]
              | h::t -> (acc, (h::t)) :: split_tail (acc @ [h]) t
                in split_tail [] l;;
  
let insert1 a l =
    let rec insert_temp a = function
          | [] -> []
              | (h1, h2)::t -> (h1 @ [a] @ h2) :: insert_temp a t
                in insert_temp a (split l);;

let rec insert2 a = function
    | [] -> []
      | h::t -> (insert1 a h) @ insert2 a t;;
  
let rec permut = function
    | [] -> [[]]
      | h::t -> insert2 h (permut t)
