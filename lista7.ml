(* zadanie 1 *)

let rec fix f x = f (fix f) x;;

let fact = fix (fun f -> fun n -> if n = 0 then 1 else n * (f (n - 1)));;

assert(fact 5 = 2 * 3 * 4 * 5);;
assert(fact 4 = 2 * 3 * 4);;
assert(fact 8 = 2 * 3 * 4 * 5 * 6 * 7 * 8);;

let rec fix_ref f x = f (fix f) !x;;

let increment = ();;

let fact_ref = fix_ref (fun f -> fun n -> if !n = 0 then (n := 1) else (n := !n
* (f (ref(!n - 1))) ));;

