(* dictionary *)
let empty = fun key -> 0;;
let add dict key v = fun key' -> 
	if key = key' then
		v
	else
		dict key;;

let find dict key = dict key;;

(* calculating quadratic equation roots ax^2 + bx + c = 0 *)
let r b c =
	fun a -> (-.b +. sqrt (b *. b -. 4. *. a *. c)) /. 2. *. a

(* stream *)
let hd s = s 0;;
let tl s = fun i -> s (i + 1);;
let (+:) s a = fun i -> s i + a;;
let (-|) s1 s2 = fun i -> (s1 i) - (s2 i);;
let map f s = fun i -> f s i;;
let derivative s = tl s -| s;;
let integral derivative s = s +: 1;;


(* PATTERN MATCHING *)
(*match expression with 
| pattern -> expression
|
|*)
(* first | is optional *)

let rec fib i =
	match i with
		|0 -> 0
		|1 -> 1
		|j -> fib(j-1) + fib(j-2);;


let rec fib = function
	0|1 as i -> i
	|j -> fib(j-1) + fib(j-2);;


let is_uppercase = function
	'A' .. 'Z' -> true
	| c -> false;;

let is_uppercase = function
	'A' .. 'Z' -> true
	| _ -> false;;

let is_odd i = 
	match i mod 2 with
	|0 -> false
	|1 -> true
	|_ -> raise(Invalid_argument "is_odd");;

(* encoding algorithm *)
let cipher = function
	|'A' -> 'C'
	|'B' -> 'A'
	|'C' -> 'D'
	|'D' -> 'B'
	|_ -> raise(Invalid_argument "cipher");;

let rec check s1 s2 =
	match s1, s2 with
		| x::xs, y::ys when (cipher x) = y -> check xs ys
		| [], [] -> true
		| _ -> false;;

let rec check s1 s2 =
	let k1 = String.length s1 in
	let k2 = String.length s2 in
	if k1 = 0 && k2 = 0 then
		true
	else if (cipher (s1.[0])) = s2.[0] then
		check (String.sub s1 1 (k1 - 1)) (String.sub s2 1 (k2 - 1))
	else
		false;;

check "ABC" "CAD"
