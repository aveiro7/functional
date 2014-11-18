(* zadanie 1 *)
let x_int = 1;;
let x_float = 5.;;
let x_char = 'a';;
let x_string = "abc";;
let x_bool = false;;
let x_unit = ();;

(* zadanie 2 *)
(* if true then 4 else 3.5*)

(* if false then 1 else 3.5*)

(* 4.75 + 2.34;; *) 

false || "ab" > "cd";;

if true then ();; 

(* if false then () else 4 *)

(* let x = 2 in x ^ "aa" *)

let y = "abc" in y ^ y;;

(fun x -> x.[1]) "abcdef";;

(fun x -> x) true;;

let x = [1;2] in x@x;;

let rec f f = f + f in f 42;;

(* zadanie 3 *)
(* let x = x in x^x;; <- x - zmienna wolna (unbound) *)
let x = 10. in let y = x**2. in y*.x;; (*x i y - zwiazane *)
(* let x = 1 and y = x in x + y;; <- pierwsze x zwiazane, drugie x i y niezwiazane *)
let x = 1 in fun y z -> x * y * z;; (* wszystkie zmienne zwiazane *)


(* zadanie 4 *)
let m = 10;;
let f x = m + x;;
let m = 100;;
f 1;; (* f 1 = 10 + 1 = 11 *)

(* zadanie 5 *)
let f = fun _ -> 42;;
(*f (raise(Invalid_argument "gorliwie!"));; *)

(* zadanie 6 *)
let plus = fun x y -> x + y;;
let plus = fun x -> fun y -> x + y;;
let plus x y = x + y;;

let plus_3 = plus 3;;

(* zadanie 7 *)
fun x -> x;; (* typ a' -> a' = <fun> *)
fun (x:int) -> (x:int);;
fun f g x -> f (g x);;
fun x -> raise(Invalid_argument "nowy typ!");;

(* zadanie 8 *)
let zlozenie f g x = f(g x);;
let id x = x;;
let rec iteruj n f  = match n with
  | 0 -> id
  | _ -> zlozenie f (iteruj (n-1) f);;

let mnoz a b = iteruj a ((+) b ) 0;;
mnoz 2 5;;

let ( *|* ) a b = iteruj b (( * ) a) 1;;
2 *|* 3;;
