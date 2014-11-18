type 'a llist = 
		LNil
	| LCons of 'a * (unit -> 'a llist);;

let lhd = function
	| LNil -> failwith "lhd"
	| LCons(x, _) -> x
;;

let ltl = function
	| LNil -> failwith "ltl"
	| LCons(_, xf) -> xf()
;;

let rec ltake = function
	| (0, _) -> []
	| (_, LNil) -> []
	| (n, LCons(x,xf)) -> x::ltake(n-1, xf())
;;

let rec next_in_seq k = LCons(k, function () -> next_in_seq (-.1./.((1. /. k) +. 2.)));;

let leibniz n = ltake (n, (next_in_seq 1.));;

leibniz 10;;
(*
let rec drugi_podpunkt f = function
	| LCons(x, LCons(y, LCons(z, fx))) -> LCons ((f x y z), drugi_podpunkt f LCons(y, LCons(z, fx)))
	| _ -> LNil
;;
	
let trzeci_podpunkt = drugi_podpunkt (fun x y z -> z -. ((y -. z)**2.) /. (x -.
2*.y +. z)) leibnitz;; *)
