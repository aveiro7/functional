(* strumienie *)

let hd s = s 0;;
let tl s = fun x -> s (x+1);;

let add s a = fun x -> (s x) + a;;

let map s f = fun x -> f (s x);;

let map2 s1 s2 f = fun x -> f (s1 x) (s2 x);;

let replace n a s x = match (x mod n) with
  | 0 -> a
  | _ -> x;;

let take n s = fun x -> s (n * x);;

let rec fold f a s = function
  | 0 -> f a (s 0)
  | x -> f (fold f a s (x-1)) (s x);;

let rec tabulate ?(a = 0) b s = match b-a with
  | 0 -> [] 
  | _ -> (s a) :: tabulate ~a:(a+1) b s;;

(* testy *)

let s x = x+1;;

assert (hd s = 1);;

assert (tabulate 10 s = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]);;

let t x = x;;

assert (hd t = 0);;

assert (hd (tl t) = hd s);;

assert (tabulate 5 (tl s) = [2; 3; 4; 5; 6]);;

assert (tabulate 5 (add s 4) = [5; 6; 7; 8; 9]);;

assert (tabulate 5 (add t 3) = [3; 4; 5; 6; 7]);;

assert (tabulate 3 (map s (fun x -> 2*x - 13)) = 
    [(2 * (s 0) - 13); 
    (2 * (s 1) - 13); 
    (2 * (s 2) - 13)]);;

let rec sumn = function 
  | 0 -> 0
  | n -> n + sumn (n-1);;

assert(tabulate 5 (fold (fun x y -> x + y) 0 s) =
    [sumn 1;
    sumn 2;
    sumn 3;
    sumn 4;
    sumn 5]);;

let rec silnia_rek = function
  | 1 -> 1
  | n -> n * silnia_rek (n-1);;

assert(tabulate 5 (fold (fun x y -> x * y) 1 s) =
    [silnia_rek 1;
    silnia_rek 2;
    silnia_rek 3;
    silnia_rek 4;
    silnia_rek 5]);;


let silnia n = (fold ( * ) 1 s) (n-1);;

assert (silnia 12 = silnia_rek 12);;
