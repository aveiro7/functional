(* zadanie 1 *)
type 'a btree = Leaf of 'a | Node of 'a btree * 'a btree;;

let checkfringe t1 t2 = 
  let rec check_aux tree1 tree2 tovis1 tovis2 = match (tree1, tree2) with
    | (Leaf(x), Leaf(y)) when x = y -> 
        if (tovis1 = [] && tovis2 = []) then true
        else if (tovis1 = [] || tovis2 = []) then false
        else let (h1::t1, h2::t2) = (tovis1, tovis2) in
          check_aux h1 h2 t1 t2
    | (Leaf(x), Leaf(y)) -> false
    | (Leaf(x), Node(l, r)) -> check_aux tree1 l tovis1 (r::tovis2)
    | (Node(l, r), Leaf(x)) -> check_aux l tree2 (r::tovis1) tovis2
    | (Node(l1, r1), Node(l2, r2)) -> check_aux l1 l2 (r1::tovis1) (r2::tovis2)
  in check_aux t1 t2 [] []
;;

assert (checkfringe 
  (Node(Node(Leaf 1, Leaf 2), Leaf 3)) (Node(Leaf 1, (Node(Leaf 2, Leaf 3)))) 
    = true);;

(* w zadaniu chodziło o to, zeby pobrac brzegi a potem sprawdzac rownosc dwoch list. Bez patrzenia na efektywnosc: po prostu zwykle listy, z efektywnoscia: listy leniwe *)

(* zadanie 2 *)
type 'a btree = Leaf of 'a | Node of 'a btree * 'a * 'a btree;;


(* ta funkcja zupelnie nie dziala! *)

let numerate_preorder t = 
  let rec aux t n k = match t with
    | Leaf(_) -> k (Leaf(n)) (n + 1)
    | Node(left, _, right) -> 
        aux left (n + 1) (fun x m -> Node(x, n, (aux right m k)))
  in aux t 1 (fun x _ -> x)
;;

numerate_preorder (Node(Node(Leaf 'a', 'b', Leaf 'c'), 'd', Leaf 'e'));;

let t1 = Node(Leaf 'x', 'x', Leaf 'x');;

let t2 = Node(t1, 'x', t1);;

numerate_preorder t2;;
failwith "x";;

(* let numerate_bfs t = 
  let rec aux bts n =
    let new_roots = List.fold_left 
      (fun (acc, m) bt -> match bt with
        | Leaf(_) -> (Leaf(m) :: acc), (m + 1)
        | Node(left, _, right) -> (Node(left, m, right)), (m + 1)) 
      ([], n) bts in
    let to_traverse = List.fold_left
      (fun acc bt -> match bt with
        | Leaf(_) -> None :: acc
        | Node(left, _, right) -> left :: right :: acc)
      [] new_roots in
    let traversed = List.fold_left
      (fun acc bt -> )
      acc to_traverse in
    List.fold_left (fun ) acc traversed
  in aux [t] 1
;; *)
 
(* fold_lefty!!!
 * wziac wszystkie korzenie w lesie
 * przenumerowac je
 * wziac wszystkich synow
 * przenumerowac synow (w zasadzie poddrzewa)
 * podpiac synow pod korzenie
 * a to wszystko dla pojedynczego korzenia *)

(* zadanie 3 *)

(* Ważne: jesli rozwazamy kontynuacje, trzeba ja jakos przekazac wyzej [a w zasadzie nizej]. Inaczej to wszystko jest bez sensu. *)


type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

type 'a array = Array of int * 'a btree;;

(* pusta tablica *)
let aempty = Array(0, Leaf);;

(* pobranie skladowej o indesie n *)
let asub a n =
  let rec aux bt n = 
    let Node(left, value, right) = bt in
      if n = 1 then value
      else if n mod 2 = 1 then aux right (n / 2)
      else aux left (n / 2)
  in
  let Array(fst_a, snd_a) = a in
  if n > (fst_a) then failwith "Out of bound exception"
  else aux (snd_a) n  
;;

let aupdate a n x = 
  let rec aux bt n k = 
    let (Node(left, value, right)) = bt in
      if n = 1 then k (Node(left, x, right))
      else if n mod 2 = 1 then 
        aux right (n / 2) (fun new_right -> k (Node(left, value, new_right)))
      else
        aux left (n / 2) (fun new_left -> k (Node(new_left, value, right)))
  in 
  let Array(fst_a, snd_a) = a in 
  if n > (fst_a) then failwith "Out of bound exception"
  else Array((fst_a), (aux (snd_a) n (fun x -> x)))
;;

let ahiext a x = 
  let rec aux bt n k =
    let (Node(left, value, right)) = bt in
      if n = 2 then k (Node(Node(Leaf, x, Leaf), value, right))
      else if n = 3 then k (Node(left, value, Node(Leaf, x, Leaf)))
      else if n mod 2 = 1 then
        aux right (n / 2) (fun new_right -> k (Node(left, value, new_right)))
      else
        aux left (n / 2) (fun new_left -> k (Node(new_left, value, right)))
  in 
  let Array(fst_a, snd_a) = a in 
  if (fst_a) = 0 then Array(1, (Node(Leaf, x, Leaf)))
  else Array((fst_a + 1), (aux (snd_a) (fst_a + 1) (fun x -> x)))
;;

let ahirem a = 
  let rec aux bt n k =
    let (Node(left, value, right)) = bt in
      if n = 1 then k (Leaf)
      else if n mod 2 = 1 then
        aux right (n / 2) (fun new_right -> k (Node(left, value, new_right)))
      else
        aux left (n / 2) (fun new_left -> k (Node(new_left, value, right)))
  in
  let Array(fst_a, snd_a) = a in 
  if (fst_a) = 0 then failwith "Empty table!"
  else Array((fst_a - 1), (aux (snd_a) (fst_a) (fun x -> x)))
;;


let t1 = aempty;;
assert(t1 = Array(0, Leaf));;
let t1 = ahiext t1 "a";;
assert(t1 = Array(1, Node(Leaf, "a", Leaf)));;
let t1 = ahiext t1 "b";;
assert(t1 = Array(2, Node(Node(Leaf, "b", Leaf), "a", Leaf)));;
let t1 = ahiext t1 "c";;
assert(t1 = Array(3, Node(Node(Leaf, "b", Leaf), "a", Node(Leaf, "c", Leaf))));;
t1;;
assert(asub t1 2 = "b");;
let t1 = ahirem t1;;
assert(t1 = Array(2, Node(Node(Leaf, "b", Leaf), "a", Leaf)));;
assert(asub t1 2 = "b");;
let t1 = ahirem t1;;
assert(t1 = Array(1, Node(Leaf, "a", Leaf)));;
let t1 = ahirem t1;;
assert(t1 = Array(0, Leaf));;

(* zadanie 5 *)

(* prolog.ml *)

(* An atom is either a propositional variable or an alternative of two goals. *)
type atom = 
  | Atom of string 
  | Or of goal * goal
(* A goal is a list of atoms. *)
and goal = atom list;;
(* A clause consists of a head (a propositional variable) and a body (a goal). *)
type clause = string * goal;;
(* A Prolog program is a list of clauses. *)
type program = clause list;;

(* Search a program for a clause with a given head. *)    
let rec lookup x pgm =
  match pgm with
    | [] ->
      None
    | (y, g) :: p ->
      if x = y then Some g else lookup x p
;;

(* 
A propositional Prolog interpreter written in CPS with two layers of continuations: 
a success and a failure continuation. The failure continuation is parameterless and 
it specifies what should happen next in case of a failure in the current goal. The 
success continuation takes a failure continuation as an argument and it specifies 
what should happen next in case the current goal is satisfied. 
*)
  
(*      eval_atom : atom -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a *)
let rec eval_atom a p sc fc =
  match a with
    | Atom x ->
      (match (lookup x p) with
  | None -> 
    fc ()
  | Some g -> 
    eval_goal g p sc fc)
    | Or (g1, g2) ->
      eval_goal g1 p sc (fun () -> eval_goal g2 p sc fc)

(*  eval_goal : goal -> program -> ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a  *)
and eval_goal g p sc fc =
  match g with
    | [] -> 
      sc fc
    | a :: g -> 
      eval_atom a p (fun fc' -> eval_goal g p sc fc') fc
;;

(*  run : goal ->  program -> bool  *)
let run g p = eval_goal g p (fun _ -> true) (fun () -> false);;

(* tests *)
  
let p1 = [("a", [Atom "b"; Atom "c"]);
    ("b", [])]
;;
  
let p2 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
    ("b", [Atom "d"]);
    ("d", []);
    ("e", [Atom "d"])]
;;

let p3 = [("a", [Atom "b"; Or ([Atom "c"], [Atom "d"]); Atom "e"]);
    ("b", [Atom "d"]);
    ("c", []);
    ("d", []);
    ("e", [Atom "d"])]
;;
  
let g1 = [Atom "a"];;

let v1_1 = run g1 p1;;
let v1_2 = run g1 p2;;
let v1_3 = run g1 p3;;

(* eof *)

type regexp =
  | Atom of char
  | And of regexp * regexp 
  | Or of regexp * regexp
  | Star of regexp
;;

let match_regexp regexp clist f k = 1;;
let run regexp clist = true;;
