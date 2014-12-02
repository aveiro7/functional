(* zadanie 1 *)

let is_palindrome xs = 
  let rec halve acc1 acc2 = function
    | [] -> (acc1, acc2)
    | [a] -> (acc1, List.tl acc2)
    | _::_::t -> halve ((List.hd acc2) :: acc1) (List.tl acc2) t
  in let (reversed_first_half, second_half) = halve [] xs xs
  in reversed_first_half = second_half
;;

assert(is_palindrome [1;2;1]);;
assert(not (is_palindrome [1;2;3]));;

(* zadanie 2 *)
type 'a btree = Leaf | Node of 'a btree * 'a * 'a btree;;

let rec check_balanced t =  
  let rec count_sons = function
    | Leaf -> (0, true)
    | Node(l, _, r) -> let (l, x) = count_sons l in
      if x then
        let (r, y) = count_sons r in
          if (((l + 1 = r) || (l = r+1) || (l=r)) && y) 
          then (l + r + 1, true)
          else (0, false)
      else (0, false)
    in let (a, b) = count_sons t
    in b;;

assert(check_balanced (Node(Node(Node(Leaf, 11, Leaf), 12, Leaf), 10, Leaf)) = false);;
assert(check_balanced (Node(Node(Leaf, 1, Leaf), 1, Node(Leaf, 1, Leaf))) = true);;

let rec gen_balanced n = 
  if n = 0 then Leaf
  else if n = 1 then Node(Leaf, 0, Leaf)
  else Node(gen_balanced ((n-1)/2), 0, gen_balanced (n - (n-1)/2 - 1));;

assert(gen_balanced 2 = Node(Leaf, 0, Node (Leaf, 0, Leaf)));;

let rec rename xs = function
  | Leaf -> (Leaf, xs)
  | Node(l, _, r) -> 
    match xs with
      | [] -> (Leaf, [])
      | (h::t) -> 
        let (left_tree, remaining_list) = rename t l in
        let (right_tree, rest_of_list) = rename remaining_list r in
        (Node(left_tree, h, right_tree), rest_of_list)
;;

let list_to_btree xs =
  let (tree, l) = rename xs (gen_balanced (List.length xs)) in
  tree
;;

assert(list_to_btree [1;2;3] = Node(Node(Leaf, 2, Leaf), 1, Node(Leaf, 3, Leaf)));;

(* zadanie domowe *)
(* zadanie 3 *)

type 'a mtree = MNode of 'a * 'a forest
and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest;;

let get_children_to_queue q forest = 
  let rec get_queue acc = function
    | EmptyForest -> acc
    | Forest(node1, nodes) -> get_queue (node1 :: acc) nodes
  in get_queue q forest
;;

let bfs_ver1 forest1 =
  let rec bfs tovisit queue forest = match (tovisit, queue, forest) with
    | ([], [], MNode(x, EmptyForest)) -> [x]
    | ([], q, MNode(x, f)) -> let (h::t) = List.rev (get_children_to_queue q f) in x :: (bfs t [] h)
    | ((h::t), q, MNode(x, f)) -> x :: (bfs t (get_children_to_queue q f) h)
  in bfs [] [] forest1
;;

let preorder forest = 
  let rec pre1 tovisit = function
    | MNode(x, Forest(tree, forest)) -> 
        x :: (pre1 ((get_children_to_queue [] forest) @ tovisit) tree)
    | MNode(x, EmptyForest) when tovisit = [] -> [x]
    | MNode(x, EmptyForest) -> let (h::t) = tovisit in x :: (pre1 t h)
  in pre1 [] forest
;;

let wielokierunkowe_drzewo = (MNode(1, Forest(MNode(5, Forest(MNode(2, EmptyForest), EmptyForest)), Forest(MNode(3, EmptyForest), EmptyForest))));;

assert(bfs_ver1 wielokierunkowe_drzewo = [1;5;3;2]);;
assert(preorder wielokierunkowe_drzewo = [1;5;2;3]);;

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let children_to_queue q forest =
  let rec get_queue2 acc = function
    | [] -> acc
    | (h::t) -> get_queue2 (h::acc) t
  in get_queue2 q forest
;;

let bfs_ver2 forest2 = 
  let rec bfs2 tovisit queue forest = match (tovisit, queue, forest) with
    | ([], [], MTree(x, [])) -> [x]
    | ([], q, MTree(x, l)) -> let (h::t) = List.rev (children_to_queue q l) in 
        x :: (bfs2 t [] h)
    | ((h::t), q, MTree(x, l)) -> x :: (bfs2 t (children_to_queue q l) h)
  in bfs2 [] [] forest2
;;

let preorder2 forest = 
  let rec pre2 tovisit = function
    | MTree(x, (h::t)) -> x :: (pre2 (t @ tovisit) h)
    | MTree(x, []) when tovisit = [] -> [x]
    | MTree(x, []) -> let (h::t) = tovisit in x :: (pre2 t h)
  in pre2 [] forest
;;

let wielokierunkowe_drzewo2 = MTree(1, [MTree(5, [MTree(2, [])]); MTree(3, [])]);;

assert(bfs_ver2 wielokierunkowe_drzewo2 = [1;5;3;2]);;
assert(preorder2 wielokierunkowe_drzewo2 = [1;5;2;3]);;

(* zadanie 4 *)
type formula = 
    Neg of formula 
  | Con of formula * formula 
  | Alt of formula * formula 
  | Var of string 
  | Value of bool 
;;

(* 4.1 *)
let rec uniq = function
  | [x] -> [x]
  | x1 :: x2 :: xs -> if x1 = x2 then uniq (x1 :: xs) else x1 :: (uniq (x2 :: xs))
  | [] -> []
;;

let sort_uniq xs = uniq (List.sort compare xs);;

assert(sort_uniq [1; 3; 5; 4; 2; 1; 1; 5; 5; 3; 1; 4] = [1;2;3;4;5]);;

let rec get_variables = function
  | Neg(form) -> get_variables form
  | Con(form1, form2) | Alt(form1, form2) -> 
      sort_uniq ((get_variables form1) @ (get_variables form2))
  | Var(name) -> [name]
  | _ -> []
;;

let rec generate_next_values = function
  | (name, true) :: xs -> (name, false) :: (generate_next_values xs)
  | (name, false) :: xs -> (name, true) :: xs
  | [] -> []
;;

let rec is_last = function
  | true :: xs -> is_last xs
  | false :: xs -> false
  | [] -> true
;;

let rec evaluate vals = function
  | Neg(form) -> not (evaluate vals form)
  | Con(form1, form2) -> (evaluate vals form1) && (evaluate vals form2)
  | Alt(form1, form2) -> (evaluate vals form1) || (evaluate vals form2)
  | Var(name) -> snd (List.find (fun (name1, value) -> name1 = name) vals)
  | Value(x) -> x
;;

let is_tautology form = 
  let vars = get_variables form in
  let first_vals = List.map (fun v -> (v, false)) vars in
  let rec aux vals = 
    if evaluate vals form = false then (false, vals)
    else if is_last (List.map snd vals) then (true, [])
    else aux (generate_next_values vals)
  in aux first_vals
;;

let impl f1 f2 = Alt(Neg(f1), f2);;
(* !(p ^ q) => !p v !q *)
assert(fst (is_tautology 
  (impl (Neg(Con(Var("p"), Var("q")))) (Alt(Neg(Var("p")), Neg(Var("q")))))
  ));; 

(* 4.2 *)
let rec negate = function
  | Neg(form) -> form
  | Alt(form1, form2) -> Con(negate(form1), negate(form2))
  | Con(form1, form2) -> Alt(negate(form1), negate(form2))
  | Var(name) -> Neg(Var(name))
  | Value(x) -> Value(not x)
;;

let rec transform_to_nnf = function
  | Neg(Var(name)) -> Neg(Var(name))
  | Neg(form) -> transform_to_nnf(negate(form)) 
  | Alt(form1, form2) -> Alt(transform_to_nnf(form1), transform_to_nnf(form2))
  | Con(form1, form2) -> Con(transform_to_nnf(form1), transform_to_nnf(form2))
  | other -> other
;;

(* !(p ^ q) <=> !p v !q *)
assert (transform_to_nnf(Neg(Con(Var("p"), Var("q")))) = 
    Alt(Neg(Var("p")), Neg(Var("q"))));;



(* 4.4 *)
let rec is_in_clause form = function
  | Alt(form1, form2) when form1 = form -> true
  | Alt(form1, form2) -> is_in_clause form form2
  | _ -> false
;;

let is_good_clause = function
  | Alt(Value(true), form) -> true
  | Alt(Var(name), form) -> is_in_clause (Neg(Var(name))) form
  | Alt(Neg(form1), form2) -> is_in_clause form1 form2
  | Value(true) -> true
  | _ -> false
;;

let rec is_tautology_cnf = function
  | Con(form1, form2) when is_good_clause form1 -> is_tautology_cnf form2
  | Con(form1, form2) -> false
  | form -> is_good_clause form
;;

(* (true v p) *)
assert(is_tautology_cnf (Alt(Value(true), Var("p"))));;
(* p ^ q *)
assert(not(is_tautology_cnf( Con(Var("p"), Var("q")))));;

(* 4.6 *)
let rec is_in_dual_clause form = function
  | Con(form1, form2) when form1 = form -> true
  | Con(form1, form2) -> is_in_dual_clause form form2
  | _ -> false
;;
  
let is_wrong_dual_clause = function
  | Con(Value(false), form) -> true
  | Con(Var(name), form) -> is_in_dual_clause (Neg(Var(name))) form
  | Con(Neg(form1), form2) -> is_in_dual_clause form1 form2
  | Value(false) -> true
  | _ -> false
;;  
  
let rec is_contradictory = function
  | Alt(form1, form2) when is_wrong_dual_clause form1 -> is_contradictory form2
  | Alt(form1, form2) -> false
  | form -> not (is_wrong_dual_clause form)
;;

(* false v (q ^ !q) *)
assert(is_contradictory (Alt(Value(false), Con(Var("q"),Neg(Var("q"))))));;
(* (p ^ !p) v (true) *)
assert(not(is_contradictory (Alt(Con(Var("p"), Neg(Var("p"))), Value(true)))));;

(* zadanie 5*)
let prod t = 
  let rec prod_cps k = function
    | Leaf -> k 1
    | Node (_, 0, _) -> 0
    | Node (l, a, r) -> prod_cps (fun x -> prod_cps (fun y -> k (x * a * y)) r) l
  in prod_cps (fun v -> v) t;;

assert(prod (Node(Node(Node(Node(Leaf, 2, Leaf), 3, Node(Leaf, 3, Node(Leaf, 12, Leaf))), 4,
Leaf),5,Leaf)) = 2 * 3 * 3 * 4 * 5 * 12);;

