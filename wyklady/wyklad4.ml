(* zadanie 1 *)
type 'a binTree = Node of 'a binTree * 'a * 'a binTree | Empty;;

let rec inorder = function
  | Empty -> []
  | Node(left, value, right) ->
      value :: (inorder left)
;;

let rec postorder t = [];;

assert (inorder (Empty) = []);;
assert (postorder(Empty) = []);;

let simple_tree = Node(Empty, "x", Empty);;

assert (inorder simple_tree = ["x"]);;

(* zadanie 2 *)
let rec mapBinTree f = function
  | Empty -> Empty
  | Node(left, value, right) -> 
      Node((mapBinTree f left), (f value), (mapBinTree f right))
;;

mapBinTree (fun x -> x+1) (Node(Node(Empty, 1, Empty), 12, Node(Empty, 5, Empty)));;

(* zadanie 3 *)

let rec listToSum acc = function
  | [] -> acc
  | h :: t -> listToSum (h + acc) t
;;

let f = mapBinTree (listToSum 0);;

let rec numberToList acc n = match n / 10 with
  | 0 -> n :: acc
  | _ -> numberToList ((n mod 10) :: acc) (n / 10)
;;

let g = mapBinTree (numberToList []);;

assert (g (Node(Empty, 1234, Empty)) = (Node(Empty, [1;2;3;4], Empty)));;

(* zadanie 4 *)
type 'a regBT = RLeaf | RNode of 'a regBT * 'a * 'a regBT;;

let innerPath = 
  let rec aux n = function
    | RLeaf -> 0
    | RNode(l, v, r) -> n + (aux (n + 1) l) + (aux (n + 1) r)
  in aux 0 
;;

let outerPath = 
  let rec aux n = function
    | RLeaf -> n
    | RNode(l, v, r) -> (aux (n + 1) l) + (aux (n + 1) r)
  in aux 0
;;
(* zadanie 5 *)
type 'a mtree = MNode of 'a * 'a forest
  and 'a forest = EmptyForest | Forest of 'a mtree * 'a forest
;;

type 'a mtree_lst = MTree of 'a * ('a mtree_lst) list;;

let dfs1 = function
  | MNode(value, EmptyForest) -> value
  | MNode(value, Forest(t, f)) -> 
;;

let preorder = [];;

(* zadanie 6 *)
type 'a graph = Graph of ('a -> 'a list);;

let find_vertex_with_n_neighbors (Graph succ) v n = 
  let rec find visited = function
    | [] -> None
    | h :: t -> if List.mem h visited then find visited t
                else if (List.length (succ h) >= n) then Some h
                else find (h :: visited) (t @ succ h)
  in find [] [v]
;;

