(* zadanie 1 *)
let rec merge_rek cmp l1 l2 = match (l1, l2) with
  | ([], []) -> []
  | (h1::t1, h2::t2) when cmp h1 h2 -> h1 :: (merge_rek cmp t1 (h2::t2)) 
  | (h::t, []) -> h::t
  | (a, b) -> merge_rek cmp b a;;

let merge cmp l1 l2 =
  let rec merge_tail acc cmp l1 l2 = match (l1, l2) with
    | ([],[]) -> acc
    | (h::t, []) -> merge_tail (h::acc) cmp t []
    | (h1::t1, h2::t2) when cmp h1 h2 -> merge_tail (h1::acc) cmp t1 (h2::t2)
    | (a, b) -> merge_tail acc cmp b a
  in List.rev (merge_tail [] cmp l1 l2);;
  
(* mergesort w O(n^2) *)
let mergesort1 l cmp = 
  let rec mergesort_tail acc1 acc2 = function
    |[] -> merge cmp  acc1 acc2
    |[x] -> merge cmp (merge cmp [x] acc1) acc2
    |h1::(h2::t) -> mergesort_tail (merge cmp acc1 [h1]) (merge cmp acc2 [h2]) t
  in mergesort_tail [] [] l;; 

let halve l = 
  let rec halve_tail acc1 acc2 = function
    | [] -> (acc2, acc1)
    | [x] -> (x::acc2, acc1)
    | h1::(h2::t) -> halve_tail (h1::acc1) (h2::acc2) t
  in halve_tail [] [] (List.rev l);;

(* mergesort w O(nlogn) bez rekurencji ogonowej *)
let rec mergesort cmp l =
  if List.length l <= 1 then l
  else begin
    let (l, r) = halve l in
    let (l, r) = (mergesort cmp l, mergesort cmp r) in
    merge cmp l r
  end;;
  
(* mergesort w O(nlogn) z rekurencja ogonowa ktory nie do konca dziala *)
let mergesort2 cmp l =
  let rec msort_tail merging_mode tosplit tomerge = match (tosplit,
  merging_mode) with
    | ([h]::t, false) when List.length (List.hd tomerge) == 1 -> 
        msort_tail true t ((merge cmp [h] (List.hd tomerge)) :: (List.tl  tomerge))
    | (h::t, false) -> 
        let (l, r) = halve h in msort_tail false (l::(r::t)) tomerge
    | ([], false) ->
        List.hd tomerge 
    | (x, true) when List.length tomerge == 1 -> 
        msort_tail false x tomerge
    | (x, true) -> let h1 = List.hd tomerge and h2 = (List.hd (List.tl tomerge))
  and t = List.tl (List.tl tomerge) in
    if (List.length h1 == List.length h2) then msort_tail true x ((merge cmp h1
    h2) :: t)
    else msort_tail false x tomerge
  in msort_tail false [l] [];;

(* mergesort2 (<=) [5; 4; 3; 2; 1];; *)

let t x y = true;;
let f x y = false;;
  
assert (merge_rek t [] [] = merge t [] []);;
assert (merge_rek f [] [] = merge f [] []);;
assert (merge_rek (<=) [1; 3; 7] [2; 4; 6] = merge (<=) [1; 3; 7] [2; 4; 6]);;
assert (merge t [] [] = []);;
assert (merge t [] [1] = [1]);;
assert (merge t [1] [] = [1]);;
assert (merge f [] [] = []);;
assert (merge f [] [1] = [1]);;
assert (merge f [1] [] = [1]);;
assert (merge (<=) [1; 2; 5] [3; 4; 5] = [1; 2; 3; 4; 5; 5]);;
assert (merge (>=) [5; 2; 1] [5; 4; 3] = [5; 5; 4; 3; 2; 1]);;
  
assert (mergesort (<=) [] = []);;
assert (mergesort (<=) [1] = [1]);;
assert (mergesort (<=) [1; 2; 3; 4] = [1; 2; 3; 4]);;
assert (mergesort (<=) [4; 3; 2; 1] = [1; 2; 3; 4]);;
assert (mergesort (<=) [1; 2; 3; 4; 5] = [1; 2; 3; 4; 5]);;
assert (mergesort (<=) [5; 4; 3; 2; 1] = [1; 2; 3; 4; 5]);;
assert (mergesort (<=) [3; 1; 4; 5; 2] = [1; 2; 3; 4; 5]);;
assert (mergesort (<=) [3; 1; 3; 4; 2; 5] = [1; 2; 3; 3; 4; 5]);;

(* zadanie 2 *) 

let split l =
  let rec split_tail acc = function
    | [] -> [(acc, [])]
    | h::t -> (acc, (h::t)) :: split_tail (acc @ [h]) t
  in split_tail [] l;; 

assert (split [] = [([],[])]);;
assert (split [1] = [([],[1]); ([1],[])]);;
assert (split [1; 2] = [([], [1;2]);([1],[2]);([1;2], [])]);;

let insert x l =  
  let rec insert1 x = function
    | [] -> []
    | (h1, h2)::t -> (h1 @ ( x :: h2)) :: (insert1 x t)
  in insert1 x (split l);;
  
assert (insert 1 [] = [[1]]);;
assert (insert 1 [2] = [[1; 2]; [2; 1]]);;
assert (insert 5 [1; 2; 3] = [[5; 1; 2; 3]; [1; 5; 2 ;3]; [1; 2; 5; 3]; [1; 2; 3; 5]]);;
  
let rec insert2 x = function
  | [] -> []
  | h::t -> (insert x h) @ insert2 x t;;

assert (insert2 1 [] = []);;
assert (insert2 1 [[]] = [[1]]);;
assert (insert2 5 [[1; 2]; [1]] = [[5; 1; 2]; [1; 5; 2]; [1; 2; 5]; [5; 1]; [1; 5]]);;
  
let rec perm = function
  | [] -> [[]]
  | h::t -> insert2 h (perm t);;

assert (perm [] = [[]]);;
assert (perm [1] = [[1]]);;
assert (perm [1;2] = [[1; 2]; [2; 1]]);;  


(*zadanie 3 *)   
let rec suffix = function
  | [] -> []
  | h::t -> (h::t) :: suffix t;;
  
let prefix l =  
  let rec prefix_acc acc = function
    | [] -> []
    | h::t -> (List.rev (h::acc)) :: prefix_acc (h::acc) t
  in prefix_acc [] l;;
  
assert (suffix [] = []);;
assert (suffix [1] = [[1]]);;
assert (suffix [1; 2; 3; 4; 5] = [[1; 2; 3; 4; 5]; [2; 3; 4; 5]; [3; 4; 5]; [4; 5]; [5]]);;

assert (prefix [] = []);;
assert (prefix [1] = [[1]]);;
assert (prefix [1; 2; 3; 4; 5] = [[1]; [1; 2]; [1; 2; 3]; [1; 2; 3; 4]; [1; 2; 3; 4; 5]]);;

assert (prefix [] = suffix []);;
assert (prefix [1] = suffix [1]);;
