type 'a queue = ('a list * 'a list) ref;;

let create () = ref ([], []);;

let add queue x =
  let (front, back) = !queue in
    queue := (x::front, back)
;;

let rec take queue = match !queue with
  | (front, x::back) -> 
      queue := (front, back);
      x
  | ([], []) -> 
      raise (Invalid_argument "queue is empty")
  | (front, []) -> 
      queue := ([], List.rev front);
      take queue
;;


type 'a elem =
  | Nil
  | Elem of 'a * 'a elem ref * 'a elem ref
;;

let nil_elem = Nil;;
let create_elem x = Elem(x, ref Nil, ref Nil);;

let get = function
  | Elem (x, _, _) -> x
  | Nil -> raise (Invalid_argument "get")
;;

let prev_elem = function
  | Elem (_, prev, _) -> !prev
  | Nil -> raise (Invalid_argument "prev_elem")
;;

type 'a dllist = 'a elem ref;;

let create () = ref Nil;;

let insert list elem = match (elem, !list) with
  | Elem (_, prev, next), Nil -> 
      prev := Nil;
      next := Nil;
      list := elem
  | Elem (_, prev1, next1), (Elem(_, prev2, next2) as head) ->
      prev1 := Nil;
      next1 := head;
      prev2 := elem;
      list := elem
  | Nil, _ -> raise (Invalid_argument "insert")
;;


let remove list elem = match elem with
  | Elem(_, prev, next) ->
      (match !prev with
        | Elem (_, _, prev_next) -> 
            prev_next := !next
        | Nil -> 
            list := !next);
      (match !next with
        | Elem (_, next_prev, _) ->
            next_prev := !prev
        | Nil -> ())
  | Nil -> raise (Invalid_argument "remove")
;;


let memo f = 
  let table = ref [] in
  let rec find_or_apply entries x =
    match entries with
      | (x', y) :: _ when x' = x -> y
      | _ :: entries -> find_or_apply entries x
      | [] -> 
          let y = f x in
          table := (x, y) :: !table;
          y
  in 
  (fun x -> find_or_apply !table x)
;;

let rec fib = function
  | 0|1 as i -> i
  | i -> fib(i - 1) + fib(i - 2)
;;


let time f x = 
  let start = Sys.time () in
  let y = f x in
  let finish = Sys.time () in
  Printf.printf "Elapsed time: %f seconds\n" (finish -. start);
  y
;;

let memo_fib = memo fib;;

time memo_fib 40;;

time memo_fib 40;;

