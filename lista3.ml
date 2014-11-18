(* zadanie 1 *)
let wielomian = [1.; 0.; -1.; 2.];;

let horner x l =
  let rec horner_tail acc = function
    | [] -> acc
    | h::t -> horner_tail (x *. acc +. h) t
  in horner_tail 0. l;;

let compute x = horner x wielomian;;

compute 0.;;
compute 1.;;
compute 4.;;


let horner_nontail x l = List.fold_left (fun a b -> x *. a +. b) 0. l;;


horner_nontail 0. wielomian;;
horner_nontail 1. wielomian;;
horner_nontail 4. wielomian;;

(* zadanie 2 *)

let rec horner2 x = function
  | [] -> 0.
  | h::t -> h +. x *. (horner2 x t);;

horner2 0. wielomian;;
horner2 1. wielomian;;
horner2 4. wielomian;;

let horner2_nontail x l = List.fold_right (fun a b -> x *. b +. a) l 0.;;

horner2_nontail 0. wielomian;;
horner2_nontail 1. wielomian;;
horner2_nontail 4. wielomian;;

(* zadanie 3 *)

let deriv l =
  let rec deriv_tail acc count = function
    | [] -> List.rev acc
    | h::t -> deriv_tail ((count *. h)::acc) (count +. 1.) t
  in deriv_tail [] 1. (List.tl l);;

let deriv_nontail l = List.rev (List.tl 
  (List.fold_left
    (fun acc x -> (List.hd acc +. 1.) :: ((x *. (List.hd acc)) ::  (List.tl acc)))
  [1.] (List.tl l)));; 

deriv_nontail [1.];;
deriv_nontail [1.; 0.];;
deriv_nontail [1.;0.;-1.];;
deriv_nontail wielomian;;

deriv wielomian;;
