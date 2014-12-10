module type SetSig = sig
  type 'a set
  val empty : 'a set
  val add : 'a -> 'a set -> 'a set
  val mem : 'a -> 'a set -> bool
end;;

module Set : SetSig = struct
  type 'a set = 'a list
  let empty = []
  let add x l = x :: l
  let mem x l = List.mem x l
end
;;

(* zadanie 1 *)
(* 1.1 *)
module A : sig 
  val x : string
end = struct
  let x = 1
  let x = "x"
end
;;
(* it is compilable, but I guess should be illegal -> x has type string,
 * as the signature says, and we try to define it as integer *)

(* 1.2 *)
module A : sig
  val x : string
  val x : string
end = struct
  let x = "x"
end
;;
(* it's also compilable, but I guess the repeated signature is nothing good *)

(* 1.3 *)
(* module a = struct
  let x = 1
end;;
  syntax error: module names should begin with uppercase
  *)

(* 1.4 *)
module M : sig 
  val f : int -> int
  val g : string -> string
end = struct
  let g x = x
  let f x = x
end;;

(* 1.5 *)
let module X = struct let x = 1 end in X.x;;

(* 1. 6 *)
(* error: unbound value h
module M = struct
  let g x = h x
  let f x = g x
  let h x = x + 1
end *)

(* 1.7 *)
module rec M : sig
  val f : int -> int
  val h : int -> int
end = struct 
  open M 
  let g x = h x
  let f x = g x
  let h x = x + 1
end;;


(* 1.8 *)
module rec M : sig
  val f : int -> int
end = struct
  let f = M.f
end;;

(* 1.9 *)
type 'a t = {set : 'a -> unit; get : unit -> 'a}
let f x =
  let cell = ref x in
  let module M = struct
    let s i = cell := i
    let g () = !cell
    let r = {set = s; get = g}
  end
    in
    M.r
;;

(* 1.10 *)
(* expression uses type out of scope [type 'a t is only valid in module M scope]
let f x =
  let cell = ref x in
  let module M = struct 
    type 'a t = {set : 'a -> unit; get : unit -> 'a}
    let s i = cell := i
    let g () = !cell
    let r = {set = s; get = g}
  end
    in
    M.r
;; *)

(* 1.11 *)
(* type mismatch: we give unit to A.f, and it expects int
module type ASig = sig type s val f : int -> s end
module type BSig = sig type t val g : t -> int end
module C : sig
  module A : ASig
  module B : BSig with type t = A.s
end = struct
  type u = string
  module A = struct type s = u let f = string_of_int end
  module B = struct type t = u let g = int_of_string end
end  
include C
let i = B.g (A.f ())
;; *)

(* 1.12 *)
module type ASig = sig 
  type t 
end
module type BSig = sig 
  val x : int 
end
module A : ASig 
with type t = int = struct 
  type t = int 
end

module B : BSig = struct
  let x = 1 
end
module C : sig
  include ASig
  val x : t
end = struct
  include A
  include B
end



(* zadanie 2 *)
(*
 * like it's on 1.7
 * module rec M : sig
   * signatures of all the functions etc
 * end = struct
   * open M
   * definitions.
 * end
 *)

(* zadanie 4 *)
let f x = x;;

(* zadanie 5 *)
(* wtf? xD *)

