module type PQUEUE =
sig 
  type priority
  type 'a t

  exception EmptyPQueue

  val empty : 'a t
  val insert : 'a t -> priority -> 'a -> 'a t
  val remove : 'a t -> priority * 'a * 'a t
end

module PQueue : PQUEUE =
struct
	type priority of integer
	type 'a t = Empty | Node of 'a * integer * 'a t

	let empty = Empty
	let insert 
end