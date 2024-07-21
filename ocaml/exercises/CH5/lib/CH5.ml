(* complex synonym *)

module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end

(* complex encapsulation *)

module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

(* big list queue *)

(** This is a nice OCamldoc comment *)
module ListQueue = struct
  let empty = []
  let enqueue x q = x :: q
end

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then
      q
    else
      loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty
