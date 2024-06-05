(* repeat *)

let rec repeat f n x =
  match n with
  | 0 -> x
  | _ -> repeat f (n - 1) (f x)

(* product *)

let product_left = List.fold_left ( *. ) 1.0
let product_right lst = List.fold_right ( *. ) lst 1.0

(* terse product *)
let product_right' = ListLabels.fold_right ~f:( *. ) ~init:1.0

(* COMMENT *)
