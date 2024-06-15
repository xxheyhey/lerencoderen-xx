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

(* sum_cube_odd (pipeline) *)

let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let sum = List.fold_left ( + ) 0

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 <> 0)
  |> List.map (fun x -> x * x)
  |> sum

(* exists *)

let rec exists_rec p = function
  | [] -> false
  | h :: t -> p h || exists_rec p t

let exists_fold p = List.fold_left (fun acc elt -> acc || p elt) false
let exists_lib p lst = List.length (List.filter p lst) > 0
let exists_lib' = List.exists (* lol *)

(* account balance *)

let new_balance_foldl balance debit_lst =
  balance -. List.fold_left ( +. ) 0. debit_lst

let new_balance_foldr balance =
  List.fold_right ( -. ) balance (* WRONG, see answer below *)

let rec new_balance_rec balance = function
  | [] -> balance
  | h :: t -> new_balance_rec (balance -. h) t

(* answers from textbook *)
let new_balance_foldl' balance = List.fold_left ( -. ) balance

let new_balance_foldr' balance debit_lst =
  List.fold_right (fun d b -> b -. d) debit_lst balance

(* library uncurried *)

let uncurried_append (l0, l1) = List.append l0 l1
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (x, y) = Stdlib.max x y

(* map composition *)

let double_map f g = List.map (fun elt -> f (g elt))

(* or *)
let double_map' f g = List.map (f @@ g)

(*  *)
