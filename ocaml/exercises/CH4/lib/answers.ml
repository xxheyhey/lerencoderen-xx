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

(* more list fun *)

let strings_gt_3 = List.filter (fun x -> String.length x > 3)
let add1_to_floats = List.map (fun x -> x +. 1.)

let join_with sep strings =
  List.fold_left
    (fun acc elt -> if acc = "" then acc ^ elt else acc ^ sep ^ elt)
    "" strings

let x = List.sort

(* association list keys *)

let keys lst =
  let rec keys_aux acc = function
    | [] -> acc
    | (key, _) :: t -> keys_aux (key :: acc) t
  in
  List.sort_uniq Stdlib.compare (keys_aux [] lst)

(* from answers *)
let keys' lst = lst |> List.rev_map fst |> List.sort_uniq Stdlib.compare

(* valid matrix *)

let is_valid_matrix = function
  | [] -> false
  | row :: rows ->
    let row_len = List.length row in
    List.for_all (fun row' -> List.length row' = row_len) rows

(* row vector add *)

let add_row_vectors = List.map2 ( + )

(* matrix add *)

let add_matrices = List.map2 (fun row1 row2 -> add_row_vectors row1 row2)

(* from answers *)
let add_matrices' = List.map2 add_row_vectors

(* matrix multiply *)

(* Test matrices and vectors *)
let matrix1 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
let matrix2 = [ [ 10; 20; 30 ]; [ 40; 50; 60 ]; [ 70; 80; 90 ] ]
let vector1 = [ 5; 5; 5 ]

let rec get_col n = function
  | [] -> []
  | row :: rows ->
    if n >= List.length row then
      failwith "n is out of bounds"
    else
      List.nth row n :: get_col n rows

let transpose matrix =
  let rec transpose_aux i matrix' =
    match matrix' with
    | [] -> []
    | row :: _ ->
      if i >= List.length row then
        []
      else
        get_col i matrix' :: transpose_aux (i + 1) matrix'
  in
  transpose_aux 0 matrix

let multiply_row_vectors = List.map2 ( * )
let dot row1 row2 = List.fold_left ( + ) 0 (multiply_row_vectors row1 row2)

let multiply_matrices matrix1 matrix2 =
  let rec multiply_matrices_aux matrix1' matrix2_transposed acc_row acc =
    match (matrix1', matrix2_transposed) with
    | row_m1 :: rest_m1, m2 -> begin
      match m2 with
      | [] ->
        multiply_matrices_aux rest_m1 (transpose matrix2) [] (acc_row :: acc)
      | row_m2 :: rest_m2 ->
        multiply_matrices_aux matrix1' rest_m2
          (acc_row @ [ dot row_m1 row_m2 ])
          acc
    end
    | [], _ -> List.rev acc
  in
  multiply_matrices_aux matrix1 (transpose matrix2) [] []

(* from answers *)
let transpose' ls =
  let rec transpose'' acc = function
    | [] | [] :: _ -> List.rev acc
    | ls -> transpose'' (List.map List.hd ls :: acc) (List.map List.tl ls)
  in
  transpose'' [] ls

let dot' = List.fold_left2 (fun acc x y -> acc + (x * y)) 0

let multiply_matrices' m1 m2 =
  List.map (fun row -> List.map (dot' row) (transpose' m2)) m1

(* another solution *)

let inner matrix row = List.map (dot' row) (transpose' matrix)
let multiply_matrices'' m1 m2 = List.map (inner m2) m1
