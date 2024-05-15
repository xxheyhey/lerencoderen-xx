(* list expressions *)
let ints = [ 1; 2; 3; 4; 5 ]
let ints2 = [ 1; 2; 3; 4; 5 ] (* Autoformatted to the wrong answer *)
let ints3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* product *)
let rec product lst =
  match lst with
  | [] -> 1
  | h :: t -> h * product t

(* concat *)
let rec concat lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ concat t

(* product test *)
let () = assert (product [] = 1)
let () = assert (product [ 1; 2; 3; 4; 5 ] = 120)

(* patterns *)
let starts_with_bigred lst =
  match lst with
  | h :: _ ->
      if h = "bigred" then
        true
      else
        false
  | _ -> false

(* Better answer: *)
let starts_with_bigred' lst =
  match lst with
  | h :: _ -> h = "bigred"
  | _ -> false

let has_two_or_four_elements lst_in =
  let rec get_length lst =
    match lst with
    | [] -> 0
    | _ :: t -> 1 + get_length t
  in
  let length = get_length lst_in in
  if length = 2 || length = 4 then
    true
  else
    false

(* Better answer: *)
let has_two_or_four_elements' lst =
  match lst with
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let first_two_elements_are_equal lst =
  match lst with
  | h :: t :: _ -> h = t
  | _ -> false

(* library *)

(* library:fifth *)
let fifth (lst : int list) =
  if List.length lst < 5 then
    0
  else
    List.nth lst 4

(* library:sort *)
let sort (lst : int list) = List.rev (List.sort Stdlib.compare lst)

(* Better: *)
let sort' (lst : int list) = lst |> List.sort Stdlib.compare |> List.rev

(* library puzzle *)
let last_element lst = List.nth (List.rev lst) 0
let any_zeroes lst = List.exists (fun x -> x = 0) lst

(* take drop *)
let rec take n lst =
  if n = 0 then
    []
  else
    match lst with
    | [] -> []
    | h :: t -> h :: take (n - 1) t

let rec drop n lst =
  if n = 0 then
    lst
  else
    match lst with
    | [] -> []
    | _ :: t -> drop (n - 1) t

(* take drop tail *)

let rec take_aux n lst acc =
  if n = 0 then
    acc
  else
    match lst with
    | [] -> acc
    | h :: t -> take_aux (n - 1) t (acc @ [ h ])

let take_tr n lst = take_aux n lst []

(* Taken from the list section in the book: *)

(** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)
let rec from i j l =
  if i > j then
    l
  else
    from i (j - 1) (j :: l)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
let ( -- ) i j = from i j []

(* unimodal *)
let rec is_unimodal_helper x = function
  | [] -> true
  | h :: t ->
      if t = [] then
        true
      else if h > x then
        is_unimodal_helper h t
      else
        false

let is_unimodal lst = is_unimodal_helper 0 lst

(* Correct answer from book: *)

(** returns: whether the input list is monotonically decreasing *)
let rec is_mon_dec = function
  | [] | [ _ ] -> true
  | h1 :: (h2 :: _ as t) -> h1 >= h2 && is_mon_dec t

(** returns: whether the input list is monotonically increasing
    then monotonically decreasing *)
let rec is_mon_inc_then_dec = function
  | [] | [ _ ] -> true
  | h1 :: (h2 :: _ as t) as lst ->
      if h1 <= h2 then
        is_mon_inc_then_dec t
      else
        is_mon_dec lst

let is_unimodal' lst = is_mon_inc_then_dec lst

(* powerset *)
let rec powerset_aux acc = function
  | [] -> acc
  | h :: t ->
      let subsets_with_h = List.map (fun set -> h :: set) acc in
      let subsets_without_h = acc in
      let acc' = subsets_with_h @ subsets_without_h in
      powerset_aux acc' t

let powerset lst = powerset_aux [ [] ] lst

(* From answers: *)
let rec powerset' = function
  | [] -> [ [] ]
  | h :: t ->
      let p = powerset' t in
      List.map (List.cons h) p @ p

(* print int list rec *)
let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      h |> string_of_int |> print_endline;
      print_int_list t

(* print int list rec *)
let print_int_list' lst =
  List.iter (fun x -> x |> string_of_int |> print_endline) lst

(* student *)
type student = { first_name : string; last_name : string; gpa : float }

let mark = { first_name = "Mark"; last_name = "Fokr"; gpa = 21. }

let full_name = function
  | { first_name; last_name; _ } -> (first_name, last_name)

let create_student first_name last_name gpa = { first_name; last_name; gpa }

(* pokerecord *)
type ptype = TNormal | TFire | TWater
type pokemon = { name : string; hp : int; ptype : ptype }

let charizard = { name = "Charizard"; hp = 78; ptype = TFire }
let squirtle = { name = "Squirtle"; hp = 44; ptype = TWater }

(* safe hd and tl *)
let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

(* pokefun *)
let rec max_hp = function
  | [] -> None
  | h :: t -> begin
      match max_hp t with
      | None -> Some h
      | Some m ->
          Some
            (if h.hp >= m.hp then
               h
             else
               m)
    end

(* date before *)
type date = int * int * int

let is_before (date1 : date) (date2 : date) =
  match (date1, date2) with
  | (year1, month1, day1), (year2, month2, day2) ->
      if year1 < year2 then
        true
      else if month1 < month2 then
        true
      else if day1 < day2 then
        true
      else
        false

(* Better answer: *)
let is_before' (date1 : date) (date2 : date) =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  y1 < y2 || (y1 == y2 && m1 < m2) || (y1 == y2 && m1 == m2 && d1 < d2)

(* earliest date *)
let rec earliest = function
  | [] -> None
  | date1 :: t -> (
      match earliest t with
      | None -> Some date1
      | Some date ->
          if is_before date1 date then
            Some date1
          else
            Some date)

(* Test dates: *)
let date1 = (2013, 2, 1)
let date2 = (2014, 3, 29)
let date3 = (2013, 1, 30)
let date4 = (2024, 8, 31)
let test_dates = [ date1; date2; date3; date4 ]

(* assoc list *)

(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t ->
      if k = k' then
        Some v
      else
        lookup k t

let assoc_list = insert 1 "one" (insert 2 "two" (insert 3 "three" []))
let lookup2 = lookup 2 assoc_list
let lookup4 = lookup 4 assoc_list

(* cards *)
type rank = Number of int | Ten | Jack | Queen | King | Ace
type suit = Hearts | Clubs | Diamonds | Spades
type card = { rank : rank; suit : suit }

(* The Ace of Clubs, the Queen of Hearts, the Two of Diamonds, the Seven of Spades: *)
let aoc = { rank = Ace; suit = Clubs }
let qoh = { rank = Queen; suit = Hearts }
let tod = { rank = Number 2; suit = Diamonds }
let sos = { rank = Number 7; suit = Spades }

(* matching *)

(* quadrant *)
type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign =
  if x < 0 then
    Neg
  else if x > 0 then
    Pos
  else
    Zero

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None

(* quadrant when *)
let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

(* depth *)
type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec depth : 'a tree -> int = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)

let rec same_shape tree1 tree2 =
  match (tree1, tree2) with
  | Leaf, Leaf -> true
  | Node (_, left1, right1), Node (_, left2, right2) ->
      same_shape left1 left2 && same_shape right1 right2
  | _ -> false

(* list max exn *)
let rec list_max_aux x = function
  | [] -> x
  | h :: t -> list_max_aux (max h x) t

let list_max = function
  | [] -> failwith "list_max"
  | h :: t -> list_max_aux h t

(* list max exn string *)
let list_max_string lst =
  try string_of_int (list_max lst) with Failure _ -> "empty"

(* is_bst *)
let test_tree = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))

let test_tree2 =
  Node
    ( 2,
      Node (1, Leaf, Node (24, Leaf, Node (43, Leaf, Leaf))),
      Node (3, Node (24, Node (32, Leaf, Leaf), Node (62, Leaf, Leaf)), Leaf) )
(*
let rec tree_max = function
    | Leaf -> 0
    | Node (node, left, right) ->
        max node (max (tree_max left) (tree_max right))
*)

type 'a tree_type = Empty | No_BST | BST of 'a * 'a

let rec is_bst_helper : 'a tree -> 'a tree_type = function
  | Leaf -> Empty
  | Node (node, left, right) -> begin
      match (is_bst_helper left, is_bst_helper right) with
      | Empty, Empty -> BST (node, node)
      | No_BST, _ | _, No_BST -> No_BST
      | Empty, BST (min, max) ->
          if min > node then
            BST (node, max)
          else
            No_BST
      | BST (min, max), Empty ->
          if max < node then
            BST (min, node)
          else
            No_BST
      | BST (lmin, lmax), BST (rmin, max2) ->
          if lmax < node && rmin > node then
            BST (lmin, max2)
          else
            No_BST
    end

let is_bst tree =
  match is_bst_helper tree with
  | No_BST -> false
  | _ -> true
