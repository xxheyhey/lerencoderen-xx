let rec map f = function
  | [] -> []
  | h :: t ->
    let h' = f h in
    h' :: map f t

let rec print_float_list = function
  | [] -> ()
  | h :: t -> begin
    match print_endline h with
    | () -> print_float_list t
  end
