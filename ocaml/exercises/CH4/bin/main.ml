open CH4.Randomstuff

let () = print_float_list ([ 1.; 3.4; 9.9; 11. ] |> map sqrt |> map string_of_float)
