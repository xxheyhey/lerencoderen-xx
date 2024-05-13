open OUnit2
open Answers

let fifth_test name expectation input =
    name >:: fun _ ->
    assert_equal expectation (fifth input) ~printer:string_of_int

let sort_test name expectation input =
    name >:: fun _ -> assert_equal expectation (sort input)

let last_element_test name expectation input =
    name >:: fun _ -> assert_equal expectation (last_element input)

let any_zeroes_test name expectation input =
    name >:: fun _ -> assert_equal expectation (any_zeroes input)

let list_max_exn_test name expectation input =
    name >:: fun _ -> assert_raises expectation (fun _ -> list_max input)

let list_max_test name expectation input =
    name >:: fun _ -> assert_equal expectation (list_max input)

let tests =
    "test suite for CH3"
    >::: [
           fifth_test "empty" 0 [];
           fifth_test "singleton" 0 [ 1 ];
           fifth_test "two_elements" 0 [ 1; 2 ];
           fifth_test "five_elements" 5 [ 1; 2; 3; 4; 5 ];
           fifth_test "ten_elements" 5 [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ];
           sort_test "empty" [] [];
           sort_test "singleton" [ 1 ] [ 1 ];
           sort_test "two_elements" [ 2; 1 ] [ 1; 2 ];
           sort_test "five_elements" [ 5; 4; 3; 2; 1 ] [ 1; 2; 3; 4; 5 ];
           sort_test "ten_elements"
             [ 10; 9; 8; 7; 6; 5; 4; 3; 3; 2 ]
             [ 3; 2; 3; 4; 5; 6; 7; 8; 9; 10 ];
           last_element_test "singleton" 1 [ 1 ];
           last_element_test "two_elements" 2 [ 1; 2 ];
           last_element_test "five_elements" 5 [ 1; 2; 3; 4; 5 ];
           last_element_test "ten_elements" 10 [ 3; 2; 3; 4; 5; 6; 7; 8; 9; 10 ];
           any_zeroes_test "singleton" false [ 1 ];
           any_zeroes_test "two_elements" false [ 1; 2 ];
           any_zeroes_test "five_elements" true [ 0; 1; 2; 3; 4; 5 ];
           any_zeroes_test "ten_elements" true [ 3; 2; 3; 4; 0; 6; 7; 8; 9; 10 ];
           list_max_test "non_empty_list" 66 [ 1; 2; 65; 66; 12; 21 ];
           list_max_exn_test "empty_list" (Failure "list_max") [];
         ]

let _ = run_test_tt_main tests
