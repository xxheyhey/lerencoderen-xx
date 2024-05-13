open OUnit2
open Sum

let sum_test name expectation input =
    name >:: (fun _ -> assert_equal expectation (sum input) ~printer:string_of_int)

let tests = "test suite for sum" >::: [
    sum_test "empty" 0 [];
    sum_test "singleton" 1 [1];
    sum_test "two_elements" 3 [1; 2];
]

let _ = run_test_tt_main tests
