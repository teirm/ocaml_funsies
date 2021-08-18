open OUnit2

let tests = "test suit for sum" >::: [
    "empty"  >:: (fun _ -> assert_equal 0 (Sum.sum []));
    "one"    >:: (fun _ -> assert_equal 1 (Sum.sum [1]));
    "onetwo" >:: (fun _ -> assert_equal 3 (Sum.sum [1; 2]));
]

let _ = run_test_tt_main tests
