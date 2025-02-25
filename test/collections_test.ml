open Alcotest
open Oasis.Collections
open Oasis.Collections.Counter

let test_counter_basic () =
  let c = counter [1; 2; 2; 3; 3; 3] in
  check int "count 1" 1 c.?[1] ;
  check int "count 2" 2 c.?[2] ;
  check int "count 3" 3 c.?[3]

let test_counter_empty () =
  let c = counter [] in
  check int "empty counter length" 0 (List.length (keys c))

let test_elements_basic () =
  let c = counter [1; 2; 2; 3; 3; 3] in
  check (list int) "elements basic" [1; 2; 2; 3; 3; 3] (elements c)

let test_elements_empty () =
  let c = counter [] in
  check (list int) "elements empty" [] (elements c)

let test_most_common_default () =
  let c = counter [1; 2; 2; 3; 3; 3] in
  check
    (list (pair int int))
    "most common default"
    [(3, 3); (2, 2); (1, 1)]
    (most_common c)

let test_most_common_n () =
  let c = counter [1; 2; 2; 3; 3; 3] in
  check
    (list (pair int int))
    "most common top 2"
    [(3, 3); (2, 2)]
    (most_common ~n:2 c)

let test_subtract_basic () =
  let a = counter [1; 2; 2; 3; 3; 3] in
  let b = counter [2; 3; 3] in
  let res = subtract a b in
  check int "subtract 1" 1 res.?[1] ;
  check int "subtract 2" 1 res.?[2] ;
  check int "subtract 3" 1 res.?[3]

let test_subtract_non_overlapping () =
  let a = counter [1; 2; 2] in
  let b = counter [3; 4] in
  let res = subtract a b in
  check int "subtract non-overlap 1" 1 res.?[1] ;
  check int "subtract non-overlap 2" 2 res.?[2] ;
  check int "subtract non-overlap 3" (-1) res.?[3] ;
  check int "subtract non-overlap 4" (-1) res.?[4]

let test_total_basic () =
  let c = counter [1; 2; 2; 3; 3; 3] in
  check int "total basic" 6 (total c)

let test_total_empty () =
  let c = counter [] in
  check int "total empty" 0 (total c)

let () =
  let open Alcotest in
  run "Collections"
    [ ( "Counter"
      , [ test_case "counter basic" `Quick test_counter_basic
        ; test_case "counter empty" `Quick test_counter_empty
        ; test_case "elements basic" `Quick test_elements_basic
        ; test_case "elements empty" `Quick test_elements_empty
        ; test_case "most common default" `Quick test_most_common_default
        ; test_case "most common top n" `Quick test_most_common_n
        ; test_case "subtract basic" `Quick test_subtract_basic
        ; test_case "subtract non-overlapping" `Quick
            test_subtract_non_overlapping
        ; test_case "total basic" `Quick test_total_basic
        ; test_case "total empty" `Quick test_total_empty ] ) ]
