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

module Heap = Heap (IntOT)

let sample_heap : IntOT.t list = [16; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let unsorted_list = [4; 10; 3; 5; 1]

let expected_heap = [10; 5; 3; 4; 1]

(* parent tests *)
let test_parent_root () =
  check (option int) "root has no parent" None (Heap.parent sample_heap 0)

let test_parent_non_root () =
  check (option int) "parent of index 3" (Some 0) (Heap.parent sample_heap 2)

(* lefti tests *)
let test_lefti_exists () =
  check (option int) "left child of index 2" (Some 5) (Heap.lefti sample_heap 2)

let test_lefti_none () =
  check (option int) "left child of last index" None (Heap.lefti sample_heap 10)

(* righti tests *)
let test_righti_exists () =
  check (option int) "right child of index 2" (Some 6)
    (Heap.righti sample_heap 2)

let test_righti_none () =
  check (option int) "right child of last index" None
    (Heap.righti sample_heap 10)

(* heapify tests *)
let test_heapify () =
  check (list int) "heapify unsorted list" expected_heap
    (Heap.heapify unsorted_list)

let test_heap_sort () =
  check (list int) "heap sort" [1; 3; 4; 5; 10] (Heap.heap_sort unsorted_list)

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
        ; test_case "total empty" `Quick test_total_empty ] )
    ; ( "Heap"
      , [ test_case "test parent root" `Quick test_parent_root
        ; test_case "test parent not root" `Quick test_parent_non_root
        ; test_case "test lefti exists" `Quick test_lefti_exists
        ; test_case "test lefti none" `Quick test_lefti_none
        ; test_case "test righti exists" `Quick test_righti_exists
        ; test_case "test righti none" `Quick test_righti_none
        ; test_case "test heapify" `Quick test_heapify
        ; test_case "test heap sort" `Quick test_heap_sort ] ) ]
