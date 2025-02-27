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

let sample_heap = List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let unsorted_list = [4; 10; 3; 5; 1]

let expected_heap = List.map (fun x -> Some x) [10; 5; 3; 4; 1]

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
  check
    (list (option int))
    "heapify unsorted list" expected_heap
    (Heap.heapify unsorted_list)

let test_heap_sort () =
  check (list int) "heap sort" [1; 3; 4; 5; 10] (Heap.heap_sort unsorted_list)

(* Test data *)
let sample_heap = List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let increased_heap = List.map (fun x -> Some x) [20; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let heap_for_extract =
  List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let expected_after_extract =
  List.map (fun x -> Some x) [14; 8; 10; 4; 7; 9; 3; 2; 1]

(* heap_increase_key tests *)
let test_heap_increase_key_valid () =
  let result = Heap.heap_increase_key sample_heap 0 20 in
  check
    (list (option int))
    "heap_increase_key with valid key" increased_heap result

let test_heap_increase_key_invalid () =
  check_raises "heap_increase_key with invalid key"
    (Invalid_argument "heap_increase_key passed a key that is not larger")
    (fun () -> ignore (Heap.heap_increase_key sample_heap 2 5) )

(* extract_max tests *)
let test_extract_max_nonempty () =
  match Heap.extract_max heap_for_extract with
  | Some (max_val, new_heap) ->
      check int "extracted max is correct" 16 max_val ;
      check
        (list (option int))
        "heap after extract_max" expected_after_extract new_heap
  | None ->
      fail "extract_max returned None for non-empty heap"

let test_extract_max_empty () =
  check
    (option (pair int (list (option int))))
    "extract_max empty heap" None (Heap.extract_max [])

(* extract_max tests for multiple extractions *)
let test_extract_max_multiple () =
  let heap = List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1] in
  match Heap.extract_max heap with
  | Some (max1, heap1) -> (
      check int "first extract max correct" 16 max1 ;
      check
        (list (option int))
        "heap after first extract"
        (List.map (fun x -> Some x) [14; 8; 10; 4; 7; 9; 3; 2; 1])
        heap1 ;
      (* Extract second max *)
      match Heap.extract_max heap1 with
      | Some (max2, heap2) ->
          check int "second extract max correct" 14 max2 ;
          check
            (list (option int))
            "heap after second extract"
            (List.map (fun x -> Some x) [10; 8; 9; 4; 7; 1; 3; 2])
            heap2
      | None ->
          fail "second extract_max returned None" )
  | None ->
      fail "first extract_max returned None"

(* extract_max test for single-element heap *)
let test_extract_max_single_element () =
  match Heap.extract_max [Some 42] with
  | Some (max_val, new_heap) ->
      check int "extracted max from single element" 42 max_val ;
      check
        (list (option int))
        "heap after extracting single element" [] new_heap
  | None ->
      fail "extract_max returned None for single-element heap"

(* extract_max test for repeated maximum elements *)
let test_extract_max_repeated_max () =
  let heap = List.map (fun x -> Some x) [16; 16; 10; 8; 7] in
  match Heap.extract_max heap with
  | Some (max_val, new_heap) ->
      check int "extracted max correct with repeated max" 16 max_val ;
      check
        (list (option int))
        "heap after extracting one max"
        (List.map (fun x -> Some x) [16; 8; 10; 7])
        new_heap
  | None ->
      fail "extract_max returned None with repeated max"

(* extract_max test for already sorted heap *)
let test_extract_max_sorted () =
  let heap = List.map (fun x -> Some x) [20; 15; 10; 5; 1] in
  match Heap.extract_max heap with
  | Some (max_val, new_heap) ->
      check int "extracted max from sorted heap" 20 max_val ;
      check
        (list (option int))
        "heap after extract from sorted"
        (List.map (fun x -> Some x) [15; 5; 10; 1])
        new_heap
  | None ->
      fail "extract_max returned None for sorted heap"

let empty_heap = []

let simple_heap = List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1]

let expected_after_insert_18 =
  List.map (fun x -> Some x) [18; 16; 10; 8; 14; 9; 3; 2; 4; 1; 7]

let expected_after_insert_5 =
  List.map (fun x -> Some x) [16; 14; 10; 8; 7; 9; 3; 2; 4; 1; 5]

(* Test inserting into an empty heap *)
let test_insert_empty () =
  let result = Heap.heap_insert empty_heap 42 in
  check (list (option int)) "insert into empty heap" [Some 42] result

(* Test inserting a larger key that becomes the new root *)
let test_insert_largest_key () =
  let result = Heap.heap_insert simple_heap 18 in
  check (list (option int)) "insert largest key" expected_after_insert_18 result

(* Test inserting a key that doesn't become the new root *)
let test_insert_non_root_key () =
  let result = Heap.heap_insert simple_heap 5 in
  check (list (option int)) "insert non-root key" expected_after_insert_5 result

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
        ; test_case "Non-empty heap" `Quick test_extract_max_nonempty
        ; test_case "Empty heap" `Quick test_extract_max_empty
        ; test_case "Multiple extractions" `Quick test_extract_max_multiple
        ; test_case "Single element heap" `Quick test_extract_max_single_element
        ; test_case "Repeated maximum elements" `Quick
            test_extract_max_repeated_max
        ; test_case "Sorted heap" `Quick test_extract_max_sorted
        ; test_case "insert into empty heap" `Quick test_insert_empty
        ; test_case "insert largest key" `Quick test_insert_largest_key
        ; test_case "insert non-root key" `Quick test_insert_non_root_key ] ) ]
