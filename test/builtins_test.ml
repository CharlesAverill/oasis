open Alcotest
open Oasis.Builtins
open Oasis.Builtins.Complex
open Oasis.Builtins.Dict

let test_abs_neg () = (check int) "same int" 1 (abs (-1))

let test_abs_zer () = (check int) "same int" 0 (abs 0)

let test_abs_pos () = (check int) "same int" 1 (abs 1)

let test_absf_neg () = (check (float 0.01)) "same float" 1. (absf (-1.))

let test_absf_zer () = (check (float 0.01)) "same float" 0. (absf 0.)

let test_absf_pos () = (check (float 0.01)) "same float" 1. (absf 1.)

let test_pow_zero () =
  (* 2^0 = 1 *)
  check (float 0.01) "2^0 = 1" 1. (pow 2 0)

let test_pow_positive () =
  (* 2^3 = 8 *)
  check (float 0.01) "2^3 = 8" 8. (pow 2 3)

let test_pow_negative () =
  (* 2^(-3) = 1/8 = 0.125 (assuming integer power) *)
  check (float 0.01) "2^-3 = 1/8" 0.125 (pow 2 (-3))
(* For integer, this rounds towards 0 *)

let test_pow_large () =
  (* 5^6 = 15625 *)
  check (float 0.01) "5^6 = 15625" 15625. (pow 5 6)

let test_powf_zero () =
  (* 2.0^0 = 1.0 *)
  check (float 0.01) "2.0^0 = 1.0" 1.0 (powf 2.0 0.0)

let test_powf_positive () =
  (* 2.0^3.0 = 8.0 *)
  check (float 0.01) "2.0^3.0 = 8.0" 8.0 (powf 2.0 3.0)

let test_powf_fractional () =
  (* 2.0^0.5 = sqrt(2) ~ 1.414 *)
  check (float 0.01) "2.0^0.5 = sqrt(2)" 1.414 (powf 2.0 0.5)

let test_powf_large () =
  (* 3.0^5.0 = 243.0 *)
  check (float 0.01) "3.0^5.0 = 243.0" 243.0 (powf 3.0 5.0)

let test_round_dfrac () =
  (* Test rounding to 0 decimal places *)
  (check (float 0.01)) "test_round_dfrac_0" 1. (round_dfrac 0 1.1) ;
  (check (float 0.01)) "test_round_dfrac_0" 2. (round_dfrac 0 1.5) ;
  (check (float 0.01)) "test_round_dfrac_0" 2. (round_dfrac 0 2.4) ;
  (* Test rounding to 1 decimal place *)
  (check (float 0.01)) "test_round_dfrac_1" 1.2 (round_dfrac 1 1.15) ;
  (check (float 0.01)) "test_round_dfrac_1" 1.3 (round_dfrac 1 1.25) ;
  (check (float 0.01)) "test_round_dfrac_1" 2.7 (round_dfrac 1 2.74) ;
  (* Test rounding to 2 decimal places *)
  (check (float 0.01)) "test_round_dfrac_2" 1.12 (round_dfrac 2 1.115) ;
  (check (float 0.01)) "test_round_dfrac_2" 1.13 (round_dfrac 2 1.125) ;
  (check (float 0.01)) "test_round_dfrac_2" 2.73 (round_dfrac 2 2.734) ;
  (* Test rounding negative numbers *)
  (check (float 0.01)) "test_round_dfrac_negative" (-1.) (round_dfrac 0 (-1.1)) ;
  (check (float 0.01)) "test_round_dfrac_negative" (-2.) (round_dfrac 0 (-2.5)) ;
  (check (float 0.01)) "test_round_dfrac_negative" (-3.) (round_dfrac 0 (-2.75)) ;
  (* Test rounding to no decimal places *)
  (check (float 0.01)) "test_round_dfrac_no_decimals" 5. (round_dfrac 0 5.4) ;
  (check (float 0.01)) "test_round_dfrac_no_decimals" 6. (round_dfrac 0 5.75) ;
  (* Test rounding with a large number of decimal places *)
  (check (float 0.00001))
    "test_round_dfrac_large_decimal" 1.11111 (round_dfrac 5 1.1111139) ;
  (check (float 0.00001))
    "test_round_dfrac_large_decimal" 1.12345 (round_dfrac 5 1.123446)

let test_all_empty () = (check bool) "same bool" true (all [])

let test_all_true () = (check bool) "same bool" true (all [true; true; true])

let test_all_false () = (check bool) "same bool" false (all [true; false; true])

let test_any_empty () = (check bool) "same bool" false (any [])

let test_any_true () = (check bool) "same bool" true (any [true; true; true])

let test_any_false () = (check bool) "same bool" true (any [true; false; true])

let test_unique_no_duplicates () =
  let result = unique [1; 2; 3; 4] in
  check (list int) "same" [1; 2; 3; 4] result

let test_unique_with_duplicates () =
  let result = unique [1; 2; 2; 3; 3; 3; 4] in
  check (list int) "same" [1; 2; 3; 4] result

let test_unique_empty_list () =
  let result = unique [] in
  check (list int) "same" [] result

let test_unique_single_element () =
  let result = unique [42] in
  check (list int) "same" [42] result

let test_unique_all_same () =
  let result = unique [7; 7; 7; 7] in
  check (list int) "same" [7] result

let test_unique_strings () =
  let result = unique ["apple"; "banana"; "apple"; "cherry"] in
  check (list string) "same" ["apple"; "banana"; "cherry"] result

let test_unique_large_numbers () =
  let result = unique [1000000000; 999999999; 1000000000] in
  check (list int) "same" [1000000000; 999999999] result

let test_unique_negative_numbers () =
  let result = unique [-1; -2; -2; -3; -3; -1] in
  check (list int) "same" [-1; -2; -3] result

let test_unique_floats () =
  let result = unique [1.1; 2.2; 2.2; 3.3; 1.1] in
  check (list (float 0.01)) "same" [1.1; 2.2; 3.3] result

let test_slice_default () =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let result = slice l 2 8 in
  (check (list int)) "slice with default step" [2; 3; 4; 5; 6; 7] result

let test_slice_step_2 () =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let result = slice l ~step:2 2 8 in
  (check (list int)) "slice with step 2" [2; 4; 6] result

let test_slice_step_3 () =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let result = slice l ~step:3 2 8 in
  (check (list int)) "slice with step 3" [2; 5] result

let test_slice_reverse_step () =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let result = slice l ~step:2 8 2 in
  (check (list int)) "slice reverse with step 2" [] result

let test_slice_out_of_bounds () =
  let l = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9] in
  let result = slice l 10 15 in
  (check (list int)) "slice out of bounds" [] result

let test_slice_empty_list () =
  let l = [] in
  let result = slice l 0 5 in
  (check (list int)) "slice empty list" [] result

let test_sum_empty () =
  let result = sum [] in
  (check int) "sum of empty list" 0 result

let test_sum_single_element () =
  let result = sum [5] in
  (check int) "sum of single element list" 5 result

let test_sum_multiple_elements () =
  let result = sum [1; 2; 3; 4; 5] in
  (check int) "sum of multiple elements" 15 result

let test_sumf_empty () =
  let result = sumf [] in
  (check (float 0.01)) "sum of empty float list" 0. result

let test_sumf_single_element () =
  let result = sumf [5.0] in
  (check (float 0.01)) "sum of single element float list" 5.0 result

let test_sumf_multiple_elements () =
  let result = sumf [1.0; 2.0; 3.0; 4.0; 5.0] in
  (check (float 0.01)) "sum of multiple elements float list" 15.0 result

let test_sumc_empty () =
  let result = absc (sumc []) in
  (check (float 0.01)) "sum of empty complex list" (absc Complex.zero) result

let test_sumc_single_element () =
  let result = absc (sumc [Complex.{real= 1.0; imaginary= 2.0}]) in
  (check (float 0.01))
    "sum of single element complex list"
    (absc Complex.{real= 1.0; imaginary= 2.0})
    result

let test_sumc_multiple_elements () =
  let result =
    absc
      (sumc
         [ Complex.{real= 1.0; imaginary= 1.0}
         ; Complex.{real= 2.0; imaginary= 2.0}
         ; Complex.{real= 3.0; imaginary= 3.0} ] )
  in
  (check (float 0.01))
    "sum of multiple complex elements"
    (absc Complex.{real= 6.0; imaginary= 6.0})
    result

let test_any_all_false () = (check bool) "same bool" false (any [false])

let test_bin_0 () = (check string) "same string" "0b0" (bin 0)

let test_bin_neg () = (check string) "same string" "-0b1010" (bin (-10))

let test_bin_pos () = (check string) "same string" "0b1010" (bin 10)

let test_oct_0 () = (check string) "same string" "0o0" (oct 0)

let test_oct_neg () = (check string) "same string" "-0o71" (oct (-57))

let test_oct_pos () = (check string) "same string" "0o71" (oct 57)

let test_hex_pos () =
  (* Test positive numbers *)
  let result = hex 42 in
  check string "hex of 42" "0x2A" result

let test_hex_neg () =
  (* Test negative numbers *)
  let result = hex (-42) in
  check string "hex of -42" "-0x2A" result

let test_hex_zero () =
  (* Test zero *)
  let result = hex 0 in
  check string "hex of 0" "0x0" result

let test_hex_large () =
  (* Test large numbers *)
  let result = hex 123456789 in
  check string "hex of 123456789" "0x75BCD15" result

let test_hex_single_digit () =
  (* Test single-digit numbers *)
  let result = hex 8 in
  check string "hex of 8" "0x8" result

let test_hex_edge_case () =
  (* Test edge case with a number just below a power of 16 *)
  let result = hex 255 in
  check string "hex of 255" "0xFF" result

let test_chr_1 () = (check string) "same string" "ỗ" (chr 0x1ED7)

let test_chr_2 () = (check string) "same string" "♓" (chr 0x2653)

let test_chr_3 () = (check string) "same string" "𑜸" (chr 0x11738)

let test_chr_4 () = (check string) "same string" "𐹪" (chr 0x10E6A)

let test_chr_5 () = (check string) "same string" "⊘" (chr 0x2298)

let test_ord_1 () = (check int) "same int" (ord "ỗ") 0x1ED7

let test_ord_2 () = (check int) "same int" (ord "♓") 0x2653

let test_ord_3 () = (check int) "same int" (ord "𑜸") 0x11738

let test_ord_4 () = (check int) "same int" (ord "𐹪") 0x10E6A

let test_ord_5 () = (check int) "same int" (ord "⊘") 0x2298

let test_chr_ord_inverse () =
  (* TODO : broken *)
  for i = 0 to 11000 do
    (check int) "same int" i (ord (chr i))
  done

(* Test complex addition *)
let test_add () =
  let c1 = complex_of_float 3. in
  let c2 = imag_of_float 4. in
  let result = c1 +^ c2 in
  check (float 0.01) "real part" 3. (real result) ;
  check (float 0.01) "imaginary part" 4. (imag result)

(* Test complex subtraction *)
let test_sub () =
  let c1 = {real= 5.; imaginary= 3.} in
  let c2 = {real= 2.; imaginary= 1.} in
  let result = c1 -^ c2 in
  check (float 0.01) "real part" 3. (real result) ;
  check (float 0.01) "imaginary part" 2. (imag result)

(* Test complex multiplication *)
let test_mul () =
  let c1 = {real= 3.; imaginary= 2.} in
  let c2 = {real= 1.; imaginary= 7.} in
  let result = c1 *^ c2 in
  check (float 0.01) "real part" (-11.) (real result) ;
  check (float 0.01) "imaginary part" 23. (imag result)

(* Test complex conjugate *)
let test_conj () =
  let c = {real= 4.; imaginary= -5.} in
  let result = ~^c in
  check (float 0.01) "real part" 4. (real result) ;
  check (float 0.01) "imaginary part" 5. (imag result)

(* Test absolute value of a complex number *)
let test_abs () =
  let c = {real= 3.; imaginary= 4.} in
  let result = absc c in
  check (float 0.01) "absolute value" 5. result

(* Test reciprocal of a complex number *)
let test_recip () =
  let c = {real= 1.; imaginary= 1.} in
  let result = recip c in
  check (float 0.01) "real part" 0.5 (real result) ;
  check (float 0.01) "imaginary part" (-0.5) (imag result)

(* Test division of complex numbers *)
let test_div () =
  let c1 = {real= 3.; imaginary= 2.} in
  let c2 = {real= 4.; imaginary= -1.} in
  let result = c1 /^ c2 in
  check (float 0.01) "real part" 0.588 (real result) ;
  check (float 0.01) "imaginary part" 0.647 (imag result)

(* Test real check function *)
let test_is_real () =
  let c1 = {real= 3.; imaginary= 0.} in
  let c2 = {real= 0.; imaginary= 1.} in
  check bool "c1 is real" true (is_real c1) ;
  check bool "c2 is not real" false (is_real c2)

let test_empty_map () =
  let m = empty 0 in
  (* Check access to an empty map (should return the default value) *)
  (check int) "default value" 0 m.?[5]
(* Access index 5 *)

let test_err_empty_map () =
  let m = err_empty () in
  (* Accessing an empty map should raise a KeyError *)
  let f () = m.?[5] in
  check_raises "key error" (KeyError "No such key") f

let test_update_map () =
  let m = empty 0 in
  (* Update map: set the value for key 5 to 42 *)
  let m_updated = m.?[5] <- 42 in
  check int "updated value" 42 m_updated.?[5]
(* Access after update *)

let test_update_existing_key () =
  let m : (int, int) dict = empty 0 in
  (* Update key 5 to 42, then update key 5 to 100 *)
  let m_updated = m.?[5] <- 42 in
  let m_updated2 = m_updated.?[5] <- 100 in
  (check int) "updated value for key 5" 100 m_updated2.?[5]

let test_err_empty_with_custom_key () =
  let m = err_empty ~string_of_key:(Some string_of_int) () in
  (* Accessing an empty map with a custom key string function should raise KeyError with a message *)
  let f () = m.?[5] in
  check_raises "key error with custom message" (KeyError "No such key '5'") f

let test_len () =
  let m = empty 0 in
  (* Add items to the map and check length *)
  let m_updated = m.?[5] <- 42 in
  check int "map length" 1 (len m_updated)
(* Length should be 1 *)

let test_key_in () =
  let m = empty 0 in
  let m_updated = m.?[5] <- 42 in
  (* Test if a key exists in the map *)
  check bool "key exists" true (key_in m_updated 5) ;
  (* Should return true for key 5 *)
  check bool "key does not exist" false (key_in m_updated 10)
(* Should return false for key 10 *)

let test_val_in () =
  let m = empty 0 in
  let m_updated = m.?[5] <- 42 in
  (* Test if a value exists in the map *)
  check bool "value exists" true (val_in m_updated 42) ;
  (* Should return true for value 42 *)
  check bool "value does not exist" false (val_in m_updated 100)
(* Should return false for value 100 *)

let test_from_keys () =
  let m = empty 0 in
  let m_updated = m.?[5] <- 42 in
  (* Create a map from keys of another map with a default value *)
  let m_from_keys = from_keys m_updated 100 in
  check int "from_keys map value" 100 m_from_keys.?[5]
(* Should return 100 for key 5 since it's a default *)

let test_items () =
  let m = empty 0 in
  let m_updated = m.?[5] <- 42 in
  (* Get the items of the map as a list of key-value pairs *)
  let items = items m_updated in
  check
    (list (pair int int))
    "items"
    [(5, 42)]
    items (* Should return [(5, 42)] *)

let test_merge () =
  let m1 = empty 0 in
  let m2 = empty 100 in
  (* Add key-value pairs to the maps *)
  let m1_updated = m1.?[5] <- 42 in
  let m2_updated = m2.?[5] <- 99 in
  (* Merge m2 into m1 and check if the values of m2 overwrite m1 *)
  let merged = merge m1_updated m2_updated in
  check int "merged map value" 99 merged.?[5]
(* Value should be 99 from m2 *)

let test_divmod_int () =
  (* Test integer division and modulo *)
  let result = divmod 10 3 in
  check (pair int int) "divmod 10 3" (3, 1) result

let test_divmod_float () =
  (* Test float division and modulo *)
  let result = divmodf 10.5 3.0 in
  check (pair (float 0.01) (float 0.01)) "divmod 10.5 3.0" (3.5, 0.0) result

let test_enumerate () =
  (* Test enumerate function on a list *)
  let result = enumerate ["a"; "b"; "c"] in
  check
    (list (pair int string))
    "enumerate"
    [(0, "a"); (1, "b"); (2, "c")]
    result

let test_enumerate_start () =
  (* Test enumerate function on a list with a start index *)
  let result = enumerate ~start:5 ["a"; "b"; "c"] in
  check
    (list (pair int string))
    "enumerate with start"
    [(5, "a"); (6, "b"); (7, "c")]
    result

let test_filter () =
  (* Test filter function to select even numbers *)
  let result = filter (fun x -> x mod 2 = 0) [1; 2; 3; 4; 5; 6] in
  check (list int) "filter even numbers" [2; 4; 6] result

let test_format_int () =
  let result = format "%d" 42 in
  check string "formatted int" "42" result

let test_format_float () =
  let result = format "%.2f" 3.14159 in
  check string "formatted float" "3.14" result

let test_format_string () =
  let result = format "%s" "Hello, world!" in
  check string "formatted string" "Hello, world!" result

let test_format_bool () =
  let result_true = format "%b" true in
  let result_false = format "%b" false in
  check string "formatted bool true" "true" result_true ;
  check string "formatted bool false" "false" result_false

let test_format_tuple () =
  let result = format "(%d, %s)" 42 "Answer" in
  check string "formatted tuple" "(42, Answer)" result

let test_format_list () =
  let result = format "[%s]" (String.concat "; " ["a"; "b"; "c"]) in
  check string "formatted list" "[a; b; c]" result

let test_max_int () =
  (* max_int of [1; 2; 3] should return Some 3 *)
  check (option int) "max_int [1; 2; 3] = Some 3" (Some 3) (max_int [1; 2; 3])

let test_max_int_empty () =
  (* max_int of an empty list should return None *)
  check (option int) "max_int [] = None" None (max_int [])

let test_max_float () =
  (* max_float of [1.0; 2.0; 3.0] should return Some 3.0 *)
  check
    (option (float 0.01))
    "max_float [1.0; 2.0; 3.0] = Some 3.0" (Some 3.0)
    (max_float [1.0; 2.0; 3.0])

let test_max_float_empty () =
  (* max_float of an empty list should return None *)
  check (option (float 0.01)) "max_float [] = None" None (max_float [])

let test_min_int () =
  (* min_int of [1; 2; 3] should return Some 1 *)
  check (option int) "min_int [1; 2; 3] = Some 1" (Some 1) (min_int [1; 2; 3])

let test_min_int_empty () =
  (* min_int of an empty list should return None *)
  check (option int) "min_int [] = None" None (min_int [])

let test_min_float () =
  (* min_float of [1.0; 2.0; 3.0] should return Some 1.0 *)
  check
    (option (float 0.01))
    "min_float [1.0; 2.0; 3.0] = Some 1.0" (Some 1.0)
    (min_float [1.0; 2.0; 3.0])

let test_min_float_empty () =
  (* min_float of an empty list should return None *)
  check (option (float 0.01)) "min_float [] = None" None (min_float [])

let test_range () =
  (* range with default start = 0 and step = 1, range 5 should return [0; 1; 2; 3; 4] *)
  check (list int) "range 5" [0; 1; 2; 3; 4] (range 5)

let test_range_start () =
  (* range with start = 2 and step = 1, range 5 should return [2; 3; 4] *)
  check (list int) "range ~start:2 5" [2; 3; 4] (range ~start:2 5)

let test_range_step () =
  (* range with start = 0, step = 2, range 5 should return [0; 2; 4] *)
  check (list int) "range ~step:2 5" [0; 2; 4] (range ~step:2 5)

let test_range_empty () =
  (* range with start = 10, step = 1, and j = 0 should return an empty list *)
  check (list int) "range ~start:10 0" [] (range ~start:10 0)

let test_swap_basic () =
  let l = [1; 2; 3; 4; 5] in
  let swapped = swap l 1 3 in
  check (list int) "swap basic" [1; 4; 3; 2; 5] swapped

let test_swap_same_index () =
  let l = [1; 2; 3; 4; 5] in
  let swapped = swap l 2 2 in
  check (list int) "swap same index" [1; 2; 3; 4; 5] swapped

let test_swap_first_last () =
  let l = [1; 2; 3; 4; 5] in
  let swapped = swap l 0 4 in
  check (list int) "swap first and last" [5; 2; 3; 4; 1] swapped

let test_swap_adjacent () =
  let l = [1; 2; 3; 4; 5] in
  let swapped = swap l 2 3 in
  check (list int) "swap adjacent" [1; 2; 4; 3; 5] swapped

let test_swap_single_element_list () =
  let l = [1] in
  let swapped = swap l 0 0 in
  check (list int) "swap single element list" [1] swapped

let test_swap_empty_list () =
  let l = [] in
  let swapped = swap l 0 0 in
  check (list int) "swap empty list" [] swapped

(* Helper dictionaries for testing *)
let d1 = ((empty 0).?["a"] <- 1).?["b"] <- 2

let d2 = (((empty 0).?["a"] <- 1).?["b"] <- 2).?["c"] <- 3

let d3 = ((empty 0).?["a"] <- 1).?["b"] <- 2

let d4 = ((empty 0).?["a"] <- 1).?["b"] <- 5

let d_empty = {_keys= []; _values= []; _f= (fun _ -> 0)}

(* Subset tests *)
let test_subset_true () = check bool "d1 < d2" true (d1 <? d2)

let test_subset_false () = check bool "d2 < d1" false (d2 <? d1)

let test_subset_empty () = check bool "empty < d1" true (d_empty <? d1)

(* Equality tests *)
let test_equal_true () = check bool "d1 =? d3" true (d1 =? d3)

let test_equal_false () = check bool "d1 =? d4" false (d1 =? d4)

(* Inequality tests *)
let test_not_equal_true () = check bool "d1 !=? d4" true (d1 <>? d4)

let test_not_equal_false () = check bool "d1 !=? d3" false (d1 <>? d3)

(* Greater than tests *)
let test_greater_true () = check bool "d2 >? d1" true (d2 >? d1)

let test_greater_false () = check bool "d1 >? d2" false (d1 >? d2)

(* Less than or equal tests *)
let test_lte_true_equal () = check bool "d1 <=? d3" true (d1 <=? d3)

let test_lte_true_subset () = check bool "d1 <=? d2" true (d1 <=? d2)

let test_lte_false () = check bool "d2 <=? d1" false (d2 <=? d1)

(* Greater than or equal tests *)
let test_gte_true_equal () = check bool "d1 >=? d3" true (d1 >=? d3)

let test_gte_true_superset () = check bool "d2 >=? d1" true (d2 >=? d1)

let test_gte_false () = check bool "d1 >=? d2" false (d1 >=? d2)

let () =
  let open Alcotest in
  run "Builtins"
    [ ( "Math"
      , [ test_case "test_abs_neg" `Quick test_abs_neg
        ; test_case "test_abs_zer" `Quick test_abs_zer
        ; test_case "test_abs_pos" `Quick test_abs_pos
        ; test_case "test_absf_neg" `Quick test_absf_neg
        ; test_case "test_absf_zer" `Quick test_absf_zer
        ; test_case "test_absf_pos" `Quick test_absf_pos
        ; test_case "test_divmod_int" `Quick test_divmod_int
        ; test_case "test_divmod_float" `Quick test_divmod_float
        ; test_case "Complex Addition" `Quick test_add
        ; test_case "Complex Subtraction" `Quick test_sub
        ; test_case "Complex Multiplication" `Quick test_mul
        ; test_case "Complex Conjugate" `Quick test_conj
        ; test_case "Complex Absolute Value" `Quick test_abs
        ; test_case "Complex Reciprocal" `Quick test_recip
        ; test_case "Complex Division" `Quick test_div
        ; test_case "Complex Real Check" `Quick test_is_real
        ; test_case "test_powf_zero" `Quick test_powf_zero
        ; test_case "test_powf_positive" `Quick test_powf_positive
        ; test_case "test_powf_fractional" `Quick test_powf_fractional
        ; test_case "test_powf_large" `Quick test_powf_large
        ; test_case "test_pow_zero" `Quick test_pow_zero
        ; test_case "test_pow_positive" `Quick test_pow_positive
        ; test_case "test_pow_negative" `Quick test_pow_negative
        ; test_case "test_pow_large" `Quick test_pow_large
        ; test_case "test_round_dfrac" `Quick test_round_dfrac ] )
    ; ( "Number representations"
      , [ test_case "test_bin_0" `Quick test_bin_0
        ; test_case "test_bin_neg" `Quick test_bin_neg
        ; test_case "test_bin_pos" `Quick test_bin_pos
        ; test_case "test_oct_0" `Quick test_oct_0
        ; test_case "test_oct_neg" `Quick test_oct_neg
        ; test_case "test_oct_pos" `Quick test_oct_pos
        ; test_case "test_hex_pos" `Quick test_hex_pos
        ; test_case "test_hex_neg" `Quick test_hex_neg
        ; test_case "test_hex_zero" `Quick test_hex_zero
        ; test_case "test_hex_large" `Quick test_hex_large
        ; test_case "test_hex_single_digit" `Quick test_hex_single_digit
        ; test_case "test_hex_edge_case" `Quick test_hex_edge_case ] )
    ; ( "String operations"
      , [ test_case "test_chr_1" `Quick test_chr_1
        ; test_case "test_chr_2" `Quick test_chr_2
        ; test_case "test_chr_3" `Quick test_chr_3
        ; test_case "test_chr_4" `Quick test_chr_4
        ; test_case "test_chr_5" `Quick test_chr_5
        ; test_case "test_ord_1" `Quick test_ord_1
        ; test_case "test_ord_2" `Quick test_ord_2
        ; test_case "test_ord_3" `Quick test_ord_3
        ; test_case "test_ord_4" `Quick test_ord_4
        ; test_case "test_ord_5" `Quick test_ord_5
          (* ; test_case "test_chr_ord_inverse" `Quick test_chr_ord_inverse *)
        ; test_case "test_format_int" `Quick test_format_int
        ; test_case "test_format_float" `Quick test_format_float
        ; test_case "test_format_string" `Quick test_format_string
        ; test_case "test_format_bool" `Quick test_format_bool
        ; test_case "test_format_tuple" `Quick test_format_tuple
        ; test_case "test_format_list" `Quick test_format_list ] )
    ; ( "Dict operations"
      , [ test_case "test_empty_map" `Quick test_empty_map
        ; test_case "test_err_empty_map" `Quick test_err_empty_map
        ; test_case "test_update_map" `Quick test_update_map
        ; test_case "test_update_existing_key" `Quick test_update_existing_key
        ; test_case "test_err_empty_with_custom_key" `Quick
            test_err_empty_with_custom_key
        ; test_case "test_len" `Quick test_len
        ; test_case "test_key_in" `Quick test_key_in
        ; test_case "test_val_in" `Quick test_val_in
        ; test_case "test_from_keys" `Quick test_from_keys
        ; test_case "test_items" `Quick test_items
        ; test_case "test_merge" `Quick test_merge
        ; test_case "subset true" `Quick test_subset_true
        ; test_case "subset false" `Quick test_subset_false
        ; test_case "subset empty" `Quick test_subset_empty
        ; test_case "equal true" `Quick test_equal_true
        ; test_case "equal false" `Quick test_equal_false
        ; test_case "not equal true" `Quick test_not_equal_true
        ; test_case "not equal false" `Quick test_not_equal_false
        ; test_case "greater true" `Quick test_greater_true
        ; test_case "greater false" `Quick test_greater_false
        ; test_case "less or equal true (equal)" `Quick test_lte_true_equal
        ; test_case "less or equal true (subset)" `Quick test_lte_true_subset
        ; test_case "less or equal false" `Quick test_lte_false
        ; test_case "greater or equal true (equal)" `Quick test_gte_true_equal
        ; test_case "greater or equal true (superset)" `Quick
            test_gte_true_superset
        ; test_case "greater or equal false" `Quick test_gte_false ] )
    ; ( "List operations"
      , [ test_case "test_all_empty" `Quick test_all_empty
        ; test_case "test_all_true" `Quick test_all_true
        ; test_case "test_all_false" `Quick test_all_false
        ; test_case "test_any_empty" `Quick test_any_empty
        ; test_case "test_any_true" `Quick test_any_true
        ; test_case "test_any_false" `Quick test_any_false
        ; test_case "test_any_all_false" `Quick test_any_all_false
        ; test_case "test_enumerate" `Quick test_enumerate
        ; test_case "test_enumerate_start" `Quick test_enumerate_start
        ; test_case "test_filter" `Quick test_filter
        ; test_case "test_max_int" `Quick test_max_int
        ; test_case "test_max_int_empty" `Quick test_max_int_empty
        ; test_case "test_max_float" `Quick test_max_float
        ; test_case "test_max_float_empty" `Quick test_max_float_empty
        ; test_case "test_min_int" `Quick test_min_int
        ; test_case "test_min_int_empty" `Quick test_min_int_empty
        ; test_case "test_min_float" `Quick test_min_float
        ; test_case "test_min_float_empty" `Quick test_min_float_empty
        ; test_case "test_range" `Quick test_range
        ; test_case "test_range_start" `Quick test_range_start
        ; test_case "test_range_step" `Quick test_range_step
        ; test_case "test_range_empty" `Quick test_range_empty
        ; test_case "test_unique_no_duplicates" `Quick test_unique_no_duplicates
        ; test_case "test_unique_with_duplicates" `Quick
            test_unique_with_duplicates
        ; test_case "test_unique_empty_list" `Quick test_unique_empty_list
        ; test_case "test_unique_single_element" `Quick
            test_unique_single_element
        ; test_case "test_unique_all_same" `Quick test_unique_all_same
        ; test_case "test_unique_strings" `Quick test_unique_strings
        ; test_case "test_unique_large_numbers" `Quick test_unique_large_numbers
        ; test_case "test_unique_negative_numbers" `Quick
            test_unique_negative_numbers
        ; test_case "test_unique_floats" `Quick test_unique_floats
        ; test_case "test_slice_default" `Quick test_slice_default
        ; test_case "test_slice_step_2" `Quick test_slice_step_2
        ; test_case "test_slice_step_3" `Quick test_slice_step_3
        ; test_case "test_slice_reverse_step" `Quick test_slice_reverse_step
        ; test_case "test_slice_out_of_bounds" `Quick test_slice_out_of_bounds
        ; test_case "test_slice_empty_list" `Quick test_slice_empty_list
        ; test_case "test_sum_empty" `Quick test_sum_empty
        ; test_case "test_sum_single_element" `Quick test_sum_single_element
        ; test_case "test_sum_multiple_elements" `Quick
            test_sum_multiple_elements
        ; test_case "test_sumf_empty" `Quick test_sumf_empty
        ; test_case "test_sumf_single_element" `Quick test_sumf_single_element
        ; test_case "test_sumf_multiple_elements" `Quick
            test_sumf_multiple_elements
        ; test_case "test_sumc_empty" `Quick test_sumc_empty
        ; test_case "test_sumc_single_element" `Quick test_sumc_single_element
        ; test_case "test_sumc_multiple_elements" `Quick
            test_sumc_multiple_elements
        ; test_case "swap basic" `Quick test_swap_basic
        ; test_case "swap same index" `Quick test_swap_same_index
        ; test_case "swap first and last" `Quick test_swap_first_last
        ; test_case "swap adjacent elements" `Quick test_swap_adjacent
        ; test_case "swap single element list" `Quick
            test_swap_single_element_list
        ; test_case "swap empty list" `Quick test_swap_empty_list ] ) ]
