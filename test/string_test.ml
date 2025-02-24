open Alcotest
open Oasis.String

(* Sample mapping function for tests *)
let sample_map = function
  | "name" ->
      "Alice"
  | "age" ->
      "30"
  | "city" ->
      "Paris"
  | id ->
      "<unknown:" ^ id ^ ">"

(* Test cases *)
let test_template_no_variables () =
  let result = template "Hello, world!" sample_map in
  check string "no variables" "Hello, world!" result

let test_template_single_variable () =
  let result = template "Hello, $name!" sample_map in
  check string "single variable" "Hello, Alice!" result

let test_template_multiple_variables () =
  let result =
    template "$name is $age years old and lives in $city." sample_map
  in
  check string "multiple variables" "Alice is 30 years old and lives in Paris."
    result

let test_template_unknown_variable () =
  let result = template "Hello, $unknown!" sample_map in
  check string "unknown variable" "Hello, <unknown:unknown>!" result

let test_template_adjacent_variables () =
  let result = template "$name$age$city" sample_map in
  check string "adjacent variables" "Alice30Paris" result

let test_template_escaped_dollar () =
  let result = template "Price: $$100" sample_map in
  check string "escaped dollar" "Price: $100" result

let test_template_trailing_dollar () =
  let result = template "Amount: $" sample_map in
  check string "trailing dollar" "Amount: $" result

let test_template_non_identifier_characters () =
  let result = template "Symbols: $name! $city?" sample_map in
  check string "non-identifier characters" "Symbols: Alice! Paris?" result

let test_capitalize_empty () = (check string) "Empty string" "" (capitalize "")

let test_capitalize_single_char () =
  (check string) "Single char" "A" (capitalize "a")

let test_capitalize_word () = (check string) "Word" "Hello" (capitalize "hELLO")

let test_capitalize_already_correct () =
  (check string) "Already capitalized" "Hello" (capitalize "Hello")

let test_center_exact_width () =
  (check string) "Exact width" "hello" (center "hello" 5)

let test_center_wider_width () =
  (check string) "Wider width" " hello " (center "hello" 7)

let test_center_odd_padding () =
  (check string) "Odd padding" "  hello " (center "hello" 8)

let test_center_with_fill_char () =
  (check string) "Custom fill char" "--hello--"
    (center ~fill_char:'-' "hello" 9)

let test_count_empty_substring () =
  (check int) "Empty substring" 0 (count "abc" "")

let test_count_no_match () = (check int) "No match" 0 (count "abcabc" "z")

let test_count_single_match () = (check int) "Single match" 1 (count "abc" "a")

let test_count_multiple_matches () =
  (check int) "Multiple matches" 2 (count "abcabc" "ab")

let test_count_overlapping_matches () =
  (check int) "Overlapping matches" 2 (count "aaaa" "aa")

let test_ends_with_true () =
  (check bool) "Ends with true" true (ends_with "hello" "lo")

let test_ends_with_false () =
  (check bool) "Ends with false" false (ends_with "hello" "he")

let test_ends_with_full_match () =
  (check bool) "Full match" true (ends_with "hello" "hello")

let test_ends_with_empty_suffix () =
  (check bool) "Empty suffix" true (ends_with "hello" "")

let test_expand_tabs_default () =
  (check string) "Default tab size" "    test" (expand_tabs "\ttest")

let test_expand_tabs_custom () =
  (check string) "Custom tab size" "  test" (expand_tabs ~tab_size:2 "\ttest")

let test_expand_tabs_no_tabs () =
  (check string) "No tabs" "test" (expand_tabs "test")

let test_find_no_match () =
  (check (option int)) "No match" None (find "hello world" "z")

let test_find_single_match () =
  (check (option int)) "Single match" (Some 0) (find "hello" "he")

let test_find_middle_match () =
  (check (option int)) "Middle match" (Some 2) (find "hello" "ll")

let test_find_end_match () =
  (check (option int)) "End match" (Some 3) (find "hello" "lo")

let test_find_full_string_match () =
  (check (option int)) "Full string match" (Some 0) (find "abc" "abc")

let test_find_empty_substring () =
  (check (option int)) "Empty substring returns 0" (Some 0) (find "abc" "")

let test_find_empty_string () =
  (check (option int)) "Empty string returns -1" None (find "" "a")

let test_find_substring_longer_than_string () =
  (check (option int)) "Substring longer than string" None (find "a" "abc")

let test_contains () =
  check bool "contains substring" true (contains "hello world" "world") ;
  check bool "does not contain substring" false (contains "hello world" "mars") ;
  check bool "empty substring" true (contains "hello" "") ;
  check bool "empty string" false (contains "" "hello")

let test_contains_char () =
  check bool "contains char" true (contains_char "abc" 'b') ;
  check bool "does not contain char" false (contains_char "abc" 'd') ;
  check bool "empty string" false (contains_char "" 'a')

let test_index () =
  check int "index of substring" 6 (index "hello world" "world") ;
  check int "index at start" 0 (index "abc" "a") ;
  try
    let _ = index "hello" "z" in
    fail "should raise Not_found"
  with Not_found -> ()

let test_is_alpha () =
  check bool "only alphabets" true (is_alpha "abcXYZ") ;
  check bool "contains non-alpha" false (is_alpha "abc123") ;
  check bool "empty string" false (is_alpha "")

let test_is_decimal () =
  check bool "only digits" true (is_decimal "123456") ;
  check bool "contains letters" false (is_decimal "123a") ;
  check bool "empty string" false (is_decimal "")

let test_is_alnum () =
  check bool "only alphabets" true (is_alnum "abc") ;
  check bool "only digits" true (is_alnum "123") ;
  check bool "mixed alnum" false (is_alnum "abc123") ;
  check bool "non-alnum" false (is_alnum "abc!") ;
  check bool "empty string" false (is_alnum "")

let test_is_lower () =
  check bool "all lowercase" true (is_lower "abcxyz") ;
  check bool "contains uppercase" false (is_lower "abcXyz") ;
  check bool "contains uncased" true (is_lower "abcxyz 1234") ;
  check bool "empty string" false (is_lower "")

let test_is_upper () =
  check bool "all uppercase" true (is_upper "ABCXYZ") ;
  check bool "contains lowercase" false (is_upper "ABCxyz") ;
  check bool "contains uncased" true (is_upper "ABCXYZ 1234") ;
  check bool "empty string" false (is_upper "")

let test_is_space () =
  check bool "all spaces" true (is_space "   \t\n") ;
  check bool "contains non-space" false (is_space "  a  ") ;
  check bool "empty string" false (is_space "")

let test_is_title () =
  check bool "proper title case" true (is_title "Hello World") ;
  check bool "improper title case" false (is_title "hello world") ;
  check bool "mixed case" false (is_title "Hello world") ;
  check bool "single word title" true (is_title "Hello") ;
  check bool "empty string" false (is_title "")

let test_ljust () =
  check string "No padding when width equals length" "hello" (ljust "hello" 5) ;
  check string "No padding when width less than length" "hello"
    (ljust "hello" 3) ;
  check string "Padding with spaces" "hello   " (ljust "hello" 8) ;
  check string "Padding with custom character" "hello---"
    (ljust ~fill_char:'-' "hello" 8) ;
  check string "Empty string padding" "   " (ljust "" 3) ;
  check string "Width zero" "hello" (ljust "hello" 0)

let test_rjust () =
  check string "No padding when width equals length" "hello" (rjust "hello" 5) ;
  check string "No padding when width less than length" "hello"
    (rjust "hello" 3) ;
  check string "Padding with spaces" "   hello" (rjust "hello" 8) ;
  check string "Padding with custom character" "---hello"
    (rjust ~fill_char:'-' "hello" 8) ;
  check string "Empty string padding" "   " (rjust "" 3) ;
  check string "Width zero" "hello" (rjust "hello" 0)

let test_lower_empty () = check string "Lower empty string" "" (lower "")

let test_lower_all_uppercase () =
  check string "Lower all uppercase" "hello" (lower "HELLO")

let test_lower_mixed_case () =
  check string "Lower mixed case" "hello world" (lower "HeLLo WoRLD")

let test_lower_non_alpha () =
  check string "Lower with non-alphabetic" "1234!@#" (lower "1234!@#")

let test_lower_unicode () = check string "Lower with unicode" "Ñ" (lower "Ñ")

let test_upper_empty () = check string "Upper empty string" "" (upper "")

let test_upper_all_lowercase () =
  check string "Upper all lowercase" "HELLO" (upper "hello")

let test_upper_mixed_case () =
  check string "Upper mixed case" "HELLO WORLD" (upper "HeLLo WoRLD")

let test_upper_non_alpha () =
  check string "Upper with non-alphabetic" "1234!@#" (upper "1234!@#")

let test_upper_unicode () = check string "Upper with unicode" "Ñ" (upper "Ñ")

(* Tests for lstrip *)
let test_lstrip_empty_string () =
  check string "lstrip empty string" "" (lstrip "" " ")

let test_lstrip_no_strip_chars () =
  check string "lstrip no matching chars" "hello" (lstrip "hello" "xyz")

let test_lstrip_spaces () =
  check string "lstrip leading spaces" "hello  " (lstrip "   hello  " " ")

let test_lstrip_custom_chars () =
  check string "lstrip custom chars" "example---" (lstrip "---example---" "-")

let test_lstrip_partial_match () =
  check string "lstrip partial match" "Hello" (lstrip "abcHello" "abc")

let test_lstrip_all_chars () =
  check string "lstrip all chars" "" (lstrip "xxx" "x")

(* Tests for rstrip *)
let test_rstrip_empty_string () =
  check string "rstrip empty string" "" (rstrip "" " ")

let test_rstrip_no_strip_chars () =
  check string "rstrip no matching chars" "hello" (rstrip "hello" "xyz")

let test_rstrip_spaces () =
  check string "rstrip trailing spaces" "   hello" (rstrip "   hello  " " ")

let test_rstrip_custom_chars () =
  check string "rstrip custom chars" "---example" (rstrip "---example---" "-")

let test_rstrip_partial_match () =
  check string "rstrip partial match" "Hello" (rstrip "Helloabc" "abc")

let test_rstrip_all_chars () =
  check string "rstrip all chars" "" (rstrip "yyy" "y")

(* Tests for partition *)

let test_partition_separator_in_middle () =
  check
    (triple string string string)
    "partition middle separator"
    ("hello", "_", "world_example")
    (partition "hello_world_example" "_")

let test_partition_separator_at_start () =
  check
    (triple string string string)
    "partition separator at start" ("", "/", "start") (partition "/start" "/")

let test_partition_separator_at_end () =
  check
    (triple string string string)
    "partition separator at end" ("end", ":", "") (partition "end:" ":")

let test_partition_separator_not_found () =
  check
    (triple string string string)
    "partition separator not found" ("no-separator", "", "")
    (partition "no-separator" "/")

let test_partition_multiple_occurrences () =
  check
    (triple string string string)
    "partition multiple occurrences" ("foo", "::", "bar::baz")
    (partition "foo::bar::baz" "::")

let test_partition_entire_string_separator () =
  check
    (triple string string string)
    "partition entire string as separator" ("", "onlyseparator", "")
    (partition "onlyseparator" "onlyseparator")

let test_partition_empty_string () =
  check
    (triple string string string)
    "partition empty string" ("", "", "") (partition "" ":")

let test_partition_empty_separator () =
  check
    (triple string string string)
    "partition empty separator" ("", "", "string") (partition "string" "")

(* Tests for remove_prefix *)

let test_remove_prefix_present () =
  check string "remove prefix present" "happy" (remove_prefix "unhappy" "un")

let test_remove_prefix_not_present () =
  check string "remove prefix not present" "happy" (remove_prefix "happy" "un")

let test_remove_prefix_full_string () =
  check string "remove prefix full string" "" (remove_prefix "full" "full")

let test_remove_prefix_partial_match () =
  check string "remove prefix partial match" "xprefix"
    (remove_prefix "prefixprefix" "prefi")

let test_remove_prefix_empty_string () =
  check string "remove prefix empty string" "" (remove_prefix "" "pre")

let test_remove_prefix_empty_prefix () =
  check string "remove prefix empty prefix" "string" (remove_prefix "string" "")

let test_remove_prefix_longer_prefix () =
  check string "remove prefix longer prefix" "short"
    (remove_prefix "short" "longprefix")

(* Tests for remove_suffix *)

let test_remove_suffix_present () =
  check string "remove suffix present" "runn" (remove_suffix "running" "ing")

let test_remove_suffix_not_present () =
  check string "remove suffix not present" "run" (remove_suffix "run" "ing")

let test_remove_suffix_full_string () =
  check string "remove suffix full string" ""
    (remove_suffix "complete" "complete")

let test_remove_suffix_partial_match () =
  check string "remove suffix partial match" "testsuffix"
    (remove_suffix "testsuffixsuffix" "suffix")

let test_remove_suffix_empty_string () =
  check string "remove suffix empty string" "" (remove_suffix "" "suf")

let test_remove_suffix_empty_suffix () =
  check string "remove suffix empty suffix" "string" (remove_suffix "string" "")

let test_remove_suffix_longer_suffix () =
  check string "remove suffix longer suffix" "short"
    (remove_suffix "short" "longsuffix")

let () =
  let open Alcotest in
  run "String"
    [ ( "Template strings"
      , [ test_case "No variables" `Quick test_template_no_variables
        ; test_case "Single variable" `Quick test_template_single_variable
        ; test_case "Multiple variables" `Quick test_template_multiple_variables
        ; test_case "Unknown variable" `Quick test_template_unknown_variable
        ; test_case "Adjacent variables" `Quick test_template_adjacent_variables
        ; test_case "Escaped dollar" `Quick test_template_escaped_dollar
        ; test_case "Trailing dollar" `Quick test_template_trailing_dollar
        ; test_case "Non-identifier characters" `Quick
            test_template_non_identifier_characters ] )
    ; ( "Spacing"
      , [ test_case "Center exact width" `Quick test_center_exact_width
        ; test_case "Center wider width" `Quick test_center_wider_width
        ; test_case "Center odd padding" `Quick test_center_odd_padding
        ; test_case "Center with fill char" `Quick test_center_with_fill_char
        ; test_case "Expand tabs default" `Quick test_expand_tabs_default
        ; test_case "Expand tabs custom" `Quick test_expand_tabs_custom
        ; test_case "Expand tabs no tabs" `Quick test_expand_tabs_no_tabs
        ; test_case "Find no match" `Quick test_find_no_match
        ; test_case "ljust" `Quick test_ljust
        ; test_case "rjust" `Quick test_rjust ] )
    ; ( "String contains"
      , [ test_case "Count empty substring" `Quick test_count_empty_substring
        ; test_case "Count no match" `Quick test_count_no_match
        ; test_case "Count single match" `Quick test_count_single_match
        ; test_case "Count multiple matches" `Quick test_count_multiple_matches
        ; test_case "Count overlapping matches" `Quick
            test_count_overlapping_matches
        ; test_case "Ends with true" `Quick test_ends_with_true
        ; test_case "Ends with false" `Quick test_ends_with_false
        ; test_case "Ends with full match" `Quick test_ends_with_full_match
        ; test_case "Ends with empty suffix" `Quick test_ends_with_empty_suffix
        ; test_case "Find single match" `Quick test_find_single_match
        ; test_case "Find middle match" `Quick test_find_middle_match
        ; test_case "Find end match" `Quick test_find_end_match
        ; test_case "Find full string match" `Quick test_find_full_string_match
        ; test_case "Find empty substring" `Quick test_find_empty_substring
        ; test_case "Find empty string" `Quick test_find_empty_string
        ; test_case "Find substring longer than string" `Quick
            test_find_substring_longer_than_string
        ; test_case "contains" `Quick test_contains
        ; test_case "contains_char" `Quick test_contains_char
        ; test_case "lstrip empty string" `Quick test_lstrip_empty_string
        ; test_case "lstrip no matching chars" `Quick test_lstrip_no_strip_chars
        ; test_case "lstrip leading spaces" `Quick test_lstrip_spaces
        ; test_case "lstrip custom chars" `Quick test_lstrip_custom_chars
        ; test_case "lstrip partial match" `Quick test_lstrip_partial_match
        ; test_case "lstrip all chars" `Quick test_lstrip_all_chars
        ; test_case "rstrip empty string" `Quick test_rstrip_empty_string
        ; test_case "rstrip no matching chars" `Quick test_rstrip_no_strip_chars
        ; test_case "rstrip trailing spaces" `Quick test_rstrip_spaces
        ; test_case "rstrip custom chars" `Quick test_rstrip_custom_chars
        ; test_case "rstrip partial match" `Quick test_rstrip_partial_match
        ; test_case "rstrip all chars" `Quick test_rstrip_all_chars
        ; test_case "partition middle separator" `Quick
            test_partition_separator_in_middle
        ; test_case "partition separator at start" `Quick
            test_partition_separator_at_start
        ; test_case "partition separator at end" `Quick
            test_partition_separator_at_end
        ; test_case "partition separator not found" `Quick
            test_partition_separator_not_found
        ; test_case "partition multiple occurrences" `Quick
            test_partition_multiple_occurrences
        ; test_case "partition entire string as separator" `Quick
            test_partition_entire_string_separator
        ; test_case "partition empty string" `Quick test_partition_empty_string
        ; test_case "partition empty separator" `Quick
            test_partition_empty_separator
        ; test_case "remove prefix present" `Quick test_remove_prefix_present
        ; test_case "remove prefix not present" `Quick
            test_remove_prefix_not_present
        ; test_case "remove prefix full string" `Quick
            test_remove_prefix_full_string
        ; test_case "remove prefix partial match" `Quick
            test_remove_prefix_partial_match
        ; test_case "remove prefix empty string" `Quick
            test_remove_prefix_empty_string
        ; test_case "remove prefix empty prefix" `Quick
            test_remove_prefix_empty_prefix
        ; test_case "remove prefix longer prefix" `Quick
            test_remove_prefix_longer_prefix
        ; test_case "remove suffix present" `Quick test_remove_suffix_present
        ; test_case "remove suffix not present" `Quick
            test_remove_suffix_not_present
        ; test_case "remove suffix full string" `Quick
            test_remove_suffix_full_string
        ; test_case "remove suffix partial match" `Quick
            test_remove_suffix_partial_match
        ; test_case "remove suffix empty string" `Quick
            test_remove_suffix_empty_string
        ; test_case "remove suffix empty suffix" `Quick
            test_remove_suffix_empty_suffix
        ; test_case "remove suffix longer suffix" `Quick
            test_remove_suffix_longer_suffix ] )
    ; ( "Case"
      , [ test_case "Capitalize empty" `Quick test_capitalize_empty
        ; test_case "Capitalize single char" `Quick test_capitalize_single_char
        ; test_case "Capitalize word" `Quick test_capitalize_word
        ; test_case "Capitalize already correct" `Quick
            test_capitalize_already_correct
        ; test_case "index" `Quick test_index
        ; test_case "is_alph" `Quick test_is_alpha
        ; test_case "is_decimal" `Quick test_is_decimal
        ; test_case "is_alnum" `Quick test_is_alnum
        ; test_case "is_lower" `Quick test_is_lower
        ; test_case "is_space" `Quick test_is_space
        ; test_case "is_title" `Quick test_is_title
        ; test_case "is_upper" `Quick test_is_upper
        ; test_case "Lower empty string" `Quick test_lower_empty
        ; test_case "Lower all uppercase" `Quick test_lower_all_uppercase
        ; test_case "Lower mixed case" `Quick test_lower_mixed_case
        ; test_case "Lower with non-alphabetic" `Quick test_lower_non_alpha
        ; test_case "Lower with unicode" `Quick test_lower_unicode
        ; test_case "Upper empty string" `Quick test_upper_empty
        ; test_case "Upper all lowercase" `Quick test_upper_all_lowercase
        ; test_case "Upper mixed case" `Quick test_upper_mixed_case
        ; test_case "Upper with non-alphabetic" `Quick test_upper_non_alpha
        ; test_case "Upper with unicode" `Quick test_upper_unicode ] ) ]
