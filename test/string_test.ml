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

let test_find_no_match () = (check int) "No match" (-1) (find "hello world" "z")

let test_find_single_match () = (check int) "Single match" 0 (find "hello" "he")

let test_find_middle_match () = (check int) "Middle match" 2 (find "hello" "ll")

let test_find_end_match () = (check int) "End match" 3 (find "hello" "lo")

let test_find_full_string_match () =
  (check int) "Full string match" 0 (find "abc" "abc")

let test_find_empty_substring () =
  (check int) "Empty substring returns 0" 0 (find "abc" "")

let test_find_empty_string () =
  (check int) "Empty string returns -1" (-1) (find "" "a")

let test_find_substring_longer_than_string () =
  (check int) "Substring longer than string" (-1) (find "a" "abc")

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
            test_template_non_identifier_characters
        ; test_case "Capitalize empty" `Quick test_capitalize_empty
        ; test_case "Capitalize single char" `Quick test_capitalize_single_char
        ; test_case "Capitalize word" `Quick test_capitalize_word
        ; test_case "Capitalize already correct" `Quick
            test_capitalize_already_correct
        ; test_case "Center exact width" `Quick test_center_exact_width
        ; test_case "Center wider width" `Quick test_center_wider_width
        ; test_case "Center odd padding" `Quick test_center_odd_padding
        ; test_case "Center with fill char" `Quick test_center_with_fill_char
        ; test_case "Count empty substring" `Quick test_count_empty_substring
        ; test_case "Count no match" `Quick test_count_no_match
        ; test_case "Count single match" `Quick test_count_single_match
        ; test_case "Count multiple matches" `Quick test_count_multiple_matches
        ; test_case "Count overlapping matches" `Quick
            test_count_overlapping_matches
        ; test_case "Ends with true" `Quick test_ends_with_true
        ; test_case "Ends with false" `Quick test_ends_with_false
        ; test_case "Ends with full match" `Quick test_ends_with_full_match
        ; test_case "Ends with empty suffix" `Quick test_ends_with_empty_suffix
        ; test_case "Expand tabs default" `Quick test_expand_tabs_default
        ; test_case "Expand tabs custom" `Quick test_expand_tabs_custom
        ; test_case "Expand tabs no tabs" `Quick test_expand_tabs_no_tabs
        ; test_case "Find no match" `Quick test_find_no_match
        ; test_case "Find single match" `Quick test_find_single_match
        ; test_case "Find middle match" `Quick test_find_middle_match
        ; test_case "Find end match" `Quick test_find_end_match
        ; test_case "Find full string match" `Quick test_find_full_string_match
        ; test_case "Find empty substring" `Quick test_find_empty_substring
        ; test_case "Find empty string" `Quick test_find_empty_string
        ; test_case "Find substring longer than string" `Quick
            test_find_substring_longer_than_string ] ) ]
