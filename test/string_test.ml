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

let test_strip_empty_string () =
  check string "strip empty string" "" (strip "" " ")

let test_strip_no_strip_chars () =
  check string "strip no matching chars" "hello" (strip "hello" "xyz")

let test_strip_spaces () =
  check string "strip leading spaces" "hello  " (strip "   hello  " " ")

let test_strip_custom_chars () =
  check string "strip custom chars" "example---" (strip "---example---" "-")

let test_strip_partial_match () =
  check string "strip partial match" "Hello" (strip "abcHello" "abc")

let test_strip_all_chars () =
  check string "strip all chars" "" (strip "xxx" "x")

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

let test_replace_single_occurrence () =
  check string "replace single occurrence" "hello OCaml"
    (replace "hello world" "world" "OCaml")

let test_replace_multiple_occurrences () =
  check string "replace multiple occurrences" "bbb" (replace "aaa" "a" "b")

let test_replace_limited_occurrences () =
  check string "replace limited occurrences" "bba"
    (replace ~count:2 "aaa" "a" "b")

let test_replace_no_occurrence () =
  check string "replace no occurrence" "hello" (replace "hello" "x" "y")

let test_replace_empty_string () =
  check string "replace empty string" "" (replace "" "a" "b")

let test_replace_empty_old () =
  check string "replace empty old string" "abcd" (replace "abcd" "" "x")

let test_replace_empty_subst () =
  check string "replace empty subst" "bc" (replace "abc" "a" "")

let test_replace_full_string () =
  check string "replace full string" "replacement"
    (replace "old" "old" "replacement")

let test_replace_partial_match () =
  check string "replace partial match" "bbca" (replace "abca" "a" "b" ~count:1)

let test_replace_count_zero () =
  check string "replace count zero" "aaa" (replace ~count:0 "aaa" "a" "b")

let test_rfind_basic () =
  check (option int) "rfind basic" (Some 12) (rfind "hello world hello" "hello")

let test_rfind_multiple_occurrences () =
  check (option int) "rfind multiple occurrences" (Some 6)
    (rfind "abcabcabc" "abc")

let test_rfind_no_occurrence () =
  check (option int) "rfind no occurrence" None (rfind "test" "xyz")

let test_rfind_full_match () =
  check (option int) "rfind full match" (Some 0) (rfind "string" "string")

let test_rfind_empty_substring () =
  check (option int) "rfind empty substring"
    (Some (String.length "abc"))
    (rfind "abc" "")

let test_rfind_substring_at_end () =
  check (option int) "rfind substring at end" (Some 3) (rfind "abcd" "d")

let test_rfind_substring_at_start () =
  check (option int) "rfind substring at start" (Some 0) (rfind "abcd" "a")

let test_rfind_substring_longer_than_string () =
  check (option int) "rfind substring longer than string" None
    (rfind "abc" "abcd")

let test_split_basic () =
  check (list string) "split basic" ["a"; "b"; "c"] (split "a,b,c" ",")

let test_split_multiple_char_separator () =
  check (list string) "split multi-char sep"
    ["hello"; "world"; "again"]
    (split "hello::world::again" "::")

let test_split_no_separator () =
  check (list string) "split no sep" ["nosplit"] (split "nosplit" ",")

let test_split_empty_string () =
  check (list string) "split empty string" [""] (split "" ",")

let test_split_only_separator () =
  check (list string) "split only sep" [""; ""] (split "," ",")

let test_split_trailing_separator () =
  check (list string) "split trailing sep" ["a"; "b"; ""] (split "a,b," ",")

let test_split_leading_separator () =
  check (list string) "split leading sep" [""; "a"; "b"] (split ",a,b" ",")

let test_split_repeated_separator () =
  check (list string) "split repeated sep" ["a"; ""; "b"] (split "a,,b" ",")

let test_split_full_match_separator () =
  check (list string) "split full match sep" [""; ""] (split "::" "::")

let test_swapcase_mixed_case () =
  check string "swapcase mixed case" "hELLO wORLD" (swapcase "Hello World")

let test_swapcase_all_upper () =
  check string "swapcase all upper" "lowercase" (swapcase "LOWERCASE")

let test_swapcase_all_lower () =
  check string "swapcase all lower" "UPPERCASE" (swapcase "uppercase")

let test_swapcase_non_alpha () =
  check string "swapcase non-alpha" "1234!@# $" (swapcase "1234!@# $")

let test_swapcase_empty_string () =
  check string "swapcase empty string" "" (swapcase "")

let test_swapcase_mixed_alpha_nonalpha () =
  check string "swapcase mixed alpha-nonalpha" "tHIS 123 iS!@#"
    (swapcase "This 123 Is!@#")

let test_swapcase_single_upper_char () =
  check string "swapcase single upper" "a" (swapcase "A")

let test_swapcase_single_lower_char () =
  check string "swapcase single lower" "Z" (swapcase "z")

let test_swapcase_unicode () =
  check string "swapcase unicode" "ß" (swapcase "ß")
(* Should remain unchanged if not in ascii_letters *)

let test_title_basic () =
  check string "title basic" "Hello World" (title "hello world")

let test_title_mixed_case () =
  check string "title mixed case" "Hello World" (title "hElLo wORld")

let test_title_single_word () =
  check string "title single word" "Hello" (title "hello")

let test_title_empty_string () = check string "title empty string" "" (title "")

let test_title_multiple_spaces () =
  check string "title multiple spaces" "Hello   World  From   Ocaml"
    (title "hello   world  from   ocaml")

let test_title_already_title_case () =
  check string "title already title case" "Hello World" (title "Hello World")

let test_title_with_numbers () =
  check string "title with numbers" "Hello 123 World" (title "hello 123 world")

let test_title_with_punctuation () =
  check string "title with punctuation" "Hello, World!" (title "hello, world!")

let test_title_uppercase_input () =
  check string "title uppercase input" "Hello World" (title "HELLO WORLD")

let test_title_lowercase_input () =
  check string "title lowercase input" "Hello World" (title "hello world")

let test_title_single_char_words () =
  check string "title single char words" "A B C D" (title "a b c d")

let test_title_trailing_leading_spaces () =
  check string "title trailing leading spaces" "   Hello World   "
    (title "   hello world   ")

let test_translate_identity () =
  check string "translate identity" "hello" (translate "hello" Fun.id)

let test_translate_uppercase () =
  check string "translate uppercase" "HELLO"
    (translate "hello" Char.uppercase_ascii)

let test_translate_lowercase () =
  check string "translate lowercase" "hello"
    (translate "HELLO" Char.lowercase_ascii)

let test_translate_shift_char () =
  check string "translate shift char" "ifmmp"
    (translate "hello" (fun c -> Char.chr (Char.code c + 1)))

let test_translate_replace_vowels () =
  check string "translate replace vowels" "h*ll* w*rld"
    (translate "hello world" (fun c ->
         match c with 'a' | 'e' | 'i' | 'o' | 'u' -> '*' | _ -> c ) )

let test_translate_empty_string () =
  check string "translate empty string" "" (translate "" Char.uppercase_ascii)

let test_translate_non_alpha () =
  check string "translate non-alpha" "!@#$%" (translate "!@#$%" (fun c -> c))

let test_translate_reverse_case () =
  check string "translate reverse case" "hELLO wORLD"
    (translate "Hello World" (fun c ->
         if Char.uppercase_ascii c = c then
           Char.lowercase_ascii c
         else
           Char.uppercase_ascii c ) )

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
        ; test_case "rfind basic" `Quick test_rfind_basic
        ; test_case "rfind multiple occurrences" `Quick
            test_rfind_multiple_occurrences
        ; test_case "rfind no occurrence" `Quick test_rfind_no_occurrence
        ; test_case "rfind full match" `Quick test_rfind_full_match
        ; test_case "rfind empty substring" `Quick test_rfind_empty_substring
        ; test_case "rfind substring at end" `Quick test_rfind_substring_at_end
        ; test_case "rfind substring at start" `Quick
            test_rfind_substring_at_start
        ; test_case "rfind substring longer than string" `Quick
            test_rfind_substring_longer_than_string ] )
    ; ( "Replacement"
      , [ test_case "strip empty string" `Quick test_strip_empty_string
        ; test_case "strip no matching chars" `Quick test_strip_no_strip_chars
        ; test_case "strip leading spaces" `Quick test_strip_spaces
        ; test_case "strip custom chars" `Quick test_strip_custom_chars
        ; test_case "strip partial match" `Quick test_strip_partial_match
        ; test_case "strip all chars" `Quick test_strip_all_chars
        ; test_case "rstrip empty string" `Quick test_rstrip_empty_string
        ; test_case "rstrip no matching chars" `Quick test_rstrip_no_strip_chars
        ; test_case "rstrip trailing spaces" `Quick test_rstrip_spaces
        ; test_case "rstrip custom chars" `Quick test_rstrip_custom_chars
        ; test_case "rstrip partial match" `Quick test_rstrip_partial_match
        ; test_case "rstrip all chars" `Quick test_rstrip_all_chars
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
            test_remove_suffix_longer_suffix
        ; test_case "replace single occurrence" `Quick
            test_replace_single_occurrence
        ; test_case "replace multiple occurrences" `Quick
            test_replace_multiple_occurrences
        ; test_case "replace limited occurrences" `Quick
            test_replace_limited_occurrences
        ; test_case "replace no occurrence" `Quick test_replace_no_occurrence
        ; test_case "replace empty string" `Quick test_replace_empty_string
        ; test_case "replace empty old substring" `Quick test_replace_empty_old
        ; test_case "replace empty subst" `Quick test_replace_empty_subst
        ; test_case "replace full string" `Quick test_replace_full_string
        ; test_case "replace partial match" `Quick test_replace_partial_match
        ; test_case "replace count zero" `Quick test_replace_count_zero
        ; test_case "split basic" `Quick test_split_basic
        ; test_case "split multi-char sep" `Quick
            test_split_multiple_char_separator
        ; test_case "split no sep" `Quick test_split_no_separator
        ; test_case "split empty string" `Quick test_split_empty_string
        ; test_case "split only sep" `Quick test_split_only_separator
        ; test_case "split trailing sep" `Quick test_split_trailing_separator
        ; test_case "split leading sep" `Quick test_split_leading_separator
        ; test_case "split repeated sep" `Quick test_split_repeated_separator
        ; test_case "split full match sep" `Quick
            test_split_full_match_separator
        ; test_case "translate identity" `Quick test_translate_identity
        ; test_case "translate uppercase" `Quick test_translate_uppercase
        ; test_case "translate lowercase" `Quick test_translate_lowercase
        ; test_case "translate shift char" `Quick test_translate_shift_char
        ; test_case "translate replace vowels" `Quick
            test_translate_replace_vowels
        ; test_case "translate empty string" `Quick test_translate_empty_string
        ; test_case "translate non-alpha" `Quick test_translate_non_alpha
        ; test_case "translate reverse case" `Quick test_translate_reverse_case
        ] )
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
        ; test_case "Upper with unicode" `Quick test_upper_unicode
        ; test_case "swapcase mixed case" `Quick test_swapcase_mixed_case
        ; test_case "swapcase all upper" `Quick test_swapcase_all_upper
        ; test_case "swapcase all lower" `Quick test_swapcase_all_lower
        ; test_case "swapcase non-alpha" `Quick test_swapcase_non_alpha
        ; test_case "swapcase empty string" `Quick test_swapcase_empty_string
        ; test_case "swapcase mixed alpha-nonalpha" `Quick
            test_swapcase_mixed_alpha_nonalpha
        ; test_case "swapcase single upper" `Quick
            test_swapcase_single_upper_char
        ; test_case "swapcase single lower" `Quick
            test_swapcase_single_lower_char
        ; test_case "swapcase unicode" `Quick test_swapcase_unicode
        ; test_case "title basic" `Quick test_title_basic
        ; test_case "title mixed case" `Quick test_title_mixed_case
        ; test_case "title single word" `Quick test_title_single_word
        ; test_case "title empty string" `Quick test_title_empty_string
        ; test_case "title multiple spaces" `Quick test_title_multiple_spaces
        ; test_case "title already title case" `Quick
            test_title_already_title_case
        ; test_case "title with numbers" `Quick test_title_with_numbers
        ; test_case "title with punctuation" `Quick test_title_with_punctuation
        ; test_case "title uppercase input" `Quick test_title_uppercase_input
        ; test_case "title lowercase input" `Quick test_title_lowercase_input
        ; test_case "title single char words" `Quick
            test_title_single_char_words
        ; test_case "title trailing leading spaces" `Quick
            test_title_trailing_leading_spaces ] ) ]
