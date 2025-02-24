(** Common string operations *)

open Builtins
open Stdlib.String

(** All lowercase ASCII letters. *)
let ascii_lowercase = "abcdefghijklmnopqrstuvwxyz"

(** All uppercase ASCII letters. *)
let ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(** All ASCII letters (lowercase + uppercase). *)
let ascii_letters = ascii_lowercase ^ ascii_uppercase

(** All decimal digit characters. *)
let digits = "0123456789"

(** All hexadecimal digit characters (lowercase and uppercase). *)
let hexdigits = "0123456789abcdefABCDEF"

(** All octal digit characters. *)
let octdigits = "01234567"

(** All punctuation characters. *)
let punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

(** All whitespace characters. *)
let whitespace = " \t\n\r"

(** All printable characters, including letters, digits, punctuation, and whitespace. *)
let printable = digits ^ ascii_letters ^ punctuation ^ whitespace

(** Checks if a character is printable.

    @param c The character to check.
    @return [true] if [c] is printable, [false] otherwise.
*)
let char_is_printable (c : char) : bool = exists (( = ) c) printable

(** Checks if all characters in a string are printable.

    @param s The string to check.
    @return [true] if all characters in [s] are printable, [false] otherwise.
*)
let string_is_printable (s : string) : bool = for_all char_is_printable s

(** Fills a template string using values from a provided map.
    Template variables are denoted by [$identifier]. Dollar signs can be escaped with [$$].

    @param template The template string containing placeholders.
    @param map A function mapping identifiers to replacement strings.
    @return The template string with placeholders replaced by mapped values.
*)
let template (template : string) (map : string -> string) : string =
  let is_id_char c = exists (( = ) c) (ascii_letters ^ "_") in
  match
    fold_left
      (fun (output, last_char_was_dollar, identifier) c ->
        match c with
        | '$' -> (
            if last_char_was_dollar then
              (output ^ "$", false, None)
            else
              match identifier with
              | None ->
                  (output, true, None)
              | Some id ->
                  (output ^ map id, true, None) )
        | _ -> (
            if last_char_was_dollar then
              (output, false, Some (make 1 c))
            else
              match identifier with
              | None ->
                  (output ^ make 1 c, false, None)
              | Some id ->
                  if is_id_char c then
                    (output, false, Some (id ^ make 1 c))
                  else
                    (output ^ map id ^ make 1 c, false, None) ) )
      ("", false, None) template
  with
  | out, last_dollar, None ->
      out
      ^
      if last_dollar then
        "$"
      else
        ""
  | out, last_dollar, Some id ->
      out ^ map id
      ^
      if last_dollar then
        "$"
      else
        ""

(** Capitalizes the first letter of a string and lowercases the rest.

    @param s The input string.
    @return A string with the first letter capitalized and the rest in lowercase.
*)
let capitalize (s : string) : string =
  if s = "" then
    ""
  else
    uppercase_ascii (sub s 0 1) ^ lowercase_ascii (sub s 1 (length s - 1))

(** Centers a string in a field of a given width using a specified fill character.

    @param s The string to center.
    @param w The target width.
    @param fill_char The character used for padding (default is space).
    @return The centered string.
*)
let center ?(fill_char : char = ' ') (s : string) (w : int) : string =
  if length s >= w then
    s
  else
    let w = w - length s in
    let even = w mod 2 = 0 in
    make
      ( (w / 2)
      +
      if even then
        0
      else
        1 )
      fill_char
    ^ s
    ^ make (w / 2) fill_char

(** Left-justifies a string by padding it on the right with a specified character.

    @param s The string to justify.
    @param w The total width of the resulting string.
    @param fill_char (optional) The character used for padding. Defaults to a space.
    @return The left-justified string. If [s] is longer than [w], [s] is returned unchanged.
*)
let ljust ?(fill_char : char = ' ') (s : string) (w : int) : string =
  if length s >= w then
    s
  else
    s ^ make (w - length s) fill_char

(** Right-justifies a string by padding it on the left with a specified character.

    @param s The string to justify.
    @param w The total width of the resulting string.
    @param fill_char (optional) The character used for padding. Defaults to a space.
    @return The right-justified string. If [s] is longer than [w], [s] is returned unchanged.
*)
let rjust ?(fill_char : char = ' ') (s : string) (w : int) : string =
  if length s >= w then
    s
  else
    make (w - length s) fill_char ^ s

(** Counts the number of non-overlapping occurrences of a substring in a string.

    @param s The string to search in.
    @param sub The substring to count.
    @return The number of occurrences of [sub] in [s].
*)
let count (s : string) (sub : string) : int =
  let sub_len = length sub in
  if sub_len = 0 then
    0
  else
    let rec aux idx acc =
      if idx > length s - sub_len then
        acc
      else if Stdlib.String.sub s idx sub_len = sub then
        aux (idx + sub_len) (acc + 1)
      else
        aux (idx + 1) acc
    in
    aux 0 0

(** Checks if a string ends with a given suffix.

    @param base The base string.
    @param suffix The suffix string.
    @return [true] if [base] ends with [suffix], [false] otherwise.
*)
let ends_with (base : string) (suffix : string) = ends_with ~suffix base

(** Checks if a string starts with a given prefix.

    @param base The base string.
    @param prefix The prefix string.
    @return [true] if [base] starts with [prefix], [false] otherwise.
*)
let starts_with (base : string) (prefix : string) = starts_with ~prefix base

(** Replaces all tab characters in a string with a specified number of spaces.

    @param s The input string.
    @param tab_size The number of spaces to replace each tab with (default is 4).
    @return The modified string with tabs expanded.
*)
let expand_tabs ?(tab_size : int = 4) (s : string) : string =
  fold_left
    (fun a c ->
      if c = '\t' then
        a ^ make tab_size ' '
      else
        a ^ make 1 c )
    "" s

(** Finds the first occurrence of a substring in a string.

    @param s The string to search in.
    @param sub The substring to search for.
    @return [Some index] of the first occurrence or [None] if not found.
*)
let find (s : string) (sub : string) : int option =
  let s_len = length s in
  let sub_len = length sub in
  if sub_len = 0 then
    Some 0
  else
    let rec aux idx =
      if idx > s_len - sub_len then
        None
      else if Stdlib.String.sub s idx sub_len = sub then
        Some idx
      else
        aux (idx + 1)
    in
    aux 0

(** Checks if a string contains a given substring.

    @param s The string to check.
    @param sub The substring to search for.
    @return [true] if [sub] is found in [s], [false] otherwise.
*)
let contains (s : string) (sub : string) : bool = find s sub != None

(** Checks if a string contains a given character.

    @param s The string to check.
    @param c The character to search for.
    @return [true] if [c] is found in [s], [false] otherwise.
*)
let contains_char (s : string) (c : char) : bool = find s (make 1 c) != None

(** Finds the index of the first occurrence of a substring, or raises [Not_found].

    @param s The string to search in.
    @param sub The substring to search for.
    @return The index of the first occurrence.
    @raise Not_found If [sub] is not found in [s].
*)
let index (s : string) (sub : string) : int =
  match find s sub with None -> raise Not_found | Some n -> n

(** Checks if a string consists only of alphabetic characters.

    @param s The string to check.
    @return [true] if all characters are alphabetic, [false] otherwise.
*)
let is_alpha (s : string) : bool =
  (not (s = "")) && for_all (contains_char ascii_letters) s

(** Checks if a string consists only of decimal digits.

    @param s The string to check.
    @return [true] if all characters are digits, [false] otherwise.
*)
let is_decimal (s : string) : bool =
  (not (s = "")) && for_all (contains_char digits) s

(** Checks if a string consists only of alphanumeric characters.

    @param s The string to check.
    @return [true] if the string is alphanumeric, [false] otherwise.
*)
let is_alnum (s : string) : bool = is_alpha s || is_decimal s

(** Checks if a string consists only of lowercase and uncased letters.

    @param s The string to check.
    @return [true] if all characters are lowercase, [false] otherwise.
*)
let is_lower (s : string) : bool =
  (not (s = ""))
  && for_all
       (contains_char (ascii_lowercase ^ digits ^ punctuation ^ whitespace))
       s

(** Converts all alphabetic characters in a string to lowercase.

    Non-alphabetic and Unicode characters remain unchanged.

    @param s The input string to convert.
    @return A new string with all alphabetic characters in lowercase.

    @example lower "Hello WORLD!" = "hello world!"
    @example lower "123_ABC_def" = "123_abc_def"
*)
let lower (s : string) : string =
  fold_left
    (fun a c ->
      let s' = make 1 c in
      match find ascii_letters s' with
      | None ->
          a ^ s'
      | Some i ->
          a
          ^ make 1
              (get ascii_lowercase
                 ( if i - 26 < 0 then
                     i
                   else
                     i - 26 ) ) )
    "" s

(** Checks if a string consists only of whitespace characters.

    @param s The string to check.
    @return [true] if all characters are whitespace, [false] otherwise.
*)
let is_space (s : string) : bool =
  (not (s = "")) && for_all (contains_char whitespace) s

(** Checks if a string is in title case (each word capitalized).

    @param s The string to check.
    @return [true] if the string is in title case, [false] otherwise.
*)
let is_title (s : string) : bool =
  (not (s = ""))
  && fst
       (fold_left
          (fun (is_title, last_was_uncased) c ->
            ( ( is_title
              &&
              if contains_char ascii_uppercase c then
                last_was_uncased
              else if contains_char ascii_lowercase c then
                not last_was_uncased
              else
                true )
            , contains_char (whitespace ^ punctuation ^ digits) c ) )
          (true, true) s )

(** Checks if a string consists only of uppercase and uncased letters.

    @param s The string to check.
    @return [true] if all characters are uppercase, [false] otherwise.
*)
let is_upper (s : string) : bool =
  (not (s = ""))
  && for_all
       (contains_char (ascii_uppercase ^ digits ^ punctuation ^ whitespace))
       s

(** Converts all alphabetic characters in a string to uppercase.

    Non-alphabetic and Unicode characters remain unchanged.

    @param s The input string to convert.
    @return A new string with all alphabetic characters in uppercase.

    @example upper "Hello world!" = "HELLO WORLD!"
    @example upper "abc_123_DEF" = "ABC_123_DEF"
*)
let upper (s : string) : string =
  fold_left
    (fun a c ->
      let s' = make 1 c in
      match find ascii_letters s' with
      | None ->
          a ^ s'
      | Some i ->
          a
          ^ make 1
              (get ascii_uppercase
                 ( if i - 26 < 0 then
                     i
                   else
                     i - 26 ) ) )
    "" s

(** Joins a list of strings into a single string using a specified separator.

    @param sep The separator string to insert between each element.
    @param lst The list of strings to join.
    @return A single concatenated string with [sep] between elements of [lst].
    If [lst] is empty, returns an empty string.
    
    @example join ", " ["a"; "b"; "c"] = "a, b, c"
    @example join "" ["hello"; "world"] = "helloworld"
*)
let join = concat

(** Removes all leading characters in [chars] from the given string [s].

    @param s The input string to process.
    @param chars A string containing all characters to strip from the start of [s].
    @return A new string with the specified leading characters removed.

    @example lstrip "   hello  " " " = "hello  "
    @example lstrip "---example---" "-" = "example---"
    @example lstrip "abcHello" "abc" = "Hello"
*)
let lstrip (s : string) (chars : string) : string =
  fst
    (fold_left
       (fun (a, stop) c ->
         if stop then
           (a ^ make 1 c, stop)
         else if contains_char chars c then
           ("", false)
         else
           (make 1 c, true) )
       ("", false) s )

(** Removes all trailing characters in [chars] from the given string [s].

    @param s The input string to process.
    @param chars A string containing all characters to strip from the end of [s].
    @return A new string with the specified trailing characters removed.

    @example rstrip "   hello  " " " = "   hello"
    @example rstrip "---example---" "-" = "---example"
    @example rstrip "Helloabc" "abc" = "Hello"
*)
let rstrip (s : string) (chars : string) : string =
  reverse_string
    (fst
       (fold_left
          (fun (a, stop) c ->
            if stop then
              (a ^ make 1 c, stop)
            else if contains_char chars c then
              ("", false)
            else
              (make 1 c, true) )
          ("", false) (reverse_string s) ) )

(** Splits a string [s] into a 3-tuple based on the first occurrence of the separator [sep].

    The result is a tuple [(before, sep, after)], where:
    - [before] is the substring before the first occurrence of [sep].
    - [sep] is the separator itself (if found).
    - [after] is the substring after the first occurrence of [sep].

    If the separator [sep] is not found, returns [(s, "", "")].

    @param s The input string to partition.
    @param sep The separator string to search for.
    @return A tuple [(before, sep, after)].

    @example partition "hello_world_example" "_" = ("hello", "_", "world_example")
    @example partition "no-separator" "/" = ("no-separator", "", "")
    @example partition "foo::bar::baz" "::" = ("foo", "::", "bar::baz")
    @example partition "onlyseparator" "onlyseparator" = ("", "onlyseparator", "")
*)
let partition (s : string) (sep : string) : string * string * string =
  match find s sep with
  | None ->
      (s, "", "")
  | Some idx ->
      ( sub s 0 idx
      , sep
      , sub s (idx + length sep) (length s - (idx + length sep)) )

(** Removes the specified [prefix] from the string [s] if it starts with [prefix].

    If [s] does not start with [prefix], returns [s] unchanged.

    @param s The input string.
    @param prefix The prefix to remove.
    @return The string without the prefix if present, otherwise the original string.

    @example remove_prefix "unhappy" "un" = "happy"
    @example remove_prefix "happy" "un" = "happy"
    @example remove_prefix "prefixprefix" "prefix" = "prefix"
*)
let remove_prefix (s : string) (prefix : string) : string =
  if starts_with s prefix then
    sub s (length prefix) (length s - length prefix)
  else
    s

(** Removes the specified [suffix] from the string [s] if it ends with [suffix].

    If [s] does not end with [suffix], returns [s] unchanged.

    @param s The input string.
    @param suffix The suffix to remove.
    @return The string without the suffix if present, otherwise the original string.

    @example remove_suffix "running" "ing" = "runn"
    @example remove_suffix "run" "ing" = "run"
    @example remove_suffix "testsuffixsuffix" "suffix" = "testsuffix"
*)
let remove_suffix (s : string) (suffix : string) : string =
  if ends_with s suffix then
    sub s 0 (length s - length suffix)
  else
    s
