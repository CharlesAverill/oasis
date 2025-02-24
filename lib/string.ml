(** Common string operations *)

open Builtins
open Stdlib.String

(** All lowercase ascii letters *)
let ascii_lowercase = "abcdefghijklmnopqrstuvwxyz"

(** All uppercase ascii letters *)
let ascii_uppercase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(** All ascii letters *)
let ascii_letters = ascii_lowercase ^ ascii_uppercase

(** All decimal digit characters *)
let digits = "0123456789"

(** All hexadecimal digit characters *)
let hexdigits = "0123456789abcdefABCDEF"

(** All octal digit characters *)
let octdigits = "01234567"

(** All puncutation characters *)
let punctuation = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

(** All whitespace characters *)
let whitespace = " \t\n\r"

(** All printable characters *)
let printable = digits ^ ascii_letters ^ punctuation ^ whitespace

(** Check if a character is printable *)
let char_is_printable (c : char) : bool = exists (( = ) c) printable

(** Check if a string is printable *)
let string_is_printable (s : string) : bool = for_all char_is_printable s

(** Given a template string and a map, fill in the template string. Map keys are
    denoted by $identifier, and dollar signs are escaped like $$
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

(** Capitalize first letter of string, lowercase the rest *)
let capitalize (s : string) : string =
  if s = "" then
    ""
  else
    uppercase_ascii (sub s 0 1) ^ lowercase_ascii (sub s 1 (length s - 1))

(** Center a string with spaces given a target width *)
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

(** Count non-overlapping occurances of a substring *)
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

(** endswith base suffix checks if the base string ends with the suffix string *)
let ends_with (base : string) (suffix : string) = ends_with ~suffix base

(** Replace tab characters with a specified number of spaces *)
let expand_tabs ?(tab_size : int = 4) (s : string) : string =
  fold_left
    (fun a c ->
      if c = '\t' then
        a ^ make tab_size ' '
      else
        a ^ make 1 c )
    "" s

(** Find first occurance of substring in string *)
let find (s : string) (sub : string) : int =
  let s_len = length s in
  let sub_len = length sub in
  if sub_len = 0 then
    0
  else
    let rec aux idx =
      if idx > s_len - sub_len then
        -1
      else if Stdlib.String.sub s idx sub_len = sub then
        idx
      else
        aux (idx + 1)
    in
    aux 0
