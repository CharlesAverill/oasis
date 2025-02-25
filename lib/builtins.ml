(** Common functions you need everywhere *)

open Logging

(** Absolute value of integers *)
let abs (x : int) =
  if x < 0 then
    -x
  else
    x

(** Absolute value of floats *)
let absf (x : float) =
  if x < 0. then
    -1. *. x
  else
    x

(** Power of ints *)
let pow a b = float_of_int a ** float_of_int b

(** Power of floats *)
let powf = ( ** )

(** Get pair of dividend and modulo of two integers *)
let divmod (x : int) (y : int) = (x / y, x mod y)

(** Get pair of dividend and modulo of two floats 
        https://stackoverflow.com/a/37977740
    *)
let divmodf (x : float) (y : float) = (x /. y, x -. (x /. y *. y))

(* Round a number to a specified number of decimal points 
   https://github.com/dbuenzli/gg/blob/8f761c278d0b2ee2adb94f9fbc033f1bfd76e536/src/gg.ml#L123-L126
*)
let round_dfrac d x =
  if x -. Float.round x = 0. then
    x
  else
    let m = 10. ** float d in
    floor ((x *. m) +. 0.5) /. m

(** Complex numbers *)
module Complex = struct
  type t = {real: float; imaginary: float}

  let zero = {real= 0.; imaginary= 0.}

  (** Real part of a complex number *)
  let real c = c.real

  (** Imaginary part of a complex number *)
  let imag c = c.imaginary

  (** Get complex representation of float *)
  let complex_of_float f = {real= f; imaginary= 0.}

  (** Get imaginary representation of float *)
  let imag_of_float f = {real= 0.; imaginary= f}

  (** Add complex numbers *)
  let add (c1 : t) (c2 : t) : t =
    {real= c1.real +. c2.real; imaginary= c1.imaginary +. c2.imaginary}

  let ( +^ ) = add

  (** Subtract complex numbers *)
  let sub (c1 : t) (c2 : t) : t =
    {real= c1.real -. c2.real; imaginary= c1.imaginary -. c2.imaginary}

  let ( -^ ) = sub

  (** Multiply complex numbers *)
  let mul (c1 : t) (c2 : t) : t =
    { real= (c1.real *. c2.real) -. (c2.imaginary *. c1.imaginary)
    ; imaginary= (c1.real *. c2.imaginary) +. (c2.real *. c1.imaginary) }

  let ( *^ ) = mul

  (** Conjugate of a complex number *)
  let conj (c : t) = {real= c.real; imaginary= -.c.imaginary}

  let ( ~^ ) = conj

  (** Whether a complex number is real *)
  let is_real (c : t) : bool = c = ~^c

  (** Absolute value of complex number *)
  let absc (c : t) : float = Float.sqrt (c *^ ~^c).real

  (** Reciprocal of a complex number *)
  let recip (c : t) : t =
    { real= c.real /. ((c.real *. c.real) +. (c.imaginary *. c.imaginary))
    ; imaginary=
        -.(c.imaginary /. ((c.real *. c.real) +. (c.imaginary *. c.imaginary)))
    }

  (** Divide complex numbers *)
  let div (c1 : t) (c2 : t) : t = c1 *^ recip c2

  let ( /^ ) = div
end

type complex = Complex.t

(** Get number in bases up to 36 *)
let rec base_number ?(first : bool = true) (prefix : string) (base : int)
    (x : int) : string =
  if first then
    let nstr = base_number ~first:false prefix base (abs x) in
    (* Reverse recursively-built string *)
    let nstr =
      (fun s ->
        Stdlib.String.init (Stdlib.String.length s) (fun n ->
            Stdlib.String.get s (Stdlib.String.length s - n - 1) ) )
        nstr
    in
    (* Remove leading zeros *)
    let nstr =
      if x != 0 && Stdlib.String.exists (( = ) '0') nstr then
        let zero_idx = Stdlib.String.index nstr '0' in
        Stdlib.String.sub nstr (zero_idx + 1)
          (Stdlib.String.length nstr - zero_idx - 1)
      else
        nstr
    in
    (* Check for negative numbers *)
    ( if x < 0 then
        "-"
      else
        "" )
    (* Add prefix *)
    ^ prefix
    ^ nstr
  else if x = 0 then
    "0"
  else
    let digits =
      Stdlib.String.sub "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 base
    in
    let digit = Stdlib.String.get digits (x mod base) in
    Stdlib.String.make 1 digit ^ base_number ~first:false prefix base (x / base)

(** Binary string of number *)
let bin = base_number "0b" 2

(** Octal string of number *)
let oct = base_number "0o" 8

(** Hexadecimal string of number *)
let hex = base_number "0x" 16

(** Get string represented by a unicode code point *)
let chr (code_point : int) : string =
  if code_point < 0x80 then
    Stdlib.String.make 1 (Char.unsafe_chr code_point)
  else if code_point < 0x800 then
    Stdlib.String.make 2 (Char.unsafe_chr (0xC0 lor (code_point lsr 6)))
    ^ Stdlib.String.make 1 (Char.unsafe_chr (0x80 lor (code_point land 0x3F)))
  else if code_point < 0x10000 then
    Stdlib.String.make 1 (Char.unsafe_chr (0xE0 lor (code_point lsr 12)))
    ^ Stdlib.String.make 1
        (Char.unsafe_chr (0x80 lor ((code_point lsr 6) land 0x3F)))
    ^ Stdlib.String.make 1 (Char.unsafe_chr (0x80 lor (code_point land 0x3F)))
  else if code_point < 0x110000 then
    Stdlib.String.make 1 (Char.unsafe_chr (0xF0 lor (code_point lsr 18)))
    ^ Stdlib.String.make 1
        (Char.unsafe_chr (0x80 lor ((code_point lsr 12) land 0x3F)))
    ^ Stdlib.String.make 1
        (Char.unsafe_chr (0x80 lor ((code_point lsr 6) land 0x3F)))
    ^ Stdlib.String.make 1 (Char.unsafe_chr (0x80 lor (code_point land 0x3F)))
  else
    invalid_arg "Invalid Unicode code point"

(** Get unicode code point of a string *)
let ord (s : string) : int =
  let len = Stdlib.String.length s in
  if len = 1 then
    Char.code (Stdlib.String.get s 0)
  else if len = 2 then
    let byte1 = Char.code (Stdlib.String.get s 0) in
    let byte2 = Char.code (Stdlib.String.get s 1) in
    ((byte1 land 0x1F) lsl 6) lor (byte2 land 0x3F)
  else if len = 3 then
    let byte1 = Char.code (Stdlib.String.get s 0) in
    let byte2 = Char.code (Stdlib.String.get s 1) in
    let byte3 = Char.code (Stdlib.String.get s 2) in
    ((byte1 land 0x0F) lsl 12)
    lor ((byte2 land 0x3F) lsl 6)
    lor (byte3 land 0x3F)
  else if len = 4 then
    let byte1 = Char.code (Stdlib.String.get s 0) in
    let byte2 = Char.code (Stdlib.String.get s 1) in
    let byte3 = Char.code (Stdlib.String.get s 2) in
    let byte4 = Char.code (Stdlib.String.get s 3) in
    ((byte1 land 0x07) lsl 18)
    lor ((byte2 land 0x3F) lsl 12)
    lor ((byte3 land 0x3F) lsl 6)
    lor (byte4 land 0x3F)
  else
    invalid_arg "Invalid UTF-8 byte sequence"

(** Get the formatted string for a given object *)
let format = Printf.sprintf

(** Dictionaries *)
module Dict = struct
  type ('a, 'b) t = {_f: 'a -> 'b; _keys: 'a list; _values: 'b list}

  (** Get operator *)
  let ( .?[] ) (m : ('a, 'b) t) (k : 'a) : 'b = m._f k

  (** Empty dictionary with default value *)
  let empty (default : 'b) : ('a, 'b) t =
    {_f= (fun _ -> default); _keys= []; _values= []}

  (** No matching key error *)
  exception KeyError of string

  (** Empty dictionary without default value, raises error *)
  let err_empty ?(string_of_key : ('a -> string) option = None) () : ('a, 'b) t
      =
    match string_of_key with
    | None ->
        {_f= (fun _ -> raise (KeyError "No such key")); _keys= []; _values= []}
    | Some f ->
        { _f= (fun k -> raise (KeyError ("No such key '" ^ f k ^ "'")))
        ; _keys= []
        ; _values= [] }

  (** Safe get, catches KeyErrors *)
  let safe_get f x = try Some f.?[x] with KeyError _ -> None

  (** Number of stored items *)
  let len m = List.length m._keys

  (** Whether a key exists in the dictionary *)
  let key_in (m : ('a, 'b) t) (k : 'a) = List.exists (( = ) k) m._keys

  (** Whether a value exists in the dictionary *)
  let val_in (m : ('a, 'b) t) (v : 'b) = List.exists (( = ) v) m._values

  (** Add to dictionary *)
  let update f x y : ('a, 'b) t =
    { _f=
        (fun x' ->
          if x = x' then
            y
          else
            f.?[x'] )
    ; _keys=
        ( f._keys
        @
        if key_in f x then
          []
        else
          [x] )
    ; _values=
        ( if key_in f x then
            match List.find_index (fun xi -> x = xi) f._keys with
            | None ->
                raise (Failure "Unreachable")
            | Some idx ->
                List.mapi
                  (fun i xi ->
                    if i = idx then
                      y
                    else
                      xi )
                  f._values
          else
            f._values @ [y] ) }

  (** Set operator *)
  let ( .?[]<- ) (m : ('a, 'b) t) (k : 'a) (v : 'b) : ('a, 'b) t = update m k v

  (** Get an empty dictionary with the same keys as another *)
  let from_keys (m : ('a, 'b) t) (default : 'b) : ('a, 'b) t =
    {(empty default) with _keys= m._keys}

  (** Public access to dictionary keys *)
  let keys m = m._keys

  (** Public access to dictionary values *)
  let values m = m._values

  (** Key-value pairs of dictionary *)
  let items (m : ('a, 'b) t) : ('a * 'b) list = List.combine m._keys m._values

  (** Add key-value pairs from another dictionary *)
  let merge (m1 : ('a, 'b) t) (m2 : ('a, 'b) t) : ('a, 'b) t =
    List.fold_left (fun a k -> a.?[k] <- m2.?[k]) m1 m2._keys

  let subset (x : ('a, 'b) t) (y : ('a, 'b) t) : bool =
    List.for_all (key_in y) x._keys

  let ( <? ) = subset

  let ( >? ) x y = subset y x

  (** Equality over keys and values (order doesn't matter) *)
  let ( =? ) (x : ('a, 'b) t) (y : ('a, 'b) t) : bool =
    x < y && y < x
    && List.for_all (fun xi -> x.?[xi] = y.?[xi]) x._keys
    && List.for_all (fun yi -> x.?[yi] = y.?[yi]) y._keys

  let ( !=? ) x y = not (x =? y)

  let ( <=? ) x y = x < y || x =? y

  let ( >=? ) x y = x > y || x =? y

  let ( >=? ) x y = subset y x || x =? y
end

type ('a, 'b) dict = ('a, 'b) Dict.t

(** All items in list are true *)
let all = List.for_all (fun x -> x)

(** Any item in list is true *)
let any = List.exists (fun x -> x)

(** Get index-annotated list of items of a list *)
let enumerate ?(start : int = 0) (l : 'a list) : (int * 'a) list =
  List.mapi (fun i x -> (i + start, x)) l

(** Filter a list by a given selection function *)
let filter = List.filter

(** Sort a list by a given comparison function *)
let sort = List.sort

(** Map a function onto a list *)
let map = List.map

(** Get most <metric> item in list by a given comparison function *)
let get_superlative (l : 'a list) (compare : 'a -> 'a -> float) : 'a option =
  match l with
  | [] ->
      None
  | h :: t ->
      Some
        (List.fold_left
           (fun a x ->
             if compare a x > 0. then
               a
             else
               x )
           h t )

(** Get largest int in list *)
let max_int (l : int list) : int option =
  get_superlative l (fun x y -> float_of_int (x - y))

(** Get largest float in list *)
let max_float (l : float list) : float option = get_superlative l Float.sub

(** Get largest complex in list *)
let max_complex (l : complex list) : complex option =
  get_superlative l (fun x y -> Complex.absc (Complex.sub x y))

(** Get smallest int in list *)
let min_int (l : int list) : int option =
  get_superlative l (fun x y -> float_of_int (y - x))

(** Get smallest float in list *)
let min_float (l : float list) : float option =
  get_superlative l (fun x y -> y -. x)

(** Get smallest complex in list *)
let max_complex (l : complex list) : complex option =
  get_superlative l (fun x y -> Complex.absc (Complex.sub y x))

(** Get range \[a..b) *)
let range ?(start : int = 0) ?(step : int = 1) j =
  let rec aux n acc =
    if n < start then
      acc
    else
      aux (n - step) (n :: acc)
  in
  aux (j - 1) []

(** Reverse a list *)
let reversed = List.rev

(** Reverse a string *)
let reverse_string s =
  Stdlib.String.init (Stdlib.String.length s) (fun n ->
      Stdlib.String.get s (Stdlib.String.length s - n - 1) )

(** Get unique items of list *)
let unique (l : 'a list) : 'a list =
  List.rev
    (List.fold_left
       (fun a x ->
         if List.exists (fun y -> x = y) a then
           a
         else
           x :: a )
       [] l )

(** Slice a list *)
let slice l ?(step : int = 1) (start : int) (stop : int) =
  let rec aux current_idx acc =
    if current_idx >= stop then
      List.rev acc
    else if current_idx >= start then
      aux (current_idx + step)
        (match List.drop current_idx l with h :: t -> h :: acc | _ -> acc)
    else
      aux (current_idx + 1) acc
  in
  aux start []

(** Sum an int list *)
let sum (l : int list) = List.fold_left ( + ) 0 l

(** Sum a float list *)
let sumf (l : float list) = List.fold_left ( +. ) 0. l

(** Sum a complex list *)
let sumc (l : complex list) : complex =
  List.fold_left Complex.add Complex.zero l

(** Combine pairs of lists into lists of pairs *)
let zip = List.combine

(** Get input from stdin *)
let stdinput (prompt : string) : string = print_string prompt ; read_line ()

(** Open an input file *)
let open_in_file (mode : string) =
  open_in_gen
    (Stdlib.String.fold_left
       (fun a c ->
         match c with
         | 'r' ->
             Open_rdonly :: a
         | 'b' ->
             Open_binary :: a
         | 't' ->
             Open_text :: a
         | _ ->
             _log Log_Error
               (Printf.sprintf "Uncrecognized mode in open_in_file: %c" c) ;
             a )
       [] mode )

(** Open an output file *)
let open_in_file (mode : string) =
  open_out_gen
    (Stdlib.String.fold_left
       (fun a c ->
         match c with
         | 'w' ->
             Open_wronly :: a
         | 'x' ->
             Open_creat :: a
         | 'a' ->
             Open_append :: a
         | 'b' ->
             Open_binary :: a
         | 't' ->
             Open_text :: a
         | _ ->
             _log Log_Error
               (Printf.sprintf "Uncrecognized mode in open_in_file: %c" c) ;
             a )
       [] mode )
