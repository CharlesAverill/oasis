(** Specialized container data types *)

open Builtins
open Builtins.Dict

(** Count items in a list *)
module Counter = struct
  include Builtins.Dict

  type 'a t = ('a, int) dict

  (** Get counter object for a list *)
  let counter (l : 'a list) : 'a t =
    List.fold_left (fun a x -> a.?[x] <- a.?[x] + 1) (empty 0) l

  (** Get elements of original list, grouped and in order of retrieval *)
  let elements (x : 'a t) : 'a list =
    List.concat (List.map (fun i -> List.init x.?[i] (fun _ -> i)) (keys x))

  (** Get the most common elements of original list, annotated with their occurrence
      numbers

      @param x The counter to look in
      @param n The number of most common elements to retrieve, defaults to -1 (all)
      @return A list of pairs of items and their occurrence numbers
  *)
  let most_common ?(n : int = -1) (x : 'a t) : ('a * int) list =
    let most_common =
      reversed (List.sort (fun x1 x2 -> x.?[x1] - x.?[x2]) (keys x))
    in
    List.map
      (fun i -> (i, x.?[i]))
      (slice most_common 0
         ( if n < 0 then
             List.length most_common
           else
             n ) )

  (** Elements occurrence values from one counter are subtracted from another, with 
      the value for missing element occurrences being 0

      @param a The counter to subtract from
      @param b The counter being subtracted
      @return The remainder counter
  *)
  let subtract (a : 'a t) (b : 'a t) : 'a t =
    List.fold_left
      (fun a k ->
        a.?[k] <- (match safe_get a k with None -> 0 | Some x -> x) - b.?[k] )
      a (keys b)

  (** The total value of the counts of a counter *)
  let total (x : 'a t) : int = sum (values x)
end

(** Binary heap implementation *)
module Heap =
functor
  (OT : Map.OrderedType)
  ->
  struct
    include List

    type x = OT.t

    type t = x option list

    let _oob_option = function None -> failwith "Out of bounds" | Some x -> x

    (** Get the index of the parent of the element at index [i] in the heap.
      Returns [None] if [i] is the root or invalid.

      @param h The heap
      @param i The index of the element
      @return [Some index] of the parent or [None] if no parent exists *)
    let parent (_ : t) (i : int) : int option =
      if i = 0 then
        None
      else
        Some ((i - 1) / 2)

    (** Get the index of the left child of the element at index [i], if it exists.

      @param h The heap
      @param i The index of the element
      @return [Some value] if left child exists, else [None] *)
    let lefti (h : t) (i : int) : int option =
      if (2 * i) + 1 <= length h then
        Some ((2 * i) + 1)
      else
        None

    (** Get the index of the right child of the element at index [i], if it exists.

      @param h The heap
      @param i The index of the element
      @return [Some index] if right child exists, else [None] *)
    let righti (h : t) (i : int) : int option =
      if (2 * i) + 2 <= length h then
        Some ((2 * i) + 2)
      else
        None

    (** Recursively enforce the max-heap property from a given index.
  
      @param a The heap (list) to adjust
      @param n Effective length of a to process
      @param i The index to enforce heap property from
      @return The adjusted heap *)
    let rec max_heapify (a : t) (n : int) (i : int) : 'a list =
      match (lefti a i, righti a i) with
      | Some l, Some r ->
          let largest =
            if
              l < n
              && OT.compare (_oob_option (nth a l)) (_oob_option (nth a i)) > 0
            then
              l
            else
              i
          in
          let largest =
            if
              r < n
              && OT.compare
                   (_oob_option (nth a r))
                   (_oob_option (nth a largest))
                 > 0
            then
              r
            else
              largest
          in
          if largest <> i then
            max_heapify (swap a i largest) n largest
          else
            a
      | _, _ ->
          a

    (** Convert a list into a max-heap.
  
      @param a The list to heapify
      @return The max-heap represented as a list *)
    let heapify (a : x list) : t =
      fst
        (fold_left
           (fun (a, i) _ -> (max_heapify a (length a) i, i - 1))
           (List.map (fun x -> Some x) a, length a / 2)
           (take ((length a / 2) + 1) a) )

    (** Sort a list *)
    let heap_sort (a : x list) : x list =
      let rec sort a n =
        if n <= 1 then
          a
        else
          let a = swap a 0 (n - 1) in
          let a = max_heapify a (n - 1) 0 in
          sort a (n - 1)
      in
      fold_left
        (fun a -> function None -> a | Some x -> a @ [x])
        []
        (sort (heapify a) (List.length a))

    (** Increase the value of an item in the heap 
        
        @param h The heap to update
        @param i The index of the heap to update 
        @param k The new value of [h[i]]
        @return The updated heap
        @raise Invalid_Argument if [k < nth h i]
        @raise Failure if [length h <= i]
    *)
    let heap_increase_key (h : t) (i : int) (k : x) : t =
      if match nth h i with None -> false | Some x -> OT.compare k x < 0 then
        invalid_arg "heap_increase_key passed a key that is not larger" ;
      let rec aux h i =
        match parent h i with
        | None ->
            h
        | Some parent ->
            if
              parent >= 0
              && OT.compare (_oob_option (nth h i)) (_oob_option (nth h parent))
                 > 0
            then
              aux (swap h i parent) parent
            else
              h
      in
      aux (replacei h i (Some k)) i

    (** Extract the maximum element from a max-heap and return the updated heap.

        @param h The heap
        @return [Some (max_element, new_heap)] if the heap is non-empty, [None] otherwise
    *)
    let extract_max (h : t) : (x * t) option =
      match h with
      | [] ->
          None
      | x :: y ->
          Some
            ( _oob_option x
            , take (length y)
                (max_heapify (swap h 0 (length h - 1)) (length h - 1) 0) )

    (** Insert a new key into the max-heap while maintaining the max-heap property.

        @param h The current heap
        @param k The key to insert
        @return The updated heap with the new key inserted and the max-heap property restored
    *)
    let heap_insert (h : t) (k : x) : t =
      let h = h @ [None] in
      heap_increase_key h (length h - 1) k
  end

module IntOT = struct
  type t = int

  let compare = ( - )
end

module FloatOT = struct
  type t = float

  let compare = ( -. )
end

module StringOT = struct
  type t = string

  let compare = Stdlib.String.compare
end
