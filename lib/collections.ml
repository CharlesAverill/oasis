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
      @result The remainder counter
  *)
  let subtract (a : 'a t) (b : 'a t) : 'a t =
    List.fold_left
      (fun a k ->
        a.?[k] <- (match safe_get a k with None -> 0 | Some x -> x) - b.?[k] )
      a (keys b)

  (** The total value of the counts of a counter *)
  let total (x : 'a t) : int = sum (values x)
end

module type Heap_t = functor (OT : Map.OrderedType) -> sig
  type x = OT.t

  type t = x list

  val parent : t -> int -> int option

  val lefti : t -> int -> int option

  val righti : t -> int -> int option

  val max_heapify : t -> int -> int -> t

  val heapify : x list -> t

  val heap_sort : x list -> x list
end

(** Binary heap implementation *)
module Heap : Heap_t =
functor
  (OT : Map.OrderedType)
  ->
  struct
    include List

    type x = OT.t

    type t = x list

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
    let rec max_heapify (a : x list) (n : int) (i : int) : 'a list =
      match (lefti a i, righti a i) with
      | Some l, Some r ->
          let largest =
            if l < n && OT.compare (nth a l) (nth a i) > 0 then
              l
            else
              i
          in
          let largest =
            if r < n && OT.compare (nth a r) (nth a largest) > 0 then
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
           (a, length a / 2)
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
      sort (heapify a) (List.length a)
  end

module IntOT = struct
  type t = int

  let compare = ( - )
end
