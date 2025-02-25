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
