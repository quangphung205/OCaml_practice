(* Name: Quang Phung
 * Lab: CSCI 2041 - 003
 * lab03.ml
 *)

(* Solution to problem 1 *)
(* sumup: int -> int
   precondition: input is a non-negative integer
   invariant: output is the sum from 0 to the input *)

let rec sumup = function
  | 0 -> 0
  | i -> i + (sumup (i - 1))


(* Solution to problem 2 *)
(* flip_pair: 'a * 'b -> 'b * 'a
   precondition: input is a tuple
   invariant: output is a reverse tuple of input *)
let flip_pair (a, b) = (b, a)

(* flip_list: ('a * 'b) list -> ('b * 'a) list
   precondition: input is a list of tuples
   invariant: output is a list of reverse tuples *)
let rec flip_list = function
  | [] -> []
  | (a, b) :: l -> (flip_pair (a, b)) :: (flip_list l)


(* Solution to problem 3 *)
let rec destutter = function
  | [] -> []
  | (x::y::l) ->
     if (x=y) then destutter (x::l) else x :: destutter (y::l)
  | i::[] -> i::[]


(* Solution to problem 4 *)
(* sum_diffs: (int list) -> int
   precondition: input is a list of integers
   invariant: output is a sum of differences between successive pairs in a list *)
let rec sum_diffs = function
  | [] | _::[] -> 0
  | x::y::l -> (x - y) + (sum_diffs (y::l))


(* Solution to problem 5 *)
(* unzip: ('a * 'b) list -> 'a list * 'b list
   precondition: input is a list of pairs
   invariant: output has two list constructed from the input *)
let rec unzip = function
  | [] -> ([],[])
  | (a, b)::l ->
     match (unzip l) with
     | (c, d) -> (a::c, b::d)


(* Solution to problem 6 *)
(* f: int -> int
   m: int -> int
   precondition: both functions take an integer as the input
   invariant: output is an integer caculated by a formula *)
let rec f = function
  | 0 -> 1
  | n -> n - m(f(n-1))
and m = function
  | 0 -> 0
  | n -> n - f(m(n-1))

(* End of lab03.ml *)
