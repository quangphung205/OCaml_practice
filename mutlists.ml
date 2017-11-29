(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 6
 * Due Date: 11/27/17
 *)

(* A type for mutable lists; lists are references to list cells that
   either indicate an empty list or a head element and a tail list *)
type 'a mylist = 'a listcell ref
  and  'a listcell = Nil | Cons of 'a * ('a mylist)

(* Solution to Problem 1
    append : 'a mylist -> 'a mylist -> 'a mylist
 *  precondition: two lists of type ('a mylist)
 * invariant: append the second list into the first list
 *)
let rec append l1 l2 =
   match !l1 with
   | Nil -> l2
   | Cons (i, tl) -> (tl := !(append tl l2); l1)

(* Solution to Problem 2
    rev_app : 'a mylist -> 'a mylist -> 'a mylist
 * precondition: two lists of type ('a mylist)
 * invariant: append the first list in the reverse order into the second list
 *)
let rec rev_app l1 l2 =
   match !l1 with
   | Nil -> l2
   | Cons (i, tl) -> let nextl = (ref !tl) in (tl := !l2; rev_app nextl l1)

(* End of mutlists.ml *)
