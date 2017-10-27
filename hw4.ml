(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 4
 * Due date: 10/27/17 *)

(* Solution to Problem 1 *)
let friendsList =
   [ ("a", ["b"; "c"; "d"]);
     ("b", ["a"; "c"; "d"; "e"]);
     ("c", ["a"; "b"; "d"; "e"]);
     ("d", ["a"; "b"; "c"; "e"]);
     ("e", ["b"; "c"; "d"]) ]

(* P1_1_a.
 * makePairLists: string -> string list -> ((string * string) * string list) list
 * precondition: input is a name of a person and a list of friend's names
 * invariant: output is a pair of the person and a friend *)
let makePairLists (p: string) fl = List.map (fun x -> if (x < p) then ((x,p), fl)
                                            else ((p, x), fl)) fl

(* P1_1_b.
 * makeAllPairLists: (string * (string list)) list -> ((string * string) * string list) list
 * precondition: input is a list of persons and their friend list
 * invariant: output is a list of pairs of that person and each of their friends *)
let makeAllPairLists l = List.map (fun (p, fl) -> makePairLists p fl) l

(* P1_2_a.
 * intersect: (string * (string list)) list -> (string * (string list)) list -> (string * (string list)) list
 * precondition: input has 2 lists of pairs of a person and each of their friends
 * invariant: output is a combination of 2 input lists *)
let intersect l1 l2 = List.fold_right (fun x l -> if (List.mem x l2) then x::l else l) l1 []

(* P1_2_b.
 * addOnePair: ((string * string) * string list) -> (((string * string) * string list)) list
               -> (((string * string) * string list)) list
 * precondition: input is a pair of friends and a list of potential common friends, a partial result list
 * invariant: update the partial result list with new information *)
let rec addOnePair (p, fl) result =
   match result with
   | [] -> [(p, fl)]
   | (p1, fl1)::t -> if (p = p1) then (p, intersect fl fl1)::t
                     else (p1, fl1)::(addOnePair (p, fl) t)

(* P1_2_c.
 * addAllPairs: (((string * string) * string list)) list -> (((string * string) * string list)) list
                -> (((string * string) * string list)) list
 * precondition: input has a list of pairs of friends and their potential common friends,
                 and a partial result list
 * invariant: update the partial result list with new information *)
let rec addAllPairs l1 l2 =
   match l1 with
   | [] -> l2
   | (p, fl)::t -> addAllPairs t (addOnePair (p, fl) l2)

(* P1_3.
 * commonFriends: (string * (string list)) list -> ((string * string) * string list) list
 * precondition: input is a list of pairs of a person and their friendlist
 * invariant: output is a list of pairs of two people and their common friendlist *)
let commonFriends l = List.fold_right addAllPairs (makeAllPairLists l) []


(* Solution to Problem 2 *)
(* P2_1. *)
type 'a olist = { data : 'a list; order : 'a -> 'a -> bool }

(* P2_2.
 * initOList: 'a -> 'a -> bool -> 'a olist
 * precondition: input is an ordering relation over a type
 * invariant: output is an olist of the given type *)
let initOList f = { data = []; order = f }

(* P2_3. *)
(* list1 satisfies the ordering relation: 1 < 2 < 3 < 4 < 5
 * list2 satisfies the ordering relation: 8 > 7 > 6 > 5 > 4
 * list3 does not satisfy the ordering relation: 1 != 3 *)
let list1 = { data = [1;2;3;4;5]; order = fun x y -> x < y }
let list2 = { data = [8;7;6;5;4]; order = fun x y -> x > y }
let list3 = { data = [1;3;5;7;9]; order = fun x y -> x = y }

(* P2_4.
 * isOrderedList: 'a olist -> bool
 * precondition: input is an olist
 * invariant: returns true if a given list satisfies the ordering relation
              returns false otherwise *)
let isOrderedList ol =
   let rec isOrderedList_helper l f =
      match l with
      | _::[] | []  -> true
      | h1::h2::t -> if (not(f h1 h2)) then false
                     else isOrderedList_helper (h2::t) f
   in isOrderedList_helper ol.data ol.order

(* P2_5.
 * insertOList: 'a -> 'a olist -> 'a olist
 * precondition: input has an element and an ordered list
 * invariant: a new orderd list with new element *)
let insertOList x ol =
   let rec insert_helper x l f =
      match l with
      | [] -> [x]
      | h::t -> if (f x h) then x::l else h::(insert_helper x t f)
   in insert_helper x ol.data ol.order

(* P2_6.
 * olistToList: 'a olist -> 'a list
 * precondition: input is an ordered list
 * invariant: output is a normal list out of an ordered list *)
let olistToList ol = ol.data


(* Solution to Problem 3 *)
(* P3_1.
 * cont_append: 'a list -> 'a list -> ('a list -> 'b) -> 'b
 * precondition: input has 2 lists and a continuation function
 * invariant: the result list of appending 2 lists *)
let rec cont_append l1 l2 c =
   match l1 with
   | [] -> (c l2)
   | h::t -> cont_append t l2 (fun x -> (c (h::x)))

(* P3_2 *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree
(* cont_sumTree : int btree -> (int -> 'a) -> 'a
 * precondition: input is a binary tree and a continuation function
 * invariant: output is the sum of all elements *)
(*
let rec cont_sumTree btree c =
   match btree with
   | Empty -> (c 0)
   | Node(i,l,r) -> (cont_sumTree l (cont_sumTree r (fun x -> c (i + x))))
*)
let btree = Node (4, Node (20, Empty, Empty), Node (5, Empty, Empty))
let btree1 = Node(4, Node (20, Empty, Empty), Empty)
(* End of hw4.ml *)
