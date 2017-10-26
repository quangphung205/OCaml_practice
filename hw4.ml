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

(* P1_a.
 * makePairLists: string -> string list -> ((string * string) * string list) list
 * precondition: input is a name of a person and a list of friend's names
 * invariant: output is a pair of the person and a friend *)
let makePairLists (p: string) fl = List.map (fun x -> if (x < p) then ((x,p), fl)
                                            else ((p, x), fl)) fl

(* P1_b.
 * makeAllPairLists: (string * (string list)) list -> ((string * string) * string list) list
 * precondition: input is a list of persons and their friend list
 * invariant: output is a list of pairs of that person and each of their friends *)
let makeAllPairLists l = List.map 
