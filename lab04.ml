(*
 * Name: Quang Phung
 * Course: CSCI 2041 - 003
 * Due date: 09/29/17
*)

(* Solution to problem 1 *)
(*
1) It is well-typed. Its type is 'a -> 'b list -> ('a * 'b) list
2) It is well-typed. Its type is int list -> int -> int list
3) It is well-typed. Its type is (int * (int list)) -> int list
4) It is well-typed. Its type is (int * string) list -> int -> string
5) It is not well-typed because in the "and" statement, we passed in l as an argument, 
   so OCaml assumed that the orginal l would be in type ('a list). However, in the "in" statement,
   we passed in the first element of l, which is variable h, to the function length,
   now the orginal l would become a type 'a list list.
*)

