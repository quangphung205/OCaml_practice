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


(* Solution to problem 2 *)
let atLeastTwo =
    function
      | ([] | [_]) -> false
      | _ -> true

let result = atLeastTwo [1]    (* return false *)
let result = atLeastTwo [1;2]  (* return true *)

let sameFstAndSnd =
    function
      | ([] | [_]) -> false
      | (h1::h2::_) when (h1 = h2) -> true
      | _ -> false



(* Solution to problem 3 *)

(* sumList': int list -> int -> int
   precondition: input is a list of integers and an accumulator to store the result
   invariant: output is a sum of all integers in the list *)
let rec sumList' lst acc =
   match lst with
   | [] -> acc
   | h::t -> sumList' t (h + acc)

(* Solution to problem 4 *)
(* sumList: int list -> int
   precondition: input is a list of integers
   invariant: output is a sum of all integers in the list *)
let sumList lst = sumList' lst 0


(* Solution to problem 5 *)
(* drop: int * 'a list -> 'a list
   precondition: a tuple whose 1st component is an integer and 2nd component is a list
   invariant: drop the first n elements of the list *)
let rec drop (n,l) =
    match l with
    | [] -> []
    | (h::t) -> if (n = 0) then l else drop (n-1,t)


(* Solution to problem 6 *)
type coord = float * float

type shape =
     | Circ of coord * float
     | Triangle of coord * coord * coord
     | Quadrangle of coord * coord * coord * coord

let pi = 4.0 *. atan 1.0;;

let distance =
    function
      |	((x1,y1),(x2,y2)) ->
            let xdiff = x1 -. x2 in
            let ydiff = y1 -. y2 in
              sqrt (xdiff *. xdiff +. ydiff *. ydiff)

(* perimeter: shape -> float
   precondition: one of any defined shapes (Circ, Triangle, Quadrangle)
   invariant: the perimeter of that shape = sum of length of all edges *)
let perimeter s =
   match s with
   | Circ (c,r) -> 2. *. r *. pi
   | Triangle (v1,v2,v3) -> (distance (v1,v2)) +. (distance (v2,v3)) +. (distance (v3,v1))
   | Quadrangle (v1,v2,v3,v4) -> (distance (v1,v2)) +. (distance (v2,v3)) +. (distance (v3,v4)) +. (distance (v4,v1))

(* End of lab04.ml *)
