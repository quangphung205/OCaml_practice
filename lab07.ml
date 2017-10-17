(* Name: Quang Phung
   Course: CSCI 2041
   Lab07.ml
   Due Date: 10/20/17 *)

(* Solution to Problem 1 *)
(* map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
   precondition: a function and  2 lists of the same type
   invariant: apply a function to both lists to create a new list *)
let rec map2 f l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> []
  | (h1::t1, h2::t2) -> (f h1 h2) :: (map2 f t1 t2)


(* Solution to Problem 2 *)
let rec append l =
  match l with
  | [] -> (fun l2 -> l2)
  | (h::t) ->
      let tail_appender = append t in
        (fun l2 -> h :: tail_appender l2)

(* map2: 'a list -> 'b list -> ('a -> 'b -> 'c) -> 'c list
   precondition: a function and  2 lists of the same type
   invariant: apply a function to both lists to create a new list *)
let rec map2' l1 l2 =
  match (l1, l2) with
  | ([], _) | (_, []) -> fun f -> []
  | (h1::t1, h2::t2) ->      
     let tail_appender = map2' t1 t2 in
     (fun f -> (f h1 h2) :: (tail_appender f))


(* Solution to Problem 3 *)
let rec accumulate f lst u =
      match lst with
      | [] -> u
      | (h::t) -> accumulate f t (f h u)

  let rec reduce f lst u =
     match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u)

(* P3_1. *)
(* union: 'a list -> 'a list -> 'a list 
   precondition: 2 lists of the same type
   invariant: the union of 2 input lists *)
let union l1 l2 = accumulate (fun x lst -> if (List.mem x lst) then lst else x::lst) l1 l2

(* P3_2. *)
(* unzip: ('a * 'b) list -> ('a list) * ('b list)
   precondition: input is a list of pairs
   invariant: output is a pair of lists *)
let unzip l = reduce (fun (x1, x2) (l1, l2) -> (x1::l1, x2::l2)) l ([],[])

(* P3_3. *)
(* zip: 'a list -> 'b list -> ('a * 'b) list
   precondition: input has 2 lists
   invariant: a new list of pairs of elements in the input lists *)
let zip l1 l2 = map2 (fun x y -> (x,y)) l1 l2


(* Solution to Problem 4 *)
(* P4_1. 
 * 1. Initially: 'a -> 'b -> 'c -> 'd
 * 2. (g x) means g has a function type and x is an argument of g
      'a -> ('c -> 'b) -> 'c -> 'd
 * 3. (f (g x)) means f has a function type, the return type of (g x) is the input type of f,
      and the return type of f is the return type of function compose
      Compose: ('b -> 'a) -> ('c -> 'b) -> 'c -> 'a 
 *)
  
let compose f g = fun x -> (f (g x))
(* test1 is type correct *)
let test1 = compose (fun x -> x + 3) (fun x -> 2 * x)
(* test2 is not type correct because the return type of g, which is bool,
   does not match the input type of f, which is expected to be int type *)
(*let test2 = compose (fun x -> 2 * x) (fun y -> true)*)

(* P4_2. 
 * Initially: 'a -> 'b -> 'c
 * '[] -> false' means l has a type of list, and the return type is bool
   'a -> 'b list -> bool
 * (p h) means p has a function type, its input type is the same with the type of each element
   in the list, and its return type is bool
   ('b -> bool) -> 'b list -> bool
 *)
let rec forsome p l =
    match l with
    | [] -> false
    | (h::t) -> (p h) || (forsome p t)

(* Examples:
 * test1 fails because the return type of the function is of int type but expected to be of bool type
let test1 = forsome (fun x -> x + 3) [1;2;3;4]

 * test2 fails because the input type of the function is of string type but expected to be of int type
let test2 = forsome (fun x -> if (x mod 2 = 0) then true else false) ["a"; "b"; "c"]
  *)
(* End of lab07.ml *)
