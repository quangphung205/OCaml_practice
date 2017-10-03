(* Name: Quang Phung
 * Course: CSCI 2041 - 003
 * Lab05
 * Due date: 10/06/17
 *)

(* Solution to problem 1 *)
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree

(* P1_1. *)
let rec maxTree = function
  | Empty -> None
  | Node (n, _, right) ->
     if (right = Empty) then Some n
     else maxTree right

(* P1_2. *)
let rec minTree = function
  | Empty -> None
  | Node (n, left, _) ->
     if (left = Empty) then Some n
     else minTree left

(* P1_3. *)
let rec isSearchTree t =
   let bigger i j =
     match j with
     | None -> true
     | (Some j') -> i >= j' in
   let smaller i j =
     match j with
     | None -> true
     | Some j' -> j' >= i in
   match t with
   | Empty -> true
   | Node (i,l,r) ->
         isSearchTree l && isSearchTree r &&
           (bigger i (maxTree l)) && (smaller i (minTree r))

(* P1_4. *)
(* t1 and t2 are BSTs, whereas t3 is not *)
let t1 = Node(20, Node(5, Empty, Empty),
	      Node(94, Node(92, Empty, Empty), Empty))
let t2 = Node("Quang", Node("Jane", Empty,
			    Node("Lucia", Empty, Empty)),
	      Node("Victor", Empty, Empty))
let t3 = Node(1, Node(2, Empty, Empty),
	      Node(4, Empty, Node(3, Empty, Empty)))

(* Solution to problem 2 *)
type  expr' =
  Int' of int | True' | False'
| Plus' of expr' * expr' | Minus' of expr' * expr'
| Lss' of expr' * expr' | Gtr' of expr' * expr'
| And' of expr' * expr' | Or' of expr' * expr'
| Cond' of expr' * expr' * expr'

(* P2_1. *)
type ty = IntTy | BoolTy

let rec typeof = function   
  | Int' _ -> Some IntTy
  | True' | False' -> Some BoolTy     
  | (Plus' (e1,e2)) | (Minus' (e1,e2)) ->
     if (((typeof e1) = Some IntTy) && ((typeof e2) = Some IntTy)) then Some IntTy
     else None
  | (Lss' (e1,e2)) | (Gtr' (e1,e2)) ->
     if (((typeof e1) = Some IntTy) && ((typeof e2) = Some IntTy)) then Some BoolTy
     else None         
  | (And' (e1,e2)) | (Or' (e1,e2)) ->
     if (((typeof e1) = Some BoolTy) && ((typeof e2) = Some BoolTy)) then Some BoolTy
     else None         
  | Cond' (e1,e2,e3) ->
     if ((typeof e1 = Some BoolTy) && (typeof e2 = typeof e3)) then typeof e2
     else None

(* P2_2. *)
let wellTyped expr =
  match (typeof expr) with
  | None -> false
  | Some _ -> true

(* P2_3. *)
let e1 = Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)),
                Int' 5, Int' 7)

let e2 = Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)),
                Int' 5, True')

let e3 = Cond' (And' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)), True'),
                False', True')


(* Solution to problem 3 *)
type ocamlTy =
  | IntTy' of int
  | BoolTy' of bool
  | VarTy of string
  | ListTy of ocamlTy
  | FuncTy of ocamlTy * ocamlTy

(* End of lab05.ml *)
