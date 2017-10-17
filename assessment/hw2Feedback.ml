(*
 * Name: Quang Phung
 * Course: 2041
 * Homework 2
 * Due Date: 10/09/17
 *)

(* Solution to Problem 1 *)
(*
1. The first pattern matching ([],_) -> [] infers zip to be a type of ('a list * 'b) -> a' list.
   The second matching (_,[]) -> [] refines the type of zip to be ('a list * 'b list) -> 'c list.
   The last matching finalizes the type of zip to be ('a list * 'b list) -> ('a * 'b) list.
   => This is well-typed.

(* Score: 2.5/3
 - Missing steps.
 *)

2. The first matching infers reverse to be a type of 'a list -> 'a list
   The second matching infers the type of l to be ('a list list) and the return value of reverse to be 'a type.
   This is not consistent with the first inference.
   => This is not well-typed.
*)

(* Score: 2.5/3
 - Missing steps.
 *)

(* Solution to Problem 2 *)
(* 1.
   fib': int -> int -> int -> int -> int
   precondition: input is a positive number n, index i, 2 accumulators fib1 and fib2
   invariant: output is a nth Fibonnaci number *)

let rec fib' n i fib1 fib2 =
   if (i = n) then fib1
      else fib' n (i + 1) fib2 (fib1 + fib2)

(* 2. invariant: use fib1 and fib2 as accumulators to calculate the nth Fibonnaci number *)

(* 3.
   fib: int -> int
   precondition: input is a positive number n
   invariant: output is the nth Fibonnaci number *)
let fib n =
   let rec fib' n i fib1 fib2 =
      if (i = n) then fib1
         else fib' n (i + 1) fib2 (fib1 + fib2)
   in fib' n 1 1 1

   (* SCORE (Problem 2): 8/8
   * 	2.1 (fib’): 4/4
   *
   * 	2.2 (invariant): 2/2
   *
   * 	2.3 (fib): 2/2
   *)

(* Solution to Problem 3 *)
(* If the database doesn't contain an entry for a particular person, None will be return,
   otherwise (Some value) will be return. None and Some are components of the option type *)

let rec find_salary (lst: (string * string * float) list) (str: string) =
   match lst with
  | [] -> None
  | (name, _, salary) :: l when (name = str) -> Some salary
  | _ :: l -> find_salary l str

let rec find_phno (lst: (string * string * float) list) (str: string) =
   match lst with
  | [] -> None
  | (name, phone, _) :: l when (name = str) -> Some phone
  | _ :: l -> find_phno l str
(* 6/6 *)


(* Solution to Problem 4 *)
(* P4_1. Define a btree type
   key is an employee's name which has a type of string
   data is an employee's phone and salary which has a type of string * float
   btree type has 3 components: value ('a type), left child (btree type), and right child (btree type) *)

type ('a, 'b) btree =
    Empty
  | Node of 'a * 'b * ('a,'b) btree * ('a,'b) btree
(* 5/5 *)

(* P4_2. *)
let initTree = Empty

(* P4_3. *)
let tree1 = Node (20, "Dang",
             Node (5, "Phung", Empty, Empty), Node (94, "Chi",
	     Node (92, "Quang", Empty, Empty), Empty))
(* 2/2 *)

let tree2 = Node ("Quang", ("8413", 100000.),
             Node ("Jane", ("8511", 5000.), Empty,
	     Node ("Lucia", ("4290", 23000.), Empty, Empty)),
	     Node ("Victor", ("6537", 22000.), Empty, Empty))
(* 2/2 *)

(* P4_4. *)
(* find: ('a, 'b) btree -> 'a -> 'b option
   precondition: input is a btree
   invariant: output is a data associated with the key input, return type is option type *)

let rec find tree x =
   match tree with
   | Empty -> None
   | Node (key, data, left, right) ->
      if (x < key) then find left x
      else if (x > key) then find right x
      else Some data
(* 3/3 *)

(* P4_5. *)
(* insert: ('a, 'b) btree -> 'a -> 'b -> ('a, 'b) btree
   precondition: input is a BST, a pair of key and data
   invariant: output is a new BST with a new node inserted *)

let rec insert tree key data =
   match tree with
   | Empty -> Node (key, data, Empty, Empty)
   | Node (k, d, left, right) as node ->
      if (key < k) then Node (k, d, insert left key data, right)
      else if (key > k) then Node (k, d, left, insert right key data)
      else node

(* P4_6. *)
(* keylist: ('a, 'b) btree -> ('a list)
   precondition: input is a BST
   invariant: output is a list of keys *)

let rec keylist tree =
   let rec keylist' tree lst =
      let rec append l1 l2 =
         match l1 with
         | [] -> l2
         | (h::t) -> h :: (append t l2)
      in
         match tree with
         | Empty -> lst
         | Node (key, _, left, right) ->
            let left_list = keylist' left []
	    in let right_list = key :: (keylist' right [])
 	       in
	          append left_list right_list
   in
      keylist' tree []

(* P4_7. *)
(* delete: ('a, 'b) btree -> 'a -> ('a, 'b) btree
   precondition: input is a BST and a key to delete
   invariant: output is a new BST with old node deleted *)

let rec delete tree key =
   let rec delete_root tr =
      let rec min = function
         | Empty -> None
	 | Node (k, v, left, right) ->
	    if (left = Empty) then Some (k, v)
	    else min left
      in match tr with
         | Empty -> Empty
	 | Node (k1, v1, left1, right1) ->
	    if (left1 = Empty) then right1
	    else if (right1 = Empty) then left1
	    else let Some (k2, v2) = min right1 in
	       Node (k2, v2, left1, delete right1 k2)
   in match tree with
      | Empty -> Empty
      | Node (k3, v3, left3, right3) ->
         if (key < k3) then Node (k3, v3, delete left3 key, right3)
	 else if (key > k3) then Node (k3, v3, left3, delete right3 key)
	 else delete_root tree


(*
  Problem 4.5-4.7: 11.5/13
  insert: insert unable to replace the node already in the tree

*)
(* Solution to Problem 5 *)
type ty = BoolTy | IntTy | FunTy of ty * ty

type expr' =
     | Id' of string                     (* for identifiers *)
     | Int' of int                       (* for integer values *)
     | True'                             (* for the boolean value true *)
     | False'                            (* for the boolean value false *)
     | Plus' of expr' * expr'            (* for exp1 + exp2 *)
     | Minus' of expr' * expr'           (* for exp1 - exp2 *)
     | Lss' of expr' * expr'             (* for exp1 < exp2 *)
     | Gtr' of expr' * expr'             (* for exp1 > exp2 *)
     | And' of expr' * expr'             (* for exp1 && exp2 *)
     | Or'  of expr' * expr'             (* for exp1 || exp2 *)
     | Cond' of expr' * expr' * expr'    (* for if exp1 then exp2 else exp3 *)
     | Fun' of string * ty * expr'       (* for fun (x:ty) -> exp *)
     | App' of expr' * expr'             (* for (exp1 exp2) *)

(* P5_1. *)

let exp1' = Fun' ("x", IntTy, Fun' ("y", BoolTy, Cond' (And' (Id' "y", True'), Plus' (Id' "x", Int' 1), Minus' (Id' "x", Int' 5))))

let exp2' = App' (Fun' ("x", IntTy, App'(Fun' ("y", IntTy, Plus' (Id' "x", Id' "y")), Id' "x")), Int' 5)

(* P5_2. *)
(* typeof_aux : expr' -> (string * ty) list -> ty option
   precondition: input is an expression and a list of identifiers
   invariant: returns None if expr is ill-typed, otherwise returns the correct type of the expressin *)

let rec typeof_aux expr nl =
  let rec mem x lst =
    match lst with
    | [] -> None
    | (str,ty)::t -> if (x = str) then (Some ty) else (mem x t)
  in match expr with
  | Id' s -> (mem s nl)
  | Int' _ -> Some IntTy
  | True' | False' -> Some BoolTy
  | Plus' (e1, e2) | Minus' (e1, e2) ->
     if (((typeof_aux e1 nl) = Some IntTy) && ((typeof_aux e2 nl) = Some IntTy)) then Some IntTy
     else None
  | Lss' (e1, e2) | Gtr' (e1, e2) | And' (e1, e2) | Or' (e1, e2) ->
     if (((typeof_aux e1 nl) = Some BoolTy) && ((typeof_aux e2 nl) = Some BoolTy)) then Some BoolTy
     else None
  | Cond' (e1, e2, e3) ->
     if (((typeof_aux e1 nl) = Some BoolTy) && ((typeof_aux e2 nl) = (typeof_aux e3 nl))) then (typeof_aux e3 nl)
     else None
  | Fun' (s, ty, e) ->
    ( match (typeof_aux e ((s, ty)::nl)) with
      | None -> None
      | Some x -> Some (FunTy (ty, x)) )
  | App' (e1, e2) ->
      match (typeof_aux e1 nl) with
      | None | Some IntTy | Some BoolTy -> None
      | Some (FunTy (ty1, ty2)) -> if (Some ty1 = (typeof_aux e2 nl)) then Some ty2
	                    else None

(* P5_3. *)
(* typeof : expr' -> ty option
   precondition: input is an expression
   invariant: returns None if the expression is not typeable or (Some Ty) if it is typeable *)

let rec typeof expr = (typeof_aux expr [])

(*
  Problem 5.1: 2/4
  -2: exp1' is incorrect

  Problem 5.2: 7/8
  -1: handling comparison cases incorrectly

  Problem 5.3: 2/2
 *)


(* Solution to Problem 6 *)
type expr =
       Id of string                     (* for identifiers *)
     | Int of int                       (* for integers *)
     | True                             (* for the boolean value true *)
     | False                            (* for the boolean value false *)
     | Plus of expr * expr              (* for exp1 + exp2 *)
     | Minus of expr * expr             (* for exp1 - exp2 *)
     | Times of expr * expr             (* for exp1 * exp2 *)
     | Div of expr * expr               (* for exp1 / exp2, division being for integers *)
     | Lss of expr * expr               (* for exp1 < exp2 *)
     | Eq of expr * expr                (* for exp1 = exp2, = being equality comparison *)
     | Gtr of expr * expr               (* for exp1 > exp2 *)
     | And of expr * expr               (* for exp1 && exp2 *)
     | Or of expr * expr                (* for exp1 || exp2 *)
     | Not of expr                      (* for not exp *)
     | Cond of expr * expr * expr       (* for if exp1 then exp2 else exp3 *)
     | Let of string * expr * expr      (* for let  = exp1 in exp2 *)

(* eval : expr -> (string * expr) list -> expr
   precondition: input is an expression and a list of indentifiers
   invariant: output is an evaluated expression *)

(* 9/10 -- need to evaluate value expression before adding to scope in Let case *)

let rec eval e nl =
  let rec mem x lst =
    match lst with
    | [] -> None
    | (str, e)::t -> if (x = str) then (Some e) else (mem x t)
  in match e with
  | Id s -> let (Some e) = mem s nl in e
  | Int i -> Int i
  | True -> True
  | False -> False
  | Plus (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> Int (a + b) )
  | Minus (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> Int (a - b) )
  | Times (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> Int (a * b) )
  | Div (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> Int (a / b) )
  | Lss (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> if (a < b) then True else False )
  | Eq (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> if (a = b) then True else False )
  | Gtr (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (Int a, Int b) -> if (a > b) then True else False )
  | And (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (True, True) -> True
      | (_,_) -> False )
  | Or (e1, e2) ->
    ( match (eval e1 nl, eval e2 nl) with
      | (False, False) -> False
      | (_, _) -> True )
  | Not e -> if ((eval e nl) = True) then False else True
  | Cond (e1, e2, e3) -> if ((eval e1 nl) = True) then (eval e2 nl) else (eval e3 nl)
  | Let (s, e1, e2) -> eval e2 ((s, e1)::nl)

(* End of hw2.ml *)
