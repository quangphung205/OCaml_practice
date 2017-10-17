(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework #3
 * Due Date: 10/18/17 *)

(* Solution to Problem 1 *)
(* divide_list : ('a -> bool) -> 'a list -> 'a list * 'a list
   precondition: input is a boolean function over a given type and a list of elements of that type
   invariant: output is a tuple of 2 lists, one has elements which satisfy a boolean function,
              and one does not *)
let rec divide_list f l =
   match l with
   | [] -> ([],[])
   | (h::t) ->
       let (t1, t2) = divide_list f t
       in if (f h) then (h::t1, t2)
          else (t1, h::t2)


(* Solution to Problem 2 *)
type 'a btree =
    Empty
  | Node of 'a * 'a btree * 'a btree


(* treemap : 'a btree -> ('a -> 'b) -> 'b btree
   precondition: input is a binary tree and a function
   invariant: use a function in input to transform it to the new tree *)
let rec treemap tree f =
   match tree with
   | Empty -> Empty
   | Node (a, left, right) -> Node (f a, treemap left f, treemap right f)


(* Solution to Problem 3 *)
(* P3_Q1
 * '[] -> u' infers that 'lst' has type list and the function 'reduce' will return type of u: 'a -> 'b list -> 'c -> 'c
 * 'f h (reduce f t u)' infers that 'f' has function type and the return type of f matches the type of u.
 * Conclusion: this function is type correct
 * reduce: ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
 *)
let rec reduce f lst u =
     match lst with
     | [] -> u
     | (h::t) -> f h (reduce f t u)

(* P3_Q2
 * '([],[])-> true' infers that l1, l2 are lists,
   and the function 'forall2' will return type bool: 'a -> 'b list -> 'c list -> bool
 * '(p l1 l2)' infers that 'p' has function type, it will take in two arguments from 2 different lists
    and return type bool to be consistent with the first match
 * Conclusion: this function is type correct
 * forall2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
 *)
let rec forall2 p l1 l2 =
     match (l1,l2) with
     | ([],[]) -> true
     | ([],_) -> false
     | (_,[]) -> false
     | ((h1::t1),(h2::t2)) ->
          (p h1 h2) && (forall2 p t1 t2)


(* Solution to Problem 4 *)
let rec accumulate f lst u =
      match lst with
      | [] -> u
      | (h::t) -> accumulate f t (f h u)

(* P4_Q1 *)
(* append: 'a list -> 'a list -> 'a list
   precondition: input has 2 lists with the same type
   invariant: output is the combined list of 2 lists *)
let append l1 l2 = reduce (fun x lst -> x::lst) l1 l2

(* reverse: 'a list -> 'a list
   precondition: input is a list
   invariant: output is an input list in reverse order *)
let reverse l1 = accumulate (fun x lst -> x::lst) l1 []

(* filter: ('a -> bool) -> 'a list -> 'a list
   precondition: input is a bool valued function and a list
   invariant: output is a list of elemets that satisfy the function condition *)
let filter f l1 = reduce (fun x lst -> if (f x) then x::lst else lst) l1 []


(* Solution to Problem 5 *)
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
     | Fun of string * expr             (* for fun x -> exp *)
     | App of expr * expr               (* for (exp1 exp2) *)

(* P5_1. *)
(* freeIn: expr -> string -> bool
   precondition: input is an expression and a string of an identifier's name
   invariant: output returns true if the identifier is unbound, false otherwise *)
let rec freeIn e x =
   match e with
   | Id s -> if (s = x) then true else false
   | Int _ -> false
   | True | False -> false
   | Plus (e1, e2) | Minus (e1, e2) | Times (e1, e2) | Div (e1, e2) -> (freeIn e1 x) || (freeIn e2 x)
   | Lss (e1, e2) | Eq (e1, e2) | Gtr (e1, e2) -> (freeIn e1 x) || (freeIn e2 x)
   | And (e1, e2) | Or (e1, e2) -> (freeIn e1 x) || (freeIn e2 x)
   | Not e -> freeIn e x
   | Cond (e1, e2, e3) -> (freeIn e1 x) || (freeIn e2 x) || (freeIn e3 x)
   | Let (s, e1, e2) -> (freeIn e1 x) || ((s <> x) && (freeIn e2 x))
   | Fun (s, e) -> (s <> x) && (freeIn e x)
   | App (e1, e2) -> (freeIn e1 x) || (freeIn e2 x)


(* P5_2. *)
let namecounter = ref 0
let newname () =
     ( namecounter := !namecounter + 1; "var" ^ string_of_int !namecounter)

(* subst: expr -> string -> expr -> expr
   precondition: input has an expression, a name of an identifier, and a new expression to substitute
   invariant: replace each occurrence of an identifier with a new expression
              based on some certain conditions *)
let rec subst e1 x e2 =
   let rec rename e old_name name =
      match e with
      | Id s -> if (s = old_name) then (Id name) else (Id s)
      | Int i -> Int i
      | True -> True
      | False -> False
      | Plus (e3, e4) -> Plus (rename e3 old_name name, rename e4 old_name name)
      | Minus (e3, e4) -> Minus (rename e3 old_name name, rename e4 old_name name)
      | Times (e3, e4) -> Times (rename e3 old_name name, rename e4 old_name name)
      | Div (e3, e4) -> Div (rename e3 old_name name, rename e4 old_name name)
      | Lss (e3, e4) -> Lss (rename e3 old_name name, rename e4 old_name name)
      | Eq (e3, e4) -> Eq (rename e3 old_name name, rename e4 old_name name)
      | Gtr (e3, e4) -> Gtr (rename e3 old_name name, rename e4 old_name name)
      | And (e3, e4) -> And (rename e3 old_name name, rename e4 old_name name)
      | Or (e3, e4) -> Or (rename e3 old_name name, rename e4 old_name name)
      | Not e -> Not (rename e old_name name)
      | Cond (e3, e4, e5) -> Cond (rename e3 old_name name, rename e4 old_name name, rename e5 old_name name)
      | Let (s, e3, e4) -> if (s = old_name) then Let (name, e3, rename e4 old_name name)
                           else Let (s, e3, rename e4 old_name name)
      | Fun (s, e) -> if (s = old_name) then Fun (name, rename e old_name name)
                      else Fun (s, rename e old_name name)
      | App (e3, e4) -> App (rename e3 old_name name, rename e4 old_name name)
   in match e1 with
   | Id s -> if (s = x) then e2 else e1
   | Int i -> Int i
   | True -> True
   | False -> False
   | Plus (e3, e4) -> Plus (subst e3 x e2, subst e4 x e2)
   | Minus (e3, e4) -> Minus (subst e3 x e2, subst e4 x e2)
   | Times (e3, e4) -> Times (subst e3 x e2, subst e4 x e2)
   | Div (e3, e4) -> Div (subst e3 x e2, subst e4 x e2)
   | Lss (e3, e4) -> Lss (subst e3 x e2, subst e4 x e2)
   | Eq (e3, e4) -> Eq (subst e3 x e2, subst e4 x e2)
   | Gtr (e3, e4) -> Gtr (subst e3 x e2, subst e4 x e2)
   | And (e3, e4) -> And (subst e3 x e2, subst e4 x e2)
   | Or (e3, e4) -> Or (subst e3 x e2, subst e4 x e2)
   | Not e -> Not (subst e x e2)
   | Cond (e3, e4, e5) -> Cond (subst e3 x e2, subst e4 x e2, subst e5 x e2)
   | Let (s, e3, e4) -> if (s = x) then Let (s, subst e3 x e2, e4)
                        else if (freeIn e2 s) then let new_name = newname () 
                                              in Let (new_name, subst e3 x e2, subst (rename e4 s new_name) x e2)
                         else Let (s, subst e3 x e2, subst e4 x e2)
   | Fun (s, e) -> if (s = x) then Fun (s, e)
                   else if (freeIn e2 s) then let new_name = newname() 
                                         in Fun (new_name, subst (rename e s new_name) x e2)
                        else Fun (s, subst e x e2)
   | App (e3, e4) -> App (subst e3 x e2, subst e4 x e2)
   
(* End of hw3.ml *)
