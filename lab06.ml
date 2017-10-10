(* Name: Quang Phung
   Course: CSCI 2041
   Lab06.ml
   Due date: 10/13/17 *)

(* Solution to Problem 1 *)
(* P1_1. *)
type empItemTy = { name : string; phone : string; salary : float }                    
(* P1_2. *)
let smalldb = [ { name = "John"; phone = "x3456"; salary = 50.1 };
		{ name = "Jane"; phone = "x1234"; salary = 107.3 };
		{ name = "Joan"; phone = "unlisted"; salary = 12.7 }]

(* P1_3. *)
(* find_salary: (empItemTy list) -> string -> float option
   precondition: input is a list of employees and an employee's name
   invariant: returns a salary of a specific employee *)
let rec find_salary l n =
  match l with
  | [] -> None
  | { name = x; salary = s }::t -> if (x = n) then Some s
                                   else find_salary t n

(* find_phno: (empItemTy list) -> string -> string option
   precondition: input is a list of employees and an employee's name
   invariant: returns a phone number of a specific employee *)
let rec find_phno l n =
  match l with
  | [] -> None
  | { name = x; phone = p }::t -> if (x = n) then Some p
                                  else find_phno t n

      
(* Solution to Problem 2 *)
(* P2_1. *)
type 'a item = Item of 'a | NL of ('a item) list
type 'a nestedlist = ('a item) list

(* P2_2. *)
let intlist1 = [Item 1; NL [Item 2; NL [Item 3]; Item 4]; NL [NL [Item 5; Item 6; NL [Item 7]]]]

(* P2_3. *)
(* flatten: ('a nestedlist) -> ('a list)
   precondition: input is a nested list of some type
   invariant: returns a simple list of that type *)
let rec flatten (nl: 'a nestedlist) =
  let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | h::t -> h::(append t l2)
  in match nl with
  | [] -> []
  | (Item x)::t -> x::(flatten t)
  | (NL l)::t -> append (flatten l) (flatten t)

     
(* Solution to Problem 3 *)
(* P3_1. *)
type form = Pos of string | Neg of string
            | And of form * form | Or of form * form

(* P3_2. *)
(* cnf: form -> form
   precondition: input is a formula of type form
   invariant: output is a cnf form of input *)
let rec cnf f =
  match f with
  | Pos s -> Pos s
  | Neg s -> Neg s
  | And (f1, f2) ->
   ( match (f1, f2) with
     | (Or (f3, f4), f5) -> Or (cnf (And (f3, f5)), cnf (And (f4, f5)))
     | (f3, Or (f4, f5)) -> Or (cnf (And (f3, f4)), cnf (And (f3, f5)))
     | (f3, f4) -> And (f3, f4) )
  | Or (f1, f2) ->
   ( match (f1, f2) with
     | (And (f3, f4), f5) -> And (cnf (Or (f3, f5)), cnf (Or (f4, f5)))
     | (f3, And (f4, f5)) -> And (cnf (Or (f3, f4)), cnf (Or (f3, f5)))
     | (f3, f4) -> Or (f1, f2) )

(* End of lab06.ml *)
