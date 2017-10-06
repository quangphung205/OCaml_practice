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
let e1 = Plus (Int 5, Int 3)
