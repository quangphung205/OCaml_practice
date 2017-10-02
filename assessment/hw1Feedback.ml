(* Solution to problem 1
1. It is well-typed. Its type is int and the value is -4
2. It is not well-typed. The '+' operator can only be used between 2 integers.
   In this expression, 3.14 is a float.
3. It is not well-typed. The '*.' operator can only be used between 2 floating-point numbers.
   In this expression, 7 is an integer.
4. It is well-typed. Its type is int and the value is 7
5. It is not well-typed. 5 has type int, whereas "hello" has type string.
   They should have the same type.
6. It is not well-typed. 7 has type int, but the 'else' branch has type unit.
   They should have the same type.
7. It is well-typed. Its type is string and the value is "hello world"
8. It is well-typed. Its type is a tuple of a function and a string.
   Its value is a tuple of the defined function f and the string "hello"
*)
(* 8/8 *)


(* Solution to problem 2
1. It is not legal because y is not defined yet.
2. It is legal. Its type is int and its value is 3
3. It is not legal because when we use 'and', we define x and y seperately.
   So, x is not visible to the expression 'y = x + 1'
4. It is legal. Its type is int and its value is 3
5. It is legal. Its type is int and its value is 5

Problem 2 Score: 5/5
*)


(* Solution to problem 3 *)

(* gcd: int -> int -> int
   precondition: input must be 2 positive integers
   invariant: output is the GCD of the inputs *)
let rec gcd a b =
   if (a = b) then a
   else if (a < b) then gcd a (b - a)
   else gcd (a - b) a


(* Solution to problem 4 *)

(* reduced_form: int * int -> int * int
   precondition: input is a tuple of 2 positive integers
   invariant: output is a tuple of a reduced form of 2 integers *)
let reduced_form (a, b) =
   let gcd_ab = gcd a b in
      (a / gcd_ab, b / gcd_ab)

      (* SCORE (Prob 4): 5/5
      *)


(* Solution to problem 5 *)

(* fromMtoN: int -> int -> (int list)
   precondition: input is two integers m & n
   invariant: output is a list of integers from m to n *)
let rec fromMtoN m n =
   if (m > n) then [] else m :: fromMtoN (m + 1) n
(* 5/5 *)

(* Solution to problem 6 *)

(* everyEven: ('a list) -> ('a list)
   precondition: input is a list of any type
   invariant: output is a list of even-index elements of input *)
(* Score: 5/5 *)
let everyEven lst =
   let rec traverse index lst =
      match lst with
      [] -> []
    | h :: l when (index mod 2 = 0) -> h :: traverse (index + 1) l
    | _ :: l when (index mod 2 != 0) -> traverse (index + 1) l
   in
      traverse 1 lst


(* Solution to problem 7 *)

(* everyNth: ('a list) -> int -> ('a list)
   precondition: a list and a positive integer N
   invariant: a list of elements at every N index *)
let everyNth lst step =
   let rec traverse index lst step =
      match lst with
      [] -> []
    | h :: l when (index mod step = 0) -> h :: traverse (index + 1) l step
    | _ :: l when (index mod step != 0) -> traverse (index + 1) l step
   in
      traverse 1 lst step

(*
  Problem 7 Score: 5/5
 *)

(* Solution to problem 8 *)

(* find_salary: ((string * string * float) list) -> string -> float
   precondition: a list of employees and an employee name
   invariant: a salary of a requesting employee
In case a certain name of employee does not exist, -1.0 will be return as the output *)
let rec find_salary (lst: (string * string * float) list) (str: string) =
   match lst with
    [] -> -1.0
  | (name, _, salary) :: l when (name = str) -> salary
  | _ :: l -> find_salary l str

(* find_phno: ((string * string * float) list) -> string -> string
   precondition: a list of employees and an employee name
   invariant: a phone number of a requesting employee
In case a certain name of an employee does not exist,
a string "DNE" will be return as the output *)
let rec find_phno (lst: (string * string * float) list) (str: string) =
   match lst with
    [] -> "DNE"
  | (name, phone, _) :: l when (name = str) -> phone
  | _ :: l -> find_phno l str

(* Score: 8/8 *)

(* Solution to problem 9 *)

(* is_matrix: (int list) list -> bool
   precondition: a 2x2 matrix represented by a list of a list of integers
   invariant: true if every row has the same number of column, false otherwise

Define a sub-function col_count which takes in a row of a matrix as
an argument and returns its number of columns *)
let rec is_matrix (mat: (int list) list) =
   let rec col_count = function
      [] -> 0
    | _ :: l -> 1 + (col_count l)
   in
      match mat with
         [] -> true
       | a :: b :: l when ((col_count a) != (col_count b)) -> false
       | _ :: l -> is_matrix l


(*
 * -.5: if you accept [[]], a 1x0 matrix, how do you represent a 0x1 matrix?
 * Total 4.5/5
 *)

(* matrix_scalar_multiply: (int list) list -> int -> (int list) list
   precondition: a matrix and an integer to multiply
   invariant: a new matrix by multiplying an integer to the old matrix

Define a sub-function row_scalar_multiply to multiply an integer to one row *)
let rec matrix_scalar_multiply (mat: (int list) list) (num: int) =
   let rec row_scalar_multiply row n =
      match row with
        [] -> []
      | a :: l -> (a * n) :: (row_scalar_multiply l n)
   in
      match mat with
        [] -> []
      | a :: l -> (row_scalar_multiply a num) :: (matrix_scalar_multiply l num)

(* Total 3/3 *)
(* End of homework 1 *)
