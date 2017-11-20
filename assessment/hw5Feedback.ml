(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 5
 * Due Date: 11/10/17
 *)

(* Solution to Problem 1 *)
(* Problem 1, Part 1
 * A state should represent two variables rev and l each of which is an int list *)
type state1 = (int list) * (int list)

(* Problem 1, Part 2 *)
let getRev (rev, l) = rev
let getL (rev, l) = l

let putRev exp s = let (rev, l) = s in (exp s, l)
let putL exp s = let (rev, l) = s in (rev, exp s)

(* Problem 1, Part 3 *)
let seq stat1 stat2 =
        fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let rec whilestat exp stat =
   fun s ->
      ifstat exp
             (seq stat (whilestat exp stat))
             (fun x -> x) s

let emptyList = fun s -> []

let l_not_empty_list = fun s -> (getL s) <> []

let l_get_head = fun s -> let (h::t) = (getL s) in h

let l_get_tail = fun s -> let (h::t) = (getL s) in t

let append_lHead_to_rev =
   fun s -> let rev = getRev s
            in let x = l_get_head s
	       in x::rev

let revprog =
   (seq (putRev emptyList)
        (whilestat (l_not_empty_list)
	    (seq (putRev append_lHead_to_rev)
	         (putL l_get_tail))))

(* Problem 1, Part 4 *)
(* revlist: (int list) -> (int list)
 * precondition: input is a list of integers
 * invariant: output is a list in reverse order of input list *)
let revlist l = let (result, l1) = revprog ([], l) in result

(* Problem 1 score:
 * 1.1 : 2/2
 * 1.2 : 2/2
 * 1.3 : 4/4
 * 1.4 : 2/2
 * Comments:
 *)

(* Solution to Problem 2 *)
(* Problem 2, Part 1 *)
(* Problem 2 grade:
  * Part 1: 5/5
  * Part 2: 5/5
  * Part 3: 2/2
  * Comments:
*)

let seq stat1 stat2 =
        fun s -> (stat2 (stat1 s))

let ifstat exp stat1 stat2 =
    fun s -> if (exp s) then (stat1 s)
             else (stat2 s)

let rec dostat stat exp =
   fun s ->
      (seq stat
         (ifstat exp (dostat stat exp)
	             (fun x -> x)) s)

(* Problem 2, Part 2 *)
(* 2.2.a
 * A state should represent 3 variables: i, sum, and n.
 * All of these variables have an int type.
 * Type of state: int * int * int
 *)
type state2 = int * int * int

(* 2.2.b *)
let getI (i, n, sum) = i
let getN (i, n, sum) = n
let getSum (i, n, sum) = sum

let putI exp s =
   let (i, n, sum) = s in (exp s, n, sum)
let putN exp s =
   let (i, n, sum) = s in (i, exp s, sum)
let putSum exp s =
   let (i, n, sum) = s in (i, n, exp s)

(* 2.2.c *)
let zero = fun s -> 0

let i_plus_one = fun s -> (getI s) + 1

let sum_plus_i = fun s -> (getSum s) + (getI s)

let i_less_than_n = fun s -> (getI s) < (getN s)

let sumup =
    (seq (putI zero)
         (seq (putSum zero)
	      (dostat (seq (putI i_plus_one)
	                   (putSum sum_plus_i))
		      i_less_than_n)))

(* 2.2.d *)
(* sumToN: int -> int
 * precondition: input is a positive integer n
 * invariant: output is a sum of integers from 1 to n
 *)
let sumToN n =
   let (i, n, sum) = sumup (0, n, 0)
   in sum


(* Solution to Problem 3 *)
(* Problem 3, Part 1
 * Let's define a mathematical notion of fib'
 * for all integers f, s
 * math_fib'(n) = f                               if n = 1
                = s                               if n = 2
		= math_fib'(n-1) + math_fib'(n-2) if n > 2
 * Property of fib': fib'(n, m, f, s) evaluates to math_fib'(n-m+1)
 *)

(* Problem 3, Part 2
 * Base: for n = m, fib'(n, m, f, s) evaluates to math_fib'(n-m+1) = math_fib'(1) = f (true)
         => P(0) holds
 *
 * Induction: for all n > m, assume that P(n) holds which means
                fib'(n, m, f, s) evaluates to math_fib'(n-m+1)
       show that for all n > m, P(n+1) holds which is to show that
                fib'(n+1, m, f, s) evaluates to math_fib'(n+1-m+1)

      fib'(n+1, m, f, s) evaluates to fib'(n, m, f, s) + fib'(n-1, m, f, s)
   => fib'(n+1, m, f, s) evaluates to math_fib'(n-m+1) + math_fib'(n-1-m+1) (by the assumption)
   => fib'(n+1, m, f, s) evaluates to math_fib'(n-m+1) + math_fib'(n-m)
   => fib'(n+1, m, f, s) evaluates to math_fib'(n-m+2) (by the mathematical notion)
   => P(n+1) holds
 *)

(* Problem 3, Part 3
 * (fib n) evaluates to whatever (fib' (n, 1, 1, 1)) evaluates to
 * with m = 1, fib'(n, 1, 1, 1) evaluates to math_fib'(n-1+1) = math_fib'(n)
  * with f = 1, s = 1, math_fib'(n) becomes a Fibonnaci sequence
   => fib'(n, 1, 1, 1) evaluates to the nth Fibonnaci number
  * Conclusion: (fib n) evalutes to the nth Fibonnaci number.
  *)
(* problem 3
  part1: 0/3
  part2: 0/5
  part3: 0/2
  wrong property
  total: 0/10
*)

(* Solution to Problem 4 *)
(* Problem 4, Part 1 *)
(* 4.1.a
 * Property of rev: (rev lst1 lst2) evaluates to (lst1^R) + lst2
 *)

(* 4.1.b
 * Base: P([]) holds
    for all lst2 of 'a list, (rev [] lst2) evaluates to lst2 which is equivalent to ([]^R) + lst2
 *
 * Induction: for all lst1, lst2 of 'a list, assume that P(lst1) holds which means
     for all lst2, for all lst1, (rev lst1 lst2) evaluates to (lst1^R) + lst2
    Goal: show that P(y::lst1) holds which means
     for all lst2 of 'a list, for all lst1 of 'a list, for all y of 'a
     (rev y::lst1 lst2) evaluates to ((y::lst1)^R) + lst2

    LHS evaluates to (rev lst1 (y::lst2))
    RHS evaluates to (lst1^R + [y]) + lst2
                   = (lst1^R) + ([y] + lst2)
		   = (lst1^R) + (y::lst2)
    By the assumption, the LHS is equivalent to the RHS
    => P(y::lst1) holds
 *)

(* 4.1.c
 * (reverse lst) evaluates to whatever (rev lst []) evaluates to
 * Based on the property of rev, (rev lst []) evaluates to ((lst^R) + []) which is equivalent to (lst^R)
 * Consequently, (reverse lst) evaluates to (lst^R)
 *)

(* Problem 4, Part 1 Score:
 * 4.1.a: 2/2
 * 4.1.b: 4/4
 * 4.1.c: 2/2
 *)


(* Problem 4, Part 2 *)
(* 4.2.a
 * Property of rev based on length: length (rev lst1 lst2) evaluates to (length lst1) + (length lst2)
 *)

(* 4.2.b
 * Base: P([]) holds
    for all lst2 of 'a list, (length (rev [] lst2)) evaluates to (length lst2)
    which is equivalent to (length []) + (length lst2)
 *
 * Induction:
    for all lst2 of 'a list, for all lst1 of 'a list,  assume that P(lst1) holds which means
     (length (rev lst1 lst2)) evaluates to (length lst1) + (length lst2)
   Goal: show that P(y::lst1) holds which means
    for all lst2 of 'a list, for all lst1 of 'a list, for all y of 'a
     (length (rev (y::lst1) lst2)) evaluates to (length y::lst1) + (length lst2)

   LHS evaluates to (length (rev lst1 (y::lst2)))
                  = (length lst1) + (length y::lst2) (by the assumption)
		  = (length lst1) + (length lst2) + 1
   RHS evaluates to 1 + (length lst1) + (length lst2)
    => the LHS is equivalent to the RHS
    => P(y::lst1) holds
 *)

(* 4.2.c
 * (length (reverse l)) evaluates to (length (rev l [])) (based on part 4.1)
   which in turn evaluates to (length l) + (length []) which evaluates to (length l)
 * Consequently, (length (reverse l)) evaluates to (length l)
 *)

(*
 * Problem 4.2 (a): 1/2
 * Problem 4.2 (b): 3.5/4
 * Problem 4.2 (c): 2/2
 * Total: 6.5/8
 * Comments: (a) missing quantifiers (b) need to complete your base case. Need more explanation
 * on how things evaluate, def of rev, def of length, mathematical property, etc
 *)

(* Solution to Problem 5 *)
(* Base: P(Empty) holds
    for all x, x' of 'a, for all 't of 'a btree, (insert Empty x) evaluates to t'
    case 1: x = x', (find t' x') evaluates to true
    case 2: x != x', (find t' x') evaluates to false
 *
 * Induction: for all x of 'a, for all l, r, t' of 'a btree, assume that
     1. P(l) holds => suppose that (insert l x) evaluates to t', (find l x') evaluates to true
        only if either x = x' or (find l x') evaluates to true
     2. P(r) holds => suppose that (insert l x) evaluates to t', (find l x') evaluates to true
        only if either x = x' or (find r x') evaluates to true

   Goal: show that for all i of 'a, P(Node(i, l, r)) holds which means
     for all x, x' of 'a, for all l, r, t' of 'a btree, suppose that (insert Node(i, l, r) x) evaluates to t'
     (find t' x') evaluates to true only if x = x' or (find Node(i, l, r) x) evaluates to true

    Suppose that (find t' x') evaluates to true
    Case x' = i: this is obviously true => P(Node(i, l, r)) holds
    Case x' < i: (find Node(i, l, r) x') evaluates to (find l x') which in turn evaluates to true
                 due to the assumption of P(l)
    Case x' > i: (find Node(i, l, r) x') evaluates to (find r x') which in turn evaluates to true
                 due to the assumption of P(r)

    => for all i of 'a, P(Node(i, l, r)) holds true

   Conclusion: for all x of 'a, for all t of 'a btree
                for all x' of 'a, for all t' of 'a btree
		  suppose (insert t x) evaluates to t'
		  (find t' x') evaluates to true only if x = x' or (find t x') evaluates to true
 *)

 (* PROBLEM 5 SCORE: 4/8
 * -4 missing 3 cases proved on the right and left branches of insert individually
 *)

(* End of hw5.ml *)
