(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 8
 * Due Date: 12/14/17
 *)

(* Problem 1, Part 1 Score:
 * Part 1(a): 2/2
 * Part 1(b): 1/1
 * Part 1(c): 2/2
 * TOTAL: 5/5
 * Comments:
 *)

(* Solution to Problem 1 - Part 1 *)
(* Part a
T(n) = c1            if n = 0
     = c1            if n = 1
     = T(n-2) + c2   if n > 1
 *)

(* Part b
  Assume that n is a power of 2
T(n) = T(n-2) + c2
     = T(n-4) + c2 + c2
     = T(0) + (n/2)*c2
     = c1 + (n/2)*c2
 *)

(* Part c
Base: T(0) and T(1) hold
     T(0) = c1 + (0/2)*c2 = c1   => true
     T(1) = c1 + (1/2)*c2 = c1   => true

Inductive steps:
   Assume that T(n) and T(n-1) hold
    -> T(n) = c1 + (n/2)*c2
       T(n-1) = c1 + ((n-1)/2)*c2
   Goal: show that T(n+2) and T(n+1) hold

T(n+2) = T(n) + c2
       = c1 + (n/2)*c2 + c2  (by the assumption of T(n))
       = c1 + ((n+2)/2)*c2
    -> T(n+2) holds

T(n+1) = T(n-1) + c2
       = c1 + ((n-1)/2)*c2 + c2
       = c1 + ((n+1)/2)*c2
    -> T(n+1) holds
 *)

(* Solution to Problem 1 - Part 2 *)
(* Part a
The worst case for merge is when both lists are in sorted order, both have the same length, and their element
are interleaving with each other. In other words, after picking an element from one list,
merge would pick an element from another list when it is called the next time and so on.
 *)

(* Part b
Case 1: n1 < n2 -> merge will be called (2*n1) times (worst case)
   T(n) = 2 * n1 = n1 + n1 < (n1 + n2) < 2*(n1 + n2)

Case 2: n2 < n1 -> merge will be called (2*n2) times (worst case)
   T(n) = 2 * n2 = n2 + n2 < (n1 + n2) < 2*(n1 + n2)

Case 3: n1 = n2 -> merge will be called (n1 + n2) times (worst case)
   T(n) = n1 + n2 < 2*(n1 + n2)

For c > 1, T(n) <= c*(n1+n2)
        -> T(n) = O(n1 + n2)
 *)


(* Problem 1, Part 2 Score:
 * Part 2(a): 2/2
 * Part 2(b): 2/2
 * TOTAL: 4/4
 * Comments:
 *)
(* Solution to Problem 1 - Part 3 *)
(* Part a
Running time of
   partition <= c*n         (by part 1)
       merge <= c*(n1 + n2) (by part 2)

S(n) <= c*n + S(n/2) + S(n/2) + c*(n/2 + n/2)
     <= 2*S(n/2) + c*n + c*n
     <= 2*S(n/2) + 2*c*n
 *)

(* Part b
S(n) = c1 if n = 0
S(n) = c1 if n = 1
For n > 1:
S(n) <= 2*S(n/2) + 2*c*n
     <= 2 * (2*S(n/4) + 2*c*n/2) + 2*c*n
     <= 4*S(n/4) + (2+2)*c*n
     <= 4 * (2*S(n/8) * 2*c*n/4) + 4*c*n
     <= 8*S(n/8) + (2+2+2)*c*n
     ...
     <= n*S(n/n) + (2+2+2+...+2)*c*n
     <= n*S(1) + 2*c*n*log2(n)
     <= n*c1 + 2*c*n*log2(n)
 *)

 (* SCORE Problem 1, Part 3 (a) and (b):   4/4
 *   Part 3a:   2/2
 *       +1 for answer
 *       +1 for how you got the answer
 *   Part 3b:   2/2
 *       +2 for a reasonable guess
 *)

(* Part c
Base: S(0) and S(1) hold
    S(0) = c1
    S(1) = 1*c1 + 2*c*1*log2(1) = c1  => true

Inductive steps:
    Using strong induction, assume that for m < n, S(m) holds
     -> for m = n/2 < n, S(n/2) holds
     which means S(n/2) <= (n/2)*c1 + 2*c*(n/2)*log2(n/2)

   Goal: show that S(n) holds

S(n) <= 2*S(n/2) + 2*c*n
     <= 2 * ((n/2)*c1 + 2*c*(n/2)*log2(n/2)) + 2*c*n  (by the assumption of S(n/2))
     <= n*c1 + 2*c*n*log2(n/2) + 2*c*n
     <= n*c1 + 2*c*n*(log2(n) - 1) + 2*c*n
     <= n*c1 + 2*c*n*log2(n) - 2*c*n + 2*c*n
     <= n*c1 + 2*c*n*log2(n)
   -> S(n) holds
 *)

(* Part d
S(n) <= c1*n + 2*c*n*log2(n)
     <= n * (c1 + 2*c*log2(n))
     <= n * (c1*log2(n) + 2*c*log2(n))  (n >= 2)
     <= n * log2(n) * (c1 + 2*c)        (n >= 2)
     <= k * n * log2(n)                 (k >= c1 + 2*c)

For some fixed constants c1 and c, choose n > 2, k > (c1 + 2*c) we have
S(n) <= k*n*log2(n)
-> S(n) = O(n*log2(n))
The worst-case running time for mergesort is O(n*log2(n))
 *)

 (* Problem 1, Part 3 (c) (d) Score:
  * Part 3(c): 1/1
  * Part 3(d): 1/1
  *)


type color = R | B
type 'a rbtree =
    Empty
| Node of color * 'a * 'a rbtree * 'a rbtree

(* Solution to Problem 2 - Part 1
 * is_RBTree_aux : 'a rbtree -> int * bool
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return true and the height of the tree
              otherwise, return false
 *)
let rec is_RBTree_aux t =
   let check_color t =
      match t with
      | Empty -> true
      | Node (c, _, l, r) ->
         (match (l,r) with
          | (Empty, Empty) -> true
          | (Empty, Node (c', _, _, _))
          | (Node (c', _, _, _), Empty) ->
              if (c' = B) then true else false
          | (Node (c1, _, _, _), Node (c2, _, _, _)) ->
              if ((c1 = B) && (c2 = B)) then true else false
         )
   in let rec is_RBTree_helper t n =
         match t with
         | Empty -> (n, true)
         | Node (c, _, l, r) as node ->
             (match c with
              | B -> let (nl, bl) = is_RBTree_helper l (n+1)
                     in let (nr, br) = is_RBTree_helper r (n+1)
                     in if (nl = nr && bl = true && br = true) then (nl, true) else (nl, false)
              | R -> if (check_color node) then
                       let (nl, bl) = is_RBTree_helper l n
                       in let (nr, br) = is_RBTree_helper r n
                       in if (nl = nr && bl = true && br = true) then (nl, true) else (nl, false)
                     else (n, false)
             )
       in is_RBTree_helper t 0

(* Solution to Problem 2 - Part 2
 * is_RBTree : 'a rbtree -> bool
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return true,
              otherwise, return false
 *)
let is_RBTree t = let (_, b) = is_RBTree_aux t in b

(* Solution to Problem 2 - Part 3
 * bh_RBTree : 'a rbtree -> int option
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return the height of the tree in option type
              otherwise, return None
 *)

let bh_RBTree t = let (n, b) = is_RBTree_aux t in
                    if (b = true) then Some n else None

(* Problem 2 Score:
 * is_RBTree_aux: 5/5
 * is_RBTree: 1/1
 * bh_RBTree: 1/1
 * TOTAL: 7/7
 * Comments:
 * (if X then true else false) is redundant.
 * (X = true) is also redundant.
 *)

(* End of hw8.ml *)
