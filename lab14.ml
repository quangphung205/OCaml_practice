(* Name: Quang Phung
 * Course: CSCI 2041
 * Lab 14
 * Due date: 12/08/17
 *)


(* Problem 1 *)

(* The code that you have to consider in this problem *)

  let rec append l1 l2 =
     match l1 with
     | [] -> l2
     | (h::t) -> h::(append t l2)

  let head l =
     match l with
     | [] -> 0
     | (h::t) -> h

(* The expression whose evaluation you have to consider:
       head (append (append [1;2] [3]) [4])
Part 1: Show the steps in call-by-name evaluation below
  head (append (append [1;2] [3]) [4])
= head (append (1::(append [2] [3])) [4])
= head 1::(append (append [2] [3]) [4])
= 1


Part 2: Show the steps in call-by-value evaluation below
  head (append (append [1;2] [3]) [4])
= head (append (1::(append [2] [3])) [4])
= head (append (1::2::(append [] [3])) [4])
= head (append (1::2::[3]) [4])
= head (append [1;2;3] [4])
= head 1::(append [2;3] [4])
= head 1::2::(append [3] [4])
= head 1::2::3::(append [] [4])
= head 1::2::3::[4]
= head [1;2;3;4]
= 1

*)

(* Problem 2 *)

(* The code given to you at the outset *)

type 'a stream = Stream of (unit -> 'a * 'a stream)

let mkStream f = Stream f
let nextStream (Stream f) = f ()

let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))
let natStream = (fromNStream 1)

(* Write the definition of zipStreams below *)
(*
 * zipStreams : 'a stream -> 'b stream -> ('a * 'b) stream
 * precondition: input is two streams
 * invariant: output is the combined of 2 input streams
 *)
let rec zipStreams s1 s2 =
   let (i1, s1') = nextStream s1
   in let (i2, s2') = nextStream s2
      in mkStream (fun () -> ((i1,i2), zipStreams s1' s2'))


(* Problem 3 *)

(* The code given to you to analyze *)
let fib n =
   let rec fib' n i fib1 fib2 =
      if (n = i) then fib1
      else fib' n (i+1) fib2 (fib1+fib2)
   in fib' n 1 1 1

(* Part 1
   T(m) = c1          if  m = 0
        = T(m-1) + c2 if  m > 0 
*)

(* Part 2
   T(m) = T(m-1) + c2
        = T(m-2) + c2 + c2
        = T(0) + (m * c2)
	= c1 + (m * c2)

   Base: m = 0 => T(0) = c1
   Inductive steps: assume T(m) holds which means T(m) = c1 + (m * c2)
          Goal: show that T(m+1) holds
     T(m+1) = T(m) + c2
            = c1 + (m * c2) + c2  (by the assumption)
            = c1 + (m+1) * c2
         => T(m+1) holds
*)

(* Part 3
   Cost of fib = T(n-1) = c1 + (n - 1) * c2 = O(n)
*)
   

(* Problem 4 *)

(* The two functions to be analyzed *)
let rec exp m =
   function
     | 0 -> 1
     | n ->  m * exp m (n-1)


let rec exp' m =
  function
  | 0 -> 1
  | 1 -> m
  | n ->
    let b = exp' m (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else m)

(* Part 1 

    c1 = the cost of returning from a recursive call
    c2 = the cost of multiplication
    T(n) = c1             if n = 0
         = T(n-1) + c2    if n > 0

    Solve:
    T(n) = T(n-1) + c2
         = T(n-2) + c2 + c2
         = T(0) + (n * c2)
         = c1 + (n * c2)  
*)

(* Part 2
 
   c1 = the cost of returning from a recursive call
   c2 = the cost of multiplication
   T'(n) = c1                       if n = 0, n = 1
        = T'(n/2) + c1 + c2 + c2    if n > 1

   Solve:
   T'(n) = T'(n/2) + c1 + c2 + c2
        = (T'(n/4) + c1 + c2 + c2) + (c1 + c2 + c2)
        = T'(1) + (logn) * (c1 + c2 + c2)
        = c1 + (logn) * (c1 + c2 + c2)
 *)

(* Part 3
   T(n) = O(n)
   T'(n) = O(lgn)
   T(n) = sqrt(2) ^ T'(n)
 *)
