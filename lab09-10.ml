(* Name: Quang Phung
 * Course: CSCI 2041
 * Lab 09-10
 * Due Date: 11/10/17 *)

(* Solution to Problem 1 *)
(* Problem 1, Part 1
 * a state should represent 5 variables: i, n, temp, fib1, fib2
 * all of them have int type
 *)
type state = int * int * int * int * int

(* Problem 1, Part 2 *)
let getI (i,n,temp,fib1,fib2) = i
let getN (i,n,temp,fib1,fib2) = n
let getTemp (i,n,temp,fib1,fib2) = temp
let getFib1 (i,n,temp,fib1,fib2) = fib1
let getFib2 (i,n,temp,fib1,fib2) = fib2

let putI exp s =
   let (i,n,temp,fib1,fib2) = s in (exp s,n,temp,fib1,fib2)
let putN exp s =
   let (i,n,temp,fib1,fib2) = s in (i,exp s,temp,fib1,fib2)
let putTemp exp s =
   let (i,n,temp,fib1,fib2) = s in (i,n,exp s,fib1,fib2)
let putFib1 exp s =
   let (i,n,temp,fib1,fib2) = s in (i,n,temp,exp s,fib2)
let putFib2 exp s =
   let (i,n,temp,fib1,fib2) = s in (i,n,temp,fib1,exp s)

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

let one = fun s -> 1
  
let i_not_equal_n = (fun s -> (getI s) <> (getN s))
  
let i_plus_one = (fun s -> (getI s) + 1)
  
let fib2_plus_temp = (fun s -> (getFib2 s) + (getTemp s))

let fibprog =
   seq (putI one)
       (seq (putFib1 one)
            (seq (putFib2 one)
	        (whilestat (i_not_equal_n)
		    (seq (putTemp getFib1)
		         (seq (putFib1 getFib2)
			      (seq (putFib2 fib2_plus_temp)
			           (putI i_plus_one)))))))

(* P1_4. *)
(* fib7: unit -> int
 * precondition: nothing is required for the input
 * invariant: output is the 7th Fibonnaci number
 *)
let fib7 () = let (i,n,temp,fib1,fib2) = fibprog (0,7,0,0,0) in fib1

(* fib: int -> int							  
 * precondition: input is a number n
 * invariant: output is the nth Fibonnaci number 
 *)
let fib n =
   let (i,n',temp,fib1,fib2) = fibprog (0,n,0,0,0)
      in fib1


(* Solution to Problem 2 *)
(* Problem 2, Part 1*)
(* 2.1.1
 * Property: (sumup n) evaluates to (1 + 2 + 3 + ... + n)
 *
 * 2.1.2
 * Base: (sumup 0) evaluates to 0 (true) => P(0) holds
 *
 * Induction:
 *   - Assume that P(n) holds => (sumup n) evaluates to (1 + 2 + 3 + ... + n)
 *   - Show that P(n+1) holds => (sumup (n+1)) evaluates to (1 + 2 + 3 + ... + n + n+1)
 *
 *     (sumup (n+1)) evaluates to (n+1 + (sumup n)) which evaluates to
 *     (n+1 + (1 + 2 + 3 + ... + n)) by the assumption of P(n)
 *     = (1 + 2 + 3 + ... + n + n+1)
 *  => P(n+1) holds true
 *)

(* Problem 2, Part 2 *)
(* 2.2.1
 * Property: (sumup_aux n acc) evaluates to (1 + 2 + 3 + ... + n + acc)
 *
 * 2.2.2
 * Base: (sumup 0 acc) evaluates to acc which is equal to (0 + acc) (true)
         => P(0) holds true
 *
 * Induction:
 *   - Assum that P(n) holds => (sumup_aux n acc) evaluates to (1 + 2 + 3 + ... + n + acc)
 *   - Show that P(n+1) holds => (sumup_aux (n+1) acc) evaluates to (1 + 2 + 3 + ... + n + n+1 + acc)
 *
 *     (sumup_aux (n+1) acc) evaluates to (sumup_aux n (n+1 + acc)
 *     which in turn evaluates to (1 + 2 + 3 + ... + n + (n+1 + acc)) by the assumption of P(n)
 *  => P(n+1) holds true
 *
 * P2.2_3.
 *   (sumup' n) evaluates to whatever (sumup_aux n 0) evaluates to
 *   (sumup_aux n 0) evaluates to (1 + 2 + 3 + ... + n + 0)
 *    => (sumup' n) evaluates to (1 + 2 + 3 + ... + n)
 *)


(* Solution to Problem 3 *)
(* Goal:
 *   for all n2 in nat,
 *     for all n1 in nat, toInt (multNat n1 n2) = (toInt n1) * (toInt n2)
 *
 * Base: P(Zero) holds true
 *     for all n2 in nat,
 *       (toInt (multNat Zero n2)) evaluates to (toInt Zero) which in turn evaluates to 0
 *   Also, (toInt Zero) * (toInt n2) = 0 * (toInt n2) = 0
 *
 * Induction:
 *  Assume that P(n1) holds true which means:
 *    for all n2 in nat,
 *      for all n1 in nat, toInt (multNat n1 n2) = (toInt n1) * (toInt n2)
 *  Show that P(Succ n1) holds true which means:
 *    for all n2 in nat,
 *      for all n1 in nat, toInt (multNat (Succ n1) n2) = (toInt (Succ n1)) * (toInt n2)
 *
 *    toInt (multNat (Succ n1) n2)
 *  = toInt (plusNat n2 (multNat n1 n2))
 *  = (toInt n2) + (toInt (multNat n1 n2)) by the property of plusNat
 *  = (toInt n2) + ((toInt n1) * (toInt n2)) by the assumption of P(n)
 *  = (1 + (toInt n1)) * (toInt n2) by factoring out (toInt n2)
 *  = (toInt (Succ n1)) * (toInt n2)
 *  => P(Succ n) holds true
 *)


(* Solution to Problem 4 *)
(* Base: P([]) holds true
        for all l of type (int list), sumlist (reverse []) = sumlist []
 *
 * Induction:
        for all l of type (int list), assume that P(l) holds which means
          sumlist (reverse l) = sumlist l
    Goal: show that for all y of type int, P(y::l) holds which means
        for all y of type int, for all l of type (int list),
          sumlist (reverse (y::l)) = sumlist (y::l)

    LHS evaluates to (sumlist (append (reverse l) [y]))
                   = (sumlist (reverse l)) + (sumlist [y]) (by the property of append)
                   = (sumlist (reverse l)) + y + (sumlist [])
                   = (sumlist (reverse l)) + y + 0
                   = (sumlist (reverse l)) + y

    RHS evaluates to (sumlist (reverse l)) + y
      => the LHS evaluates to the same value as the RHS
      => P(y::l) holds true
 *)
                          

(* Solution to Problem 5 *)
(* Problem 5, Part 1: minTree
 * Base: show that P(Empty) holds true
     for all x of type 'a, for all t' of type ('a btree),
        if (insert Empty x) evaluates to t' then either
            minTree t' = (minTree Empty) or minTree t' = Some x

      (insert Empty x) evaluates to Node(x, Empty, Empty)
      minTree Node(x,Empty,Empty) evaluates to (Some x)
        => P(Empty) holds
 *
 * Induction:
     for all x of type 'a, for all l,r,t' of type ('a btree), assume that
          P(l) holds true which means
       if (insert l x) evaluate to t' then either
          (minTree t' = minTree l) or (minTree t' = Some x)

          P(r) holds true which means
       if (insert r x) evaluate to t' then either
          (minTree t' = minTree r) or (minTree t' = Some x)

    Goal: for all y of type 'a, show that P(Node(y, l, r)) holds which means
     for all x,y of type 'a, for all l,r,t' of type ('a btree),
       if (insert Node(y,l,r) x) evaluates to t' then either
          (minTree t' = minTree Node(y,l,r)) or (minTree t' = Some x)

     Case x < y:
        (insert Node(y,l,r) x) evaluates to Node(y, insert l x, r)
        minTree Node(y, insert l x, r) evaluates to whatever (minTree (insert l x)) evaluates to
          which is true based on the assumption of P(l)
        => this case holds true

     Case x >= y:
        (insert Node(y,l,r) x) evaluates to Node(y, l, insert r x)
        minTree Node(y, l, insert r x) evaluates to whatever (minTree l)) evaluates to
         (minTree l) evaluates to the same values as (minTree Node(y,l,r))
        => this case holds true as well
      => P(Node(y, l, r)) holds true
 *)

(* Problem 5, Part 2: maxTree
 * Base: show that P(Empty) holds true
     for all x of type 'a, for all t' of type ('a btree),
        if (insert Empty x) evaluates to t' then either
            maxTree t' = (maxTree Empty) or maxTree t' = Some x

      (insert Empty x) evaluates to Node(x, Empty, Empty)
      maxTree Node(x,Empty,Empty) evaluates to (Some x)
        => P(Empty) holds
 *
 * Induction:
     for all x of type 'a, for all l,r,t' of type ('a btree), assume that
          P(l) holds true which means
       if (insert l x) evaluate to t' then either
          (maxTree t' = maxTree l) or (maxTree t' = Some x)

          P(r) holds true which means
       if (insert r x) evaluate to t' then either
          (maxTree t' = maxTree r) or (maxTree t' = Some x)

    Goal: for all y of type 'a, show that P(Node(y, l, r)) holds which means
     for all x,y of type 'a, for all l,r,t' of type ('a btree),
       if (insert Node(y,l,r) x) evaluates to t' then either
          (maxTree t' = maxTree Node(y,l,r)) or (maxTree t' = Some x)

     Case x < y:
        (insert Node(y,l,r) x) evaluates to Node(y, insert l x, r)
        maxTree Node(y, insert l x, r) evaluates to whatever (maxTree r) evaluates to
          (maxTree r) evaluates to the same value as (maxTree Node(y,l,r))
        => this case holds true
          
     Case x >= y:
        (insert Node(y,l,r) x) evaluates to Node(y, l, insert r x)
        maxTree Node(y, l, insert r x) evaluates to whatever (maxTree (insert r x)) evaluates to
          which is true based on the assumption of P(r)
        => this case holds true as well
    => P(Node(y, l, r)) holds true
 *)

(* End of lab09-10.ml *)
