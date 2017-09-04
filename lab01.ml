(* A function to compute the nth Fibonacci number *)

let rec fib n =
  if n < 3 
    then 1 
    else fib (n-2) + fib (n-1)
