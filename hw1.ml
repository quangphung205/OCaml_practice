(* Solution to problem 1
1. It is well-typed. Its type is int and the value is -4
2. It is not well-typed. The '+' operator can only be used between 2 integers.
   In this expression, 3.14 is a float.
3. It is not well-typed. The '*.' operator can only be used between 2 floats.
   In this expression, 7 is an integer.
4. It is well-typed. Its type is int and the value is 7
5. It is not well-typed. 5 has type int, whereas "hello" has type string. They should
   have the same type.
6. It is not well-typed. 7 has type int, but the 'else' branch has type unit.
   They should have the same type.
7. It is well-typed. Its type is string and the value is "hello world"
8. It is not well-typed. There shouldn't be a comma in the expression
*)


(* Solution to problem 2
1. It is not legal because y is not defined yet.
2. It is legal. Its type is int and its value is 3
3. It is not legal because when we use 'and', we define x and y seperately. So, x is not
   visible in the expression 'y = x + 1'
4. It is legal. Its type is int and its value is 3
5. It is legal. Its type is int and its value is 5
*)


(* Solution to problem 3 *)
let rec gcd a b =
   if (a = b) then a
   else if (a < b) then gcd a (b - a)
   else gcd (a - b) a


(* Solution to problem 4 *)
let reduced_form (a, b) =
   let gcd_ab = gcd a b in
      if (gcd_ab = 1) then (a, b)
      else (a / gcd_ab, b / gcd_ab)


(* Solution to problem 5 *)
let fromMtoN m n =
   


