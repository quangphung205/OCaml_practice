### Homework 3 Automated Tests

Keep in mind that this feedback does not indicate your grade for the assignment

Run on October 19, 00:20:29 AM.

+  _1_ / _1_ : Pass: Check that file "hw3.ml" exists.

+  _1_ / _1_ : Pass: Check that an OCaml file "hw3.ml" has no syntax or type errors.

    OCaml file "hw3.ml" has no syntax or type errors.



+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   divide_list (fun x -> true) []
   ```
   matches the pattern `([], [])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   divide_list (fun x -> if (String.length x < 4) then true else false) ["this"; "is"; "a"; "list"; "of"; "mixed"; "length"; "strings"]
   ```
   matches the pattern `(["is"; "a"; "of"], ["this"; "list"; "mixed"; "length"; "strings"])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   divide_list (fun x -> if (x mod 2 = 0) then true else false) [1;3;6;8;9;10]
   ```
   matches the pattern `([6;8;10], [1;3;9])`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   treemap (Node (4, Node (2, Empty, Empty), Node (5, Empty,Empty))) (fun x -> x + 3)
   ```
   matches the pattern `Node (7, Node (5, Empty, Empty), Node (8, Empty, Empty))`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   treemap (Node (4, Node (2, Empty, Empty), Node (5, Empty,Empty))) (fun x -> (x mod 2 = 0))
   ```
   matches the pattern `Node (true, Node (true, Empty, Empty), Node (false, Empty, Empty))`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   append [1;2;3] [4;5;6]
   ```
   matches the pattern `[1;2;3;4;5;6]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   append [] []
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   reverse [1;2;3]
   ```
   matches the pattern `[3;2;1]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   reverse []
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   filter (fun x -> (x mod 2 = 0)) [1;2;3;4;5;6]
   ```
   matches the pattern `[2;4;6]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   filter (fun x -> (x mod 2 = 0)) []
   ```
   matches the pattern `[]`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Plus (Id "x", Id "y")) "x"
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Plus (Id "x", Id "y")) "z"
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Let ("x", Id "x", Plus (Id "x", Id "y"))) "x"
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Let ("x", Id "z", Plus (Id "x", Id "y"))) "x"
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Fun ("x", Id "x")) "x"
   ```
   matches the pattern `false`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   freeIn (Fun ("y", Id "x")) "x"
   ```
   matches the pattern `true`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   subst (Plus (Int 5, Id "x")) "x" (Plus (Int 3,Int 4))
   ```
   matches the pattern `Plus (Int 5, Plus (Int 3, Int 4))`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   subst (Times (Int 5, Id "y")) "x" (Plus (Int 3,Int 4))
   ```
   matches the pattern `Times (Int 5, Id "y")`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   subst (Or (Id "x", And (Not (Id "x"), Int 5))) "x" True
   ```
   matches the pattern `Or (True, And (Not True, Int 5))`.

   




+  _1_ / _1_ : Pass: 
Check that the result of evaluating
   ```
   subst (Cond (Or (Lss (Id "x",Int 5), Eq (Id "y", Int 7)), Id "x", Int 10)) "x"  (Plus (Int 3,Int 4))
   ```
   matches the pattern `Cond (Or (Lss (Plus (Int 3, Int 4), Int 5), Eq (Id "y", Int 7)), Plus (Int 3, Int 4), Int 10)`.

   




