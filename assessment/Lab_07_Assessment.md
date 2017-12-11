### Assessment for Lab 07

#### Total score: _10_ / _10_

Run on December 11, 12:49:02 PM.

+  _5_ / _5_ : Pass: Check that file "lab07.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "lab07.ml" has no syntax or type errors.

    OCaml file "lab07.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   map2 (fun x y -> x + y) [1;2;3] [4;5;6]
   ```
   matches the pattern `[5; 7; 9]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   List.sort compare (union [1;2;4] [4;5;6])
   ```
   matches the pattern `[1;2;4;5;6]`.

   




+ Pass: 
Check that the result of evaluating
   ```
   unzip [(1,4);(2,5);(3,6)] 
   ```
   matches the pattern `([1;2;3], [4;5;6])`.

   




+ Pass: 
Check that the result of evaluating
   ```
   zip [1;2;3] [4;5;6]
   ```
   matches the pattern `[(1,4);(2,5);(3,6)]`.

   




#### Total score: _10_ / _10_

