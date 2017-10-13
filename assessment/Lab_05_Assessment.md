### Assessment for Lab 05

#### Total score: _10_ / _10_

Run on October 12, 21:30:33 PM.

+  _5_ / _5_ : Pass: Check that file "lab05.ml" exists.

+  _5_ / _5_ : Pass: Check that an OCaml file "lab05.ml" has no syntax or type errors.

    OCaml file "lab05.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   isSearchTree (Node (5,Node (3,
            Node (1,Empty,Empty),
            Node (4,Empty,Empty)),
            Node (8,Node (7,Node (6,Empty,Empty), Empty),
            Node (9,Empty,Node(10,Empty, Empty)))))
   ```
   matches the pattern `true`.

   




+ Pass: 
Check that the result of evaluating
   ```
   typeof  (Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)), Int' 5, Int' 7))
   ```
   matches the pattern `Some IntTy`.

   




+ Pass: 
Check that the result of evaluating
   ```
   wellTyped (Cond' (Lss' (Int' 10, Plus' (Int' 5, Int' 7)), Int' 5, Int' 7))
   ```
   matches the pattern `true`.

   




#### Total score: _10_ / _10_

