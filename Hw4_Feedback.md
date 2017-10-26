### Homework 04 Feedback Results

This feedback is generated to assist you in fulfilling the basic requirements (naming, etc) of the assignment. 
            **This file is not your final grade and will not necessarily be used to assign your grade.** 
            This feedback is *not* an exhaustive test suite. You are responsible for ensuring that the specification in the
            written description is satisfied by your code, which will require additional test cases that you create.

Run on October 26, 14:20:43 PM.

+ Pass: Check that file "hw4.ml" exists.

+ Pass: Check that an OCaml file "hw4.ml" has no syntax or type errors.

    OCaml file "hw4.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   makePairLists "a" ["b"]
   ```
   matches the pattern `
            [(("a", "b"), ["b"])]
            `.

   




+ Fail: 
Check that the result of evaluating
   ```
   makeAllPairLists [("a", ["b"])]
   ```
   matches the pattern `
            [[(("a", "b"), ["b"])]]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
 ;;
[24mError: This expression has type 'a list
       but an expression was expected of type
         ('b -> 'c) -> 'b list -> 'c list

   ```



+ Fail: 
Check that the result of evaluating
   ```
   addOnePair (("a", "b"), ["b"]) []
   ```
   matches the pattern `
             [(("a", "b"), ["b"])] 
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value addOnePair
`


+ Fail: 
Check that the result of evaluating
   ```
    makeAllPairLists [("a", ["b"])]
   ```
   matches the pattern `
             [[(("a", "b"), ["b"])]]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
 ;;
[24mError: This expression has type 'a list
       but an expression was expected of type
         ('b -> 'c) -> 'b list -> 'c list

   ```



+ Fail: 
Check that the result of evaluating
   ```
   commonFriends [("a", ["b"]); ("b", ["a"])]
   ```
   matches the pattern `
             [(("a", "b"), [])]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value commonFriends
`


+ Fail: 
Check that the result of evaluating
   ```
   olistToList (insertOList 15 (insertOList 5 (initOList (<))))
   ```
   matches the pattern `
            [5; 15]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value olistToList
`


+ Fail: 
Check that the result of evaluating
   ```
   olistToList (insertOList 15 (insertOList 10 (insertOList 1 (insertOList 5 (initOList (<))))))
   ```
   matches the pattern `
            [1; 5; 10; 15]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value olistToList
`


+ Fail: 
Check that the result of evaluating
   ```
   isOrderedList list1
   ```
   matches the pattern `
            true
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value isOrderedList
`


+ Fail: 
Check that the result of evaluating
   ```
   isOrderedList list3
   ```
   matches the pattern `
            false
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value isOrderedList
`


+ Fail: 
Check that the result of evaluating
   ```
   cont_append [1;2;3] [4;5;6] (fun x -> x)
   ```
   matches the pattern `
                [1; 2; 3; 4; 5; 6]
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound value cont_append
`


+ Fail: 
Check that the result of evaluating
   ```
   let tr = 
              Node (5, 
                Node (10, 
                    Node (15, Empty, Empty), 
                    Empty), 
                Node (30, Empty, Empty))
            in cont_sumTree tr (fun x -> x)
   ```
   matches the pattern `
                60
            `.

   


   Your solution evaluated incorrectly and produced some part of the following:

 ` ;;
[24mError: Unbound constructor Node
Hint: Did you mean None?
`

