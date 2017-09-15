(* OCaml file for lab 02.
   Fix the errors in this file. *)

let five = (2 + 3)

let myfun x = x.[2]

let mult x y = x * y

let two = 2
let plus_two x = x + two

let and3 a b c = a && b && c

let helloworld = "hello" ^ "world"

let three_or_ten x = if x then 3 else 10
let three = three_or_ten (1 < 2)
let ten = three_or_ten (5 < 2)


let nine = mult (three_or_ten true) (three_or_ten (2 < 4))

let mult_or_plus a b = if (three_or_ten (a < 5) = 3) then (2 * b) else (2 + b)
