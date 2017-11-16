(* Name: Quang Phung
 * Course: CSCI 2041 
 * Lab 11
 * Due Date: 11/17/17
 *)

(* Solution to Problem 2 *)
type 'a mylist = 'a listcell ref
and  'a listcell = Nil | Cons of 'a * ('a mylist) ref

(* reverse: 'a mylist -> 'a mylist -> 'a mylist
 * precondition: input is a list of type 'a mylist and a Nil list
 * invariant: output is an input list in reverse order
 *)
let rec reverse (l: 'a mylist) acc =
   match !l with
   | Nil -> acc
   | Cons (i, tl) -> let nextl = !tl in (tl := acc; reverse nextl l)


(* Solution to Problem 3 *)
(* a function for reading a number from the input channel infile *)
let readnum infile =
  let rec skip_space () =
    let ch = input_char infile
    in if (ch = ' ') then skip_space ()
       else ch in
  let is_digit ch = (ch >= '0' && ch <= '9') in
  let rec getnum num =
      let ch = skip_space ()
      in if (is_digit ch)
         then getnum (num * 10 + (int_of_char ch - int_of_char '0'))
         else num
   in let ch = skip_space ()
      in if (is_digit ch)
         then Some (getnum (int_of_char ch - int_of_char '0'))
         else None

(* a function to test readnum *)
let get_num_from_user () =
  (Printf.printf "Enter a number: "; flush stdout;
   match (readnum stdin) with
   | None -> Printf.printf "Bad input\n"
   | (Some n) -> Printf.printf "Your input: %d\n" n)				     
(* End of lab11.ml *)
