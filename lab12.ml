(* Name: Quang Phung
 * Course: CSCI 2041
 * Lab 12
 * Due Date: 11/24/17
 *)

                 (* Copyright (c) Gopalan Nadathur *)

(* Problem 1 *)

exception Search_Failure
exception No_More_Solution
  
(* ask_user : ('a -> unit) -> 'a - unit
   The first argument is intended to be a 
   printer for the second. The function uses
   the printer to display the second argument
   and then succeeds or fails based on user input *)
let ask_user printer config =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y")
   then (raise Search_Failure)
   else ()

(* Intended type:
      printlist : ('a -> unit) -> ('a list) -> unit
   A function for displaying (on the terminal)
   a list of items, given as the second argument, 
   using the first argument that is a printer for such
   items *)
let printlist item_printer l =
   let rec printlist_aux l =
     match l with
     | [] -> Printf.printf "%c" ']'
     | (h::t) -> Printf.printf "%s" ", ";
                 item_printer h;
                 printlist_aux t
   in (Printf.printf "%c" '[';
       match l with
       | (h::t) -> item_printer h; printlist_aux t
       | _ -> printlist_aux l)

(* int_printer : int -> unit
   A printer for integer values *)
let int_printer i = Printf.printf "%d" i

(* show_solution : int -> (int list) -> unit
   A function for printing a solution to the 
   task of finding a list of numbers that add up
   to a given number. The first argument is the number
   and the second argument is the solution to be 
   printed. *)
let show_solution s =
   fun l ->
      Printf.printf "A list that sums to %d: " s;
      printlist int_printer l;
      Printf.printf "\n"
      
(* sumlist : int list -> int
   A function for summing up the items in an integer
   list. *)
let sumlist l =
   let rec sumlist_aux l acc =
     match l with
     | [] -> acc
     | (h::t) -> sumlist_aux t (h + acc)
  in sumlist_aux l 0

(* Problem 2 
   Many of the functions defined for Problem 1 above should find
   reuse in solving this problem *)
 
 (* ask_user_cont : ('a -> unit) -> 'a -> (unit -> 'b) -> (unit -> 'b) -> 'b
    a function for displaying the second argument using the 
    first argument (which is a printer for the second argument) and then
    interacting with the user to determine whethere to continue using 
    the third or the fourth argument. *)
let ask_user_cont printer config succ fail =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y") then (fail ()) else (succ ())


(* Solution to Problem 1 *)
(* find_list: int list list -> int -> unit
 * print a list whose sum is equal to an input number
 * then ask users choose to accept the solution or not
 * if yes, then print another solution and ask users again
 * if no, then return a unit type and quit a program
 *)
let rec find_list l n =
  match l with
  | [] -> raise No_More_Solution
  | (h::t) -> if (sumlist h) = n then 
      try (ask_user (show_solution n) h) with
      | Search_Failure -> find_list t n
    else try find_list t n with
    | No_More_Solution -> Printf.printf "No more solution.\n"
            

(* Solution to Problem 2 *)
(* find_list_cont: int list list -> int -> (unit -> 'a) -> (unit -> 'a) -> 'a
 * This function has the same functionality with the find_list
 * use two continuations instead of exceptions
 *)
let rec find_list_cont l n succ fail =
  match l with
  | [] -> fail ()
  | (h::t) -> if (sumlist h) = n then
      ask_user_cont (show_solution n) h succ (fun () -> find_list_cont t n succ fail)
	else find_list_cont t n succ fail
