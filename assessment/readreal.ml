(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 6
 * Due Date: 11/27/17
 *)

(* Homework 6, CSci 2041, Fall 2017
   This file provides code to be used in building a function for reading
   floating point numbers as described in the homework write-up.
   The task is to fill in the definitions of the functions getWhole and
   getFrac and to use these in defining the function getFloat, the real
   objective in this problem *)

(* a reference identifier that provides a lookahead on an input channel *)
let ch = ref ' '

(* moving the next character from an input channel into the lookahead
   getChar : in_channel -> unit *)
let getChar infile = (ch := input_char infile)

(* looking up the next character on the input channel
   lookupChar : unit -> char *)
let lookupChar () = !ch

(* A useful function for skipping over blank spaces in the input channel
   skipSpace : in_channel -> unit *)
let rec skipSpace infile =
  if (lookupChar () = ' ') then (getChar infile; skipSpace infile)
  else ()

(* checking if a given character is a digit
    isDigit : char -> bool *)
let isDigit ch = (ch >= '0' && ch <= '9')

(* Solution to Problem 1
    getWhole : in_channel -> int *)
(* A function for reading a sequence of digits from an input channel
   and interpreting them as a whole number
   Precondition: the "cursor" on the input channel is over the first digit
   getWhole : in_channel -> int *)
let getWhole infile =
   let rec getnum num =
      if (isDigit (lookupChar ())) then let temp = num * 10 + (int_of_char !ch - int_of_char '0')
                                        in (getChar infile; getnum temp)
      else num
      in getnum 0

(* SCORE (Problem 2.1 -- getWhole):   4/4
*     2/2 : read entire number up to decimal
*     2/2 : stopped cursor at decimal
*)

(* Solution to Problem 2
    getFrac : in_channel -> float *)
(* A function for reading a sequence of digits from an input channel
   and interpreting them as the fractional part of a floating point number
   Precondition: the "cursor" on the input channel is over the first digit
   getFrac : in_channel -> float *)
let rec getFrac infile =
   let rec getFrac' frac exp=
      if (isDigit (lookupChar ())) then let temp = frac +. (float (int_of_char !ch - int_of_char '0')) /. exp
                                        in (getChar infile; getFrac' temp (exp *. 10.))
      else frac
   in getFrac' 0.0 10.

(* Problem 2.2 Score:
  2/2:
  2/2:
*)

(* Solution to Problem 3
    getFloat : in_channel -> float option *)
(* A function for reading in a floating point number, in the format
   described in the homework writeup, and returning it using the
   option type. The function should return None if the next
   token in the input is not a floating point number
    getFloat : in_channel -> float option *)
(* 2.3:
  2/2
  2/2
*)

let getFloat infile =
  (* Here is roughly the logic that needs to be implemented
      1. skip over the initial blank characters
      2. if the first non-blank is a digit, get the
         whole number part using the function getWhole
      3. check that the next character is a decimal point
      4. if the next character is a digit, get the fractional
         part using getFrac
      5. Combine the whole number and the fractional part to
         get the floating point number n and return (Some n)
      6. If the decimal point or both the whole number and
      fractional parts are missing, then return None *)
   let _ = skipSpace infile
   in if (!ch = '.') then (getChar infile; if (isDigit !ch) then Some (getFrac infile) else None)
      else if (isDigit !ch) then let result = ref 0.0
      in (result := !result +. float (getWhole infile);
	 if (!ch = '.') then (getChar infile; if (isDigit !ch) then (Some (!result +. getFrac infile))
	    else (Some !result))
	        else None)
		    else None

(* A function to test getWhole *)
let test_getWhole () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %d\n" (getWhole stdin)
   else Printf.printf "Bad input\n")

(* A function to test getFrac *)
let test_getFrac () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   skipSpace stdin;
   if (isDigit (lookupChar ()))
   then Printf.printf "Your input: %f\n" (getFrac stdin)
   else Printf.printf "Bad input\n")

(* a function to test getFloat *)
let test_getFloat () =
  (Printf.printf "Enter a number: "; flush stdout;
   ch := ' ';
   match (getFloat stdin) with
   | None -> Printf.printf "Bad input\n"
   | (Some r) -> Printf.printf "Your input: %f\n" r)
