(* Name: Quang Phung
 * Course: CSCI 2041
 * Lab08
 * Due date: 10/27/17 *)

(* Solution to Problem 1 *)
(* P1_1.
 * processWord: (string * int) list -> string -> (string * int) list
 * precondition: input is a (word,count) list and a string
 * invariant: add a new string to a list and update its count *)
let rec processWord wcl (s: string) =
  match wcl with
  | [] -> [(s,1)]
  | (w,c)::t -> if (s = w) then (w, c + 1)::t
                else if (s < w) then (s,1)::wcl
                     else (w,c)::(processWord t s)

(* P1_2.
 * assimilateWordCount: (string * int) list -> string * int -> (string * int) list
 * precondition: input is a (word,count) list and a (word,count) pair
 * invariant: add a new (word,count) pair to a list and update its count *)
let rec assimilateWordCount wcl ((word: string), count) =
  match wcl with
  | [] -> [(word, count)]
  | (w,c)::t -> if (word = w) then (w, c + count)::t
                else if (word < w) then (word, count)::wcl
                else (w,c)::(assimilateWordCount t (word, count))

let processOneList l = List.fold_left processWord [] l
let assimilateWCList wcs wcl =
        List.fold_left assimilateWordCount wcs wcl
let wordCounts ls =
      List.fold_left assimilateWCList [] (List.map processOneList ls)


(* Solution to Problem 2 *)
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree
type 'a bstree = { data : 'a btree ; lss : 'a -> 'a -> bool }

(* insert: 'a -> 'a bstree -> 'a bstree
 * precondition: input is a tree and a new node
 * invariant: add a new node to the current tree *)
let rec insert x t =
  let rec insert_helper y btree lt =
    match btree with
    | Empty -> Node (y, Empty, Empty)
    | Node (y', l, r) -> if (lt y y') then Node (y', insert_helper y l lt, r)
	                 else Node (y', l, insert_helper y r lt)
  in { data = (insert_helper x t.data t.lss); lss = t.lss }


(* Solution to Problem 3 *)
(* cont_fact: int -> (int -> 'a) -> 'a
 * precondition: input is an integer and a continuation function
 * invariant: output is a factorial of an input *)
let rec cont_fact n c =
  match n with
  | 0 -> (c 1)
  | m -> cont_fact (m - 1) (fun x -> c (m * x))

(* fact is not a tail recursive function but has the correct behavior of calculating the factorial -> n! = 1 x 2 x 3 x ... x n
 * cont_fact is a tail recursive function and has the same behavior as fact -> n! = 1 x 2 x 3 x ... x n
 * tr_fact is a tail recursive funtion but does not have the correct behavior 
 * of calculating the factorial -> n! = n x (n-1) x (n-2) x ... 1 *)
(* End of lab08.ml *)
