(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 6
 * Due Date: 11/27/17
 *)

(* Some functions for displaying colourings *)

(* Printing a list using a function for printing items in the list *)
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

(* A function for displaying an integer item *)
let int_printer i = Printf.printf "%d" i

(* A function for displaying a colour (represented by a string) *)
let show_color c = Printf.printf "%s" c

(* A function for displaying a node, colour pair *)
let show_node_and_color (n,c) =
   Printf.printf "(%d," n; show_color c; Printf.printf ")"

(* A function for showing a (complete) colouring *)
let show_coloring l =
   Printf.printf "\nColoring for the graph: "; printlist show_node_and_color l;
   Printf.printf "\n"


(* Solution to Problem 1 *)
(* Problem 1 Specific Code *)

exception Search_Failure

let ask_user printer config =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y")
   then (raise Search_Failure)
   else ()

let color_graph nodes adjacency colors =
   let rec color_graph_aux nodes colors' colored =
    (* this code should try to extend the colouring
       already present in colored into a colouring
       also for all the nodes in nodes. The general
       scheme:
           1. If no more nodes, then colouring has
              succeeded, interact with the user
           2. Else, pick a color for the first node and
              add it to colored
           3. try to color the remaining nodes in the
              extended colored environment
           4. if coloring is unsuccessful, indicated by
              an exception, handle the exception by
              picking another color for the first node
           5. if the colors for the first node have been
              exhausted, raise an exception to signal failure *)
          let rec check_color (n,color) adjacency colored =
	     let rec check_color_aux (n,color) alist colored =
	        match alist with
		| [] -> false
		| (h::t) -> if (List.mem (h,color) colored) then true
		            else check_color_aux (n,color) t colored
	     in match adjacency with
	        | [] -> false
		| (n',alist)::t -> if (n <> n') then check_color (n,color) t colored
		                   else check_color_aux (n,color) alist colored	  
        in match (nodes,colors') with
	   | ([],_) -> ask_user show_coloring colored
	   | (_,[]) -> raise Search_Failure
	   | ((h1::t1),(h2::t2)) ->  if ((check_color (h1,h2) adjacency colored) = false) then
	      try (color_graph_aux t1 colors ((h1,h2)::colored)) with
	      | Search_Failure -> color_graph_aux nodes t2 colored
                                     else color_graph_aux nodes t2 colored
   in try (color_graph_aux nodes colors []) with
          Search_Failure -> Printf.printf "\nNo (more) colourings possible\n"


(* Solution to Problem 2 *)
(* Problem 2 Code *)

let ask_user_cps printer config succ fail =
   printer config;
   Printf.printf "More solutions (y/n)? ";
   if (read_line () = "y") then (fail ()) else (succ ())

let color_graph_cps nodes adjacency colors =
   let rec color_graph_aux nodes colors' colored succ fail =
    (* this code should try to extend the colouring
       already present in colored into a colouring
       also for all the nodes in nodes. The general
       scheme:
           1. If no more nodes in nodes, the interact
              with user
           2. If there is a node, pick a color for it
           3. try to color the remaining nodes in the
              extended colored environment, passing
              suitable success and failure continuations;
              the latter should resume by picking another
              color for the current node.
           4. if no color is available for current node
              invoke the failure continuation  *)
           (* This is just to make the code compile *)
          let rec check_color (n,color) adjacency colored =
	     let rec check_color_aux (n,color) alist colored =
	        match alist with
		| [] -> false
		| (h::t) -> if (List.mem (h,color) colored) then true
		            else check_color_aux (n,color) t colored
	     in match adjacency with
	        | [] -> false
		| (n',alist)::t -> if (n <> n') then check_color (n,color) t colored
		                   else check_color_aux (n,color) alist colored	  
        in match (nodes,colors') with
	   | ([],_) -> ask_user_cps (show_coloring colored) succ fail
	   | (_,[]) -> fail ()
	   | ((h1::t1),(h2::t2)) ->  if ((check_color (h1,h2) adjacency colored) = false) then
	       (color_graph_aux t1 colors ((h1,h2)::colored) succ
                  (fun () -> color_graph_aux nodes t2 colored succ fail))
                                     else color_graph_aux nodes t2 colored succ fail
   in color_graph_aux nodes colors [] (fun () -> ())
                               (fun () -> Printf.printf "\nNo (more) colourings\n")
