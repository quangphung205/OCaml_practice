(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 7
 * Due date: 12/08/17
 *)

module type DATA =
   sig
     type item
     val leq : item * item -> bool
     val print : out_channel -> item -> unit
     val eq : item * item -> bool
   end

(* Solution to Problem 4 - Part 1 *)
module Data : DATA =
   struct
     type item
     let leq ((p:item),(q:item)):bool = p <= q
     let print (printer:out_channel) (x:item) = ()
     let eq ((p:item),(q:item)):bool = p = q
   end


module type BTREE =
   sig
     type item
     type btree
     val insert : item * btree -> btree
     val print : out_channel -> btree -> unit
     val find : btree*item -> bool
     val initTree : unit -> btree
   end

module BTree (Data : DATA) : (BTREE with type item = Data.item) =
   struct
      type item = Data.item
      type color = R | B
      type btree =
          Empty
        | Node of color * item * btree * btree

      let balance t =
        match t with
        | ( Node(B,z,Node(R,x,Node(R,y,a,b),c),d) |
            Node(B,z,Node(R,y,a,Node(R,x,b,c)),d) |
            Node(B,y,a,Node(R,z,Node(R,x,b,c),d)) |
            Node(B,y,a,Node(R,x,b,Node(R,z,c,d))) ) ->
            Node(R,x,Node(B,y,a,b),Node(B,z,c,d))
        |  _ -> t

      let insert ((d:item),(t:btree)) =
        let rec ins t =
           match t with
           | Empty -> Node (R, d, Empty, Empty)
           | Node (c,d',l,r) ->
                 if (d < d') then balance (Node (c,d',ins l, r))
                 else balance (Node (c,d', l,ins r))
        in match (ins t) with
           | Node (_,d,l,r) -> Node (B,d,l,r)
           | Empty -> raise (Invalid_argument "insert")

      let print outfile bt =
         let rec indent n =
            match n with
            | 0 -> ()
            | n -> (Printf.fprintf outfile "  "; indent (n-1)) in
            let rec print_aux n =
              function
             | Empty -> ()
             | Node (_,i,l,r) ->
                (print_aux (n+1) l;
                   indent n; Data.print outfile i; Printf.fprintf outfile "\n";
                      print_aux (n+1) r
                                        )
           in print_aux 0 bt

      let rec find =
        function
       | (Empty,i) -> false
       | (Node(_,i',l,r),i) ->
               if Data.eq(i,i') then true
               else if Data.leq(i,i')
                    then find (l,i)
                    else find (r,i)

      let initTree () = Empty
   end

 (* SCORE (Problem 4.1 -- BTree functor) : 5/5
  *      +5 : for correctly defining the BTree functor
  *)

(* Solution to Problem 4 - Part 2 *)
module IntData : (DATA with type item = int) =
   struct
      type item = int
      let leq ((p:item),(q:item)):bool = p <= q
      let print (printer:out_channel) (x:item) = Printf.fprintf printer "%d" x
      let eq ((p:item),(q:item)):bool = p = q
   end

module IntBTree = BTree(IntData)

(* Solution to Problem 4 - Part 3 *)
module StringData : (DATA with type item = string) =
   struct
      type item = string
      let leq ((p:item),(q:item)):bool = p <= q
      let print (printer:out_channel) (x:item) = Printf.fprintf printer "\"%s\"" x
      let eq ((p:item),(q:item)):bool = p = q
   end

module StringBTree = BTree(StringData)

(* Problem 4.2 and 4.3 Score:
 * 4.2: 5/5
 * 4.3: 5/5
 *)

let inttree =
        IntBTree.insert (1,
         (IntBTree.insert (2,
            (IntBTree.insert (3,
               (IntBTree.insert (4,
                  (IntBTree.insert (5,
                      (IntBTree.insert (6,
                         IntBTree.insert (7,
                           IntBTree.insert (8,
                             IntBTree.initTree ())))))))))))));;

let strtree =
        StringBTree.insert ("a",
         (StringBTree.insert ("b",
            (StringBTree.insert ("c",
               (StringBTree.insert ("d",
                  (StringBTree.insert ("e",
                      (StringBTree.insert ("f",
                         (StringBTree.insert ("g",
                            (StringBTree.insert ("h",
                               StringBTree.initTree ())))))))))))))));;
