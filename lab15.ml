(* Name: Quang Phung
 * Course: CSCI 2041
 * Lab 15
 * Due Date: 12/13/17
 *)

type color = R | B

type 'a rbtree =
    Empty
| Node of color * 'a * 'a rbtree * 'a rbtree

(* Solution to Problem 1
 * is_RBTree_aux : 'a rbtree -> int * bool
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return true and the height of the tree
              otherwise, return false
 *)
let rec is_RBTree_aux t =
   let check_color t =
      match t with
      | Empty -> true
      | Node (c, _, l, r) -> 
         (match (l,r) with
          | (Empty, Empty) -> true
          | (Empty, Node (c', _, _, _)) | (Node (c', _, _, _), Empty) -> if (c' = B) then true else false
          | (Node (c1, _, _, _), Node (c2, _, _, _)) -> if ((c1 = B) && (c2 = B)) then true else false
         )
   in let rec is_RBTree_helper t n =
         match t with
         | Empty -> (n, true)
         | Node (c, _, l, r) as node ->
             (match c with
              | B -> let (nl, bl) = is_RBTree_helper l (n+1)
                     in let (nr, br) = is_RBTree_helper r (n+1)
                        in if (nl = nr) then (nl, true) else (nl, false)
              | R -> if (check_color node) then
                         let (nl, bl) = is_RBTree_helper l n
                         in let (nr, br) = is_RBTree_helper r n
                            in if (nl = nr) then (nl, true) else (nl, false)
                     else (n, false)
             )
       in is_RBTree_helper t 0

(* Solution to Problem 2
 * is_RBTree : 'a rbtree -> bool
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return true,
              otherwise, return false
 *)
let is_RBTree t = let (_, b) = is_RBTree_aux t in b

(* Solution to Problem 3
 * bh_RBTree : 'a rbtree -> int option
 * precondition: a red-black tree
 * invariant: if it satisfies all conditions, return the height of the tree in option type
              otherwise, return None
 *)

let bh_RBTree t = let (n, b) = is_RBTree_aux t in
                    if (b = true) then Some n else None

(* End of lab15.ml *)
