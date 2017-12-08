(* Name: Quang Phung
 * Course: CSCI 2041
 * Homework 7
 * Due Date: 12/08/17
 *)

(* Solution to Problem 1 *)
(* Part 1: call-by-name
   take (map sqr [1;2;3]) 2
-> take ((sqr 1) :: (map sqr [2;3])) 2
-> take (1 :: (map sqr [2;3])) 2
-> 1 :: (take (map sqr [2;3]) 1)
-> 1 :: (take ((sqr 2) :: (map sqr [3])) 1)
-> 1 :: (take (4 :: (map sqr [3])) 1)
-> 1 :: 4 :: (take (map sqr [3]) 0)
-> 1 :: 4 :: []
 *)

(* Part 2: call-by-value
   take (map sqr [1;2;3]) 2
-> take ((sqr 1) :: (map sqr [2;3])) 2
-> take (1 :: (map sqr [2;3])) 2
-> take (1 :: (sqr 2) :: (map sqr [3])) 2
-> take (1 :: 4 :: (map sqr [3])) 2
-> take (1 :: 4 :: (sqr 3) :: (map sqr [])) 2
-> take (1 :: 4 :: 9 :: (map sqr [])) 2
-> take (1 :: 4 :: 9 :: []) 2
-> 1 :: (take (4 :: 9 :: []) 1)
-> 1 :: 4 :: (take (9 :: []) 0)
-> 1 :: 4 :: []
 *)


type 'a stream = Stream of (unit -> 'a * 'a stream)
let mkStream f = Stream f
let nextStream (Stream f) = f ()
let rec fromNStream n = mkStream (fun () -> (n, fromNStream (n+1)))
let natStream = (fromNStream 1)

(* Solution to Problem 2 - Part 1
 * mapStream : ('a -> 'b) -> 'a stream -> 'b stream
 * precondition: input is a mapping function and a stream
 * invariant: output is a new stream after applying a function to the old stream
 *)
let rec mapStream f s =
   let (x, s') = nextStream s
   in mkStream (fun () -> (f x, mapStream f s'))


(* Solution to Problem 2 - Part 2
 * squareStream : int stream
 * cubeStream : int stream
 *)
let squareStream = mapStream (fun x -> x * x) natStream
let cubeStream = mapStream (fun x -> x * x * x) natStream

(* Solution to Problem 2 - Part 3
 * squarecubeStream : int stream
 *)
let rec makeSquareCubeStream s c =
   let (x1, s') = nextStream s
   in let (x2, c') = nextStream c
      in if (x1 = x2) then mkStream (fun () -> (x1, makeSquareCubeStream s' c'))
         else if (x1 < x2) then makeSquareCubeStream s' c
              else makeSquareCubeStream s c'

let squarecubeStream = makeSquareCubeStream squareStream cubeStream


type 'a stream' = Stream' of 'a stream_aux ref
and  'a stream_aux =
       | Evald of ('a * 'a stream')
       | UnEvald of (unit -> 'a * 'a stream')
(* Solution to Problem 3 - Part 1 *)
(* mkStream' : (unit -> 'a * 'a stream') -> 'a stream'
 * create a type Stream' of a function f
 *)
let mkStream' f = Stream' (ref (UnEvald f))

(* nextStream' : 'a stream' -> 'a * 'a stream'
 * get the next stream of f
 *)
let nextStream' (Stream' f) =
   match !f with
   | Evald e -> e
   | UnEvald u -> let value = u () in (f := Evald value; value)

(* Solution to Problem 3 - Part 2 *)
let rec fromNStream' n = mkStream' (fun () -> (n, fromNStream' (n+1)))
let natStream' = (fromNStream' 1)





























