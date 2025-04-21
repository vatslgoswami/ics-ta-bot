(* Lecture 12, Feb 28 *)

(* Map, Filter, and Fold: *)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

(* Map as a list constructor: map f l = { f (x) | x in l} *)

(* Is cart_prod l1 l2 a map?

  Yes! 

  let cart_prod l1 l2 = fold_right (fun x t1 -> concatenate (map (fun y -> (x, y)) l2) t1) l1 []

*)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> 
    match pred x with
    | true -> x :: filter pred xs
    | false -> filter pred xs
;;

(* Filter as a list constructor: filter pred l = { x | x in l and (pred x) holds } *)

let rec fold_right (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: t -> f h (fold_right f v t)
;;

(* Some examples: *)

let add (x: int) (y: int) : int = x + y;;
let sumlist l = fold_right add 0 l;;

(* 
  sumlist [2; 4]
= fold_right add 0 [2; 4]
= add 2 (fold_right add 0 [4])
= add 2 (fold_right add 0 [4])
= add 2 (add 4 fold_right add 0 [])
= add 2 (add 4 0)
= add 2 4
= 6
*)

let unit_map (x:'a) : int = 1;;
let mylength l = fold_right add 0 (map unit_map l) ;;

(* a map can be 'baked' into a filter *)

let myfunc (x: 'a) (n: int) : int = 1 + n ;;
let mylength_new_2 l = fold_right myfunc 0 l ;;

(* 
  mylength_new_2 [3; 4; 1] 
= fold_right myfunc 0 [3; 4; 1]
= myfunc 3 (fold_right myfunc 0 [4; 1])
= myfunc 3 (myfunc 4 (fold_right myfunc 0 [1]))
= myfunc 3 (myfunc 4 (myfunc 1 (fold_right myfunc 0 [])))
= myfunc 3 (myfunc 4 (myfunc 1 0))
= myfunc 3 (myfunc 4 (1 + 0))
= myfunc 3 (1 +  (1 + 0))
=  1 + (1 +  (1 + 0))
*)

(* What is the following? *)
let subtract (x: int) (y: int) : int = (x - y) ;;
let temp = fold_right subtract 0 [4; -2; 1; 0] ;;

(* 

  fold_right subtract 0 [4; 2; 1; 0]
= subtract 4 (fold_right subtract 0 [2; 1; 0])
= subtract 4 (subtract 2 (fold_right subtract 0 [1; 0]))
= subtract 4 (subtract 2 (subtract 1 (fold_right subtract 0 [0])))
= subtract 4 (subtract 2 (subtract 1 (subtract 0 (fold_right subtract 0 []))))
= subtract 4 (subtract 2 (subtract 1 (subtract 0 0)))
= subtract 4 (subtract 2 (subtract 1 (subtract 0 0)))
... 
= 7
*)

(** Factorial Implementations *)

(* Using if-else syntax *)
let rec factorial_if (n: int): int =
  if n = 0 then 1 
  else n * factorial_if (n - 1)
;;
(* Why is this wrong? *)

(* Using pattern matching *)
let rec factorial_match (n: int): int = 
  match n with
  | 0 -> 1
  | n -> n * factorial_match (n - 1)
;;
  
(* Using fold_right *)
let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b
;;

let factorial_fold (n: int): int = fold_right ( * ) 1 (range 1 n) ;;


(* Finding the maximum element in a list *)
let max_elem (l: int list) : int option =
  match l with
  | [] -> None
  | x::xs -> Some (fold_right max x xs)
;;

(* Flattening a list of lists *)

let concatenate l1 l2 = fold_right (fun x s -> x :: s) l1 l2 ;;

let flatten l = fold_right concatenate [] l ;;
let flattened = flatten [[1;2]; [3;4]; [5]] ;;
(* Result: [1;2;3;4;5] *)

(* Left Folding *)
let rec fold_left (f: 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> fold_left f (f v x) xs
;;

(* Is fold_left f v l = fold_right f v l?

No. Consider: fold_right subtract 0 [4; -2; 1; 0] and fold_left subtract 0 [4; -2; 1; 0] *)

(* Zip of Lists *)
(* Zip takes two lists and combines them into a list of pairs. *)

let rec zip (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1 with
  | [] -> []
  | (x:: xs) -> 
    match l2 with 
    | [] -> []
    | (y:: ys) -> (x, y) :: (zip xs ys)
;;

let zipped_list = zip [1;2;3] ["a";"b";"c"] ;;
(* Result: [(1, "a"); (2, "b"); (3, "c")] *)


(* Lambda functions are anonymous functions defined using the `fun` keyword. *)

let square: int -> int = fun x -> x * x ;; 
let l2 = map (fun (x: int): int -> x * x) [2; 4; 6] ;;

let double: int -> int = fun x -> x * 2 ;;
let doubled = map (fun x -> x * 2) [1;2;3] ;;  (* Result: [2;4;6] *)
let evens = filter (fun x -> x mod 2 = 0) [1;2;3;4;5;6] ;; (* Result: [2;4;6] *)

let count_occurrences elem l =
  fold_right (fun x n -> if x = elem then 1 + n else 0 + n) 0 l ;;
  
let count_twos = count_occurrences 2 [1;2;2;3;2;4;5] ;; (* Result: 3 *)

(* Reverse a list using fold_left *)
let reverse (l: 'a list): 'a list = fold_left (fun s x -> x :: s) [] l;;
let reversed = reverse [1; 2; 3; 4; 5];;

(* 
  reverse [1; 2; 3; 4; 5]
= fold_left (fun s x -> x::s) [] [1;2;3;4;5]
= fold_left (fun s x -> x::s) (1: []) [2; 3; 4; 5] 
= fold_left (fun s x -> x::s) (2: 1: []) [3; 4; 5]
= fold_left (fun s x -> x::s) (3: 2: 1: []) [4; 5]
... 

 *)






(* Testing Euclid's Proof of Infinitude of Primes:

Euclid's Proof proceeds as follows. For the sake of contradiction let the set of primes be finite. That is, let the primes be only p1, p2, p3, ... pk. Then consider their product n = p1 * p2 * ... * pk. The number (n+1) is not divisible by any prime pi. Hence n+1 must have a prime factor other than p1, p2, p3, ... pk. This is a contradiction. 

Based on this, let us build a tester called euclid_tester. We will claim that there finitely many primes, and primes are bounded by some large number n. The `euclid_tester n' will find us a prime number greater than n. It proceeds as follows: 

First construct all prime numbers less than n. *)

(* We will work with numbers [2; 3; 4; ... ; n], and skip 1 as 1 is neither prime nor composite *)
let full_range (n: int) : int list = range 2 n ;;

(* Using is_prime from Lecture 11, one can apply a filter on `full_range n'. In this class we will see a different method called the Sieve of Eratosthenes *)

let rec sieve (l: int list) : int list =
  match l with
  | [] -> []
  | x :: xs -> x :: sieve (filter (fun y -> y mod x <> 0) xs)
;;

(* One can rewrite this using a cumbersome fold as: *)
let sieve_fold (l: int list) : int list = fold_right (fun x s -> x :: filter (fun y -> y mod x <> 0) s) l []

let primes_less_than n = sieve_fold (range 2 n) ;;

(* Now one can take the product of all these numbers and add 1. This can be done with a fold. *)
let big_prod_plus_one n = fold_right ( * ) 1 (sieve_fold (range 2 n)) + 1 ;;

(* Or a very very cumbersome: *)
let big_prod_plus_one_ugly n = fold_right ( * ) 1 (fold_right (fun x s -> x :: filter (fun y -> y mod x <> 0) s) (range 2 n) []) + 1 ;;

(* Find its smallest non-trivial divisor. Our code starts at 2, and checks all numbers up to sqrt n: *)
let rec smallest_divisor (n: int) (k: int) : int =
  match k * k > n, n mod k = 0 with
  | true, _ -> n                              (* If k^2 > n, return n *)
  | _, true -> k                              (* If n is divisible by k, return k *)
  | _ -> smallest_divisor n (k + 1)           (* Otherwise, recurse with k + 1 *)
;; 

(* Then using smallest_divisor n k, we have: *)
let euclid_tester (n: int) : int = smallest_divisor (big_prod_plus_one n) 2;;
