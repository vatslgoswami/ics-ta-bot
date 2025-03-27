(* Lecture 11, Feb 25 *)

(* In the previous class, we defined lists. A custom type declaration for list can be: *)
type 'a mylist = Nil | Cons of 'a * 'a mylist ;;

(* Here, 'a denotes an arbitary type. That is, the list will have elements of type 'a. *)
(* We can instantiate it with say int, to get something like this: *)

let sample_mylist: int mylist = Cons(1, Cons(2, Nil)) ;;

(* Observe that this parallels Peano axioms. *)

type nat = Zero | Succ of nat ;;

(* One can map the Nil constructor to Zero and Cons to Succ, giving us the following function: *)

let rec foo (l: 'a mylist) : nat = 
  match l with
  | Nil -> Zero
  | Cons (h, tail) -> Succ (foo tail)
;;

(* Observe closely. The function foo computes the length of the list. *)

(* For ease of notation, we will use native lists instead of explicitely spelling out the type constructors Nil and Cons. That is, instead of sample_mylist, we will have lists of the format: *)

let sample_list: int list = [1; 2] ;;

(* Convert a native list to our custom list *)
let rec of_list (l : 'a list) : 'a mylist =
  match l with
  | [] -> Nil
  | h :: tail -> Cons (h, of_list tail)     (* Form: head :: tail *)
;;

(* Convert from our custom list to a native list *)
let rec to_list (l : 'a mylist): 'a list =
  match l with
  | Nil -> []
  | Cons (h, tail) -> h :: to_list tail
;;

(* We can join, that is concatenate two lists as follows: *)

let rec concatenate (l1: 'a list) (l2: 'a list) : 'a list =
  match l1 with
  | [] -> l2
  | x :: xs -> x :: concatenate xs l2
;;    

(* For ease of notation, you may also use the infix operator (@). That is
   concatenate l1 l2 = l1 @ l2 *)

(* Here are two ways to reverse a list: *)
let rec reverse (l:'a list) : 'a list = 
  match l with
  | [] -> []
  | h::t -> (reverse t) @ [h]
;;

(* Tail-recursive reverse *)
let rec reverse1 (l : 'a list) (acc : 'a list) : 'a list =
  match l with
  | [] -> acc
  | h :: t -> reverse1 t (h :: acc)
;;

let reverse_tail (l : 'a list) : 'a list =
  reverse1 l [];;

(* To understand tail-recursive reverse, first let us look at an execution of reverse1: 

    reverse1 [1; 2; 3; 4] [5; 6]
  = reverse1 1::[2; 3; 4] [5; 6]
  = reverse1 [2; 3; 4] 1::[5; 6]
  = reverse1 [2; 3; 4] [1; 5; 6]
  = ...

  Now let us look at reverse_tail: 

    reverse_tail [1; 2; 3; 4] []
  = reverse1 [1; 2; 3; 4] []
  = reverse1 1::[2; 3; 4] []
  = reverse1 [2; 3; 4] 1::[]
  = reverse1 [2; 3; 4] [1]
  = reverse1 [3; 4] [2; 1]
  = reverse1 [4] [3; 2; 1]
  = reverse1 [] [4; 3; 2; 1]
  = [4; 3; 2; 1]
*)

(* Find minimum *)
exception EmptyListUnexpected;;

let rec findmin (l: int list) : int option =
  match l with
  | [] -> None
  | [h] -> Some h
  | h :: t -> 
      match findmin t with
      | None -> raise EmptyListUnexpected                  (* This case would never be matched. *)
      | Some min_t -> Some (
        match h with
        | _ when h < min_t -> h 
        | _ -> min_t)
;;

(* One can also just simplify this code by removing line 101 and using an if statement: *)

let rec findmin_simple (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match findmin_simple t with
      | None -> Some h
      | Some min_t -> Some (if h < min_t then h else min_t)
;;

(* Let us consider a case where we have a general comparison operation (not just minimum). This can be for example maximum, or the minimum with respect to a function (say f(x) = x^2 - 4x + 2). Let us write the code for it *)

let eval_poly (x: int) : int = (x*x) - (4*x) + 2 ;;

let rec findopt_poly (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match findopt_poly t with
      | None -> Some h
      | Some min_t -> Some (if (eval_poly h) < (eval_poly min_t) then h else min_t)
;;

(* This function parallels find_min_simple. Instead of writing a function every time, it would be cool to get a general function: 

findopt (comparison : a' -> a' -> bool) (l: 'a list) : 'a = ...

For a fixed comparison function, we could write: 

*)

let compare_poly (x: int) (y: int) : bool = (eval_poly x) < (eval_poly y) ;; 

let rec findopt_gen (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match findopt_gen t with
      | None -> Some h
      | Some min_t -> Some (if (compare_poly h min_t) then h else min_t)
;;

(* Can we pass compare_poly as a parameter to findopt_gen? Yes! Using higher order functions. *)

(* Higher Order Functions *)
(* A higher order function is one that takes another function as an input. *)
(* Consider this almost useless function apply, that takes in a function f and applies it to an argument x. That is 
      apply f x = f x ;; *)

let apply (f: 'a -> 'b) (x: 'a) : 'b  = f x ;; 

(* This is useless as putting x next to f automatically applies f to x. However, this gives us an example of a higher order function. *)


(* Similar to this, we can now define a findopt as: *)

let rec findopt (compare: int -> int -> bool) (l: int list) : int option =
  match l with
  | [] -> None
  | h :: t -> 
      match (findopt compare t) with
      | None -> Some h
      | Some min_t -> Some (if (compare h min_t) then h else min_t)
;;


(* Another use case is a way to apply a Function to a list, for example, we want to map such that: 
    map f [x0; x1; ...; xk] -> [(f x0); (f x1); ...; (f xk)]

    Concretely:
    
    map ((+) 3) [2;6;8] = [5;9;11])
    map (( * ) 2) [2;6;8] = [4;12;16])
    
    map (+) [2;6;8]
    
    As this evaluates to:

   map (+) [2;6;8] = [(+) 2; (+) 6; (+) 8];;

  As: 
  (+) : int -> int -> int
  2 : int
  (+) 2 : int -> int

  This is from what we have seen in currying and evaluation of partial functions.  
  
  What is the type of map?
  let rec map (f: 'a -> 'b) (l: 'a list) : 'b list = 
  ... 

  That is:
  map: ('a -> 'b) -> 'a list -> 'b list
*)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

(* Let us consider another operation on lists: *)

let rec even_only (l: int list) : int list =
  match l with
  | [] -> []
  | x :: xs -> 
      match x mod 2 with
      | 0 -> x :: even_only xs
      | _ -> even_only xs
;;

assert(even_only [3; 5; 7] = []);;

(* What about a higher order function for such processes?

This corresponds to filtering a list. A filter selects all items from list l that satisfy property p. 

Say for example, filter (is_even) l = even_only l 

We can implement filter as: *)

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> 
    match pred x with
    | true -> x :: filter pred xs
    | false -> filter pred xs
;;

(* Now let us use filters! *)

(* Find all divisors of a number *)
(* divisors 12 = [1; 2; 3; 4; 6; 12] *)

let is_divisor (n: int) (x: int) : bool = (x mod n = 0);;

(* range a b = [a; ...; b]*)
let rec range (a: int) (b: int) : int list =
  match a > b with
  | true -> []
  | false -> a :: range (a + 1) b
;;

(* 
  range 1 4
= 1 :: (range (1 + 1) 4)
= 1 :: range 2 4
= 1 :: 2 :: range 3 4
= 1 :: 2 :: 3 :: range 4 4
= 1 :: 2 :: 3 :: 4 :: range 5 4
= 1 :: 2 :: 3 :: 4 :: []
= [1; 2; 3; 4]
*)

(* List of divisors = all numbers between 1 and n, that divide n *)
(* List of divisors = range 1 n, then filter with (is divisor n) *)

let divisors (n: int) : int list = filter (is_divisor n) (range 1 n) ;;

(* Observe that this is basically constructing the set: 
   { x| x in [1; ...; n] and (is_divisor n x) }
*)

(* Can we use it for primality testing? *)
(* is_prime: int -> bool *)
(* Primes below n *)
let is_prime (x: int) : bool = (divisors x = [1; x]) ;;

(* 
   Note that lists are not sets. That is: 
   [2; 1] <> [1; 2]
   [2; 2; 1] <> [2; 1]
 *)

(* Generate all primes less than a number n *)
(* All primes in [1; ...; n] = range 1 n *)
let primes (n: int) : int list = filter is_prime (range 1 n);;

(* That is, filter can be composed with interesting functions to process lists *)

(* Let us consider the problem of finding all find all pythagorean triples under 20

We first write it down mathematically: 

  { (a, b, c) | a in [1; ...; 20], b in [1; ...; 20], c in [1; ...; 20] and a^2 + b^2 = c^2 }

  To construct this, we need: 

  all_triples_under_20 = [(1,1,1), (1, 1, 2), ... (20, 20, 20)]

  and 

  is_pythagorean (a, b, c) = (a^2 + b^2 = c^2)

  Then the solution is: filter is_pythagorean all_triples_under_20

  And we are done!
*)

(* How do you construct all_triples? *)

(* Here is a first try: *)

let rec cart_prod_first_try (l1: 'a list) (l2: 'b list): ('a * 'b) list = 
  match (l1, l2) with 
  | ([], _) -> []
  | (_, []) -> []
  | (x:: xs, y:: ys) -> (x, y)::(cart_prod_first_try xs ys)
;;

(* Consider its run:

  cart_prod [1; 2] [3; 4; 5]
= (1, 3):: cart_prod [2] [4; 5]
= (1, 3):: (2, 4) :: cart_prod [] [5]
= (1, 3):: (2, 4) :: []
= [(1, 3); (2, 4)]

Instead, we wanted [(1, 3); (1, 4); (1; 5); (2, 3); (2, 4); (2, 5)]

For this, the recursive case should be something like: 
  (pair up x with every thing in l2) :: (cart_prod xs l2) 
  
  That is (1 times [3; 4; 5]) :: cart_prod [2] [3; 4; 5] 
*)

let rec multiply_list (a: 'a) (l: 'b list) : ('a * 'b) list = 
  match l with 
  | [] -> [] 
  | x :: xs -> (a, x) :: multiply_list a xs
;;



(* multiply_list 1 [3; 4; 5]
= (1, 3) :: multiply_list 1 [4; 5]
= (1, 3) :: (1, 4) :: multiply_list 1 [5]
= (1, 3) :: (1, 4) :: (1, 5) :: multiply_list 1 []
= (1, 3) :: (1, 4) :: (1, 5) :: []
= [(1, 3); (1, 4); (1, 5)]
*)

let rec cartesian_product (l1: 'a list) (l2: 'b list) : ('a * 'b) list =
  match l1 with
  | [] -> []
  | x :: xs -> concatenate (multiply_list x l2) (cartesian_product xs l2)
;;

let range20 : int list = range 1 20;;

let all_triples : ((int * int) * int) list = cartesian_product (cartesian_product range20 range20) range20;;

(* What is the type of all_triples? 
   It could be: 
   1) int list * int list * int list
   2) (int * int * int) list
   3) Something else 
    

  It happens to be something else. Try printing it and checking!

  Notice that: 
  ([], [1; 3; 4], [2; 19]) : int list * int list * int list

  And:
  [(1, 2, 3); (1, 4, 6)]: (int * int * int) list

  But as tuples aren't natively associative, we have: 

  all_triples = [((1, 1), 1); ... ]

  That is all_triples: ((int * int) * int) list

  Let us fix that! *)


exception WhatTheHellException ;;

let bar (p: (int * int) * int) : (int * int * int) = 
  match p with
  | ((x, y), z) -> (x, y, z)
  | _ -> raise WhatTheHellException         (* Not really needed though *)
;;

(* Easier: let bar ((x, y), z) = (x, y, z) *)

(* All well formed triples: *)
let awf_triples = map bar all_triples

let is_pythagorean (p: int * int * int) : bool =
  match p with
  | (x, y, z) when (x * x + y * y = z * z) -> true
  | _ -> false
;;

let all_pythagorean_triples = filter is_pythagorean awf_triples;;



(* Combining map and filter *)

let sqr (a: int) : int = a * a;;
let is_even (x: int) : bool = x mod 2 = 0;;
let sqr_even l = map sqr (filter is_even l);;

(* This is the same as:

    S = { a^2 | a in l, a is even}
    S = { sqr(a) | a in l, is_even(a)}

In general, most set builder notation can be converted to a list construction using the appropriate map and filter.

*)

(* Combining Elements *)

let rec sumlist (l: int list) : int =
  match l with
  | [] -> 0
  | x :: xs -> x + sumlist xs
;;

let rec multlist (l: int list) : int =
  match l with
  | [] -> 1
  | x :: xs -> x * multlist xs
;;

(* What is the common pattern? 


let rec combinelist (l: int list) : int =
  match l with
  | [] -> default_value
  | h :: tail -> combine h (combinelist tail)


  Same as before, we turn it into a higher order function: 

let rec combine func value list =
  match list with
  | [] -> value
  | h :: tail -> func h (combine func value tail)
;;

What is its type?

f : 'a -> 'b -> 'b
v : 'b
l : 'a list

This is built in as List.fold_right
Same way, map and filter are available as List.map and List.filter

*)

let rec foldr (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: tail -> f h (foldr f v tail)
;;

let add (x: int) (y: int) : int = x + y;;

let sumlist l = foldr add 0 l;;

(* 

  sumlist [2; 4]
= foldr add 0 [2; 4]
= add 2 (foldr add 0 [4])
= add 2 (foldr add 0 [4])
= add 2 (add 4 foldr add 0 [])
= add 2 (add 4 0)
= add 2 4
= 6
*)