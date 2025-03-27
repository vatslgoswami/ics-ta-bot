(* Define the nat type *)
type nat = Zero | Succ of nat;;

let print_nat (n: nat): unit =
    let rec nat_to_int (n: nat) : int =
        match n with
        | Zero -> 0
        | Succ x -> nat_to_int(x) + 1
    in
    Printf.printf "%d\n" (nat_to_int n);;
;;

let one : nat = Succ Zero;;
let two : nat = Succ one;;
let three : nat = Succ two;;

let rec plus (n: nat) (m: nat) : nat =
    match n with
    | Zero -> m
    | Succ n' -> Succ (plus n' m)
;;

let rec mult (n: nat) (m: nat) : nat = 
    match n with
    | Zero -> Zero
    | Succ n' -> plus (mult n' m) m
;;

let rec minus (n: nat) (m: nat): nat = 
    match (n, m) with 
    | (_, Zero) -> n
    | (Zero, _ ) -> Zero 
    | (Succ n', Succ m') -> minus n' m'
;;

let four : nat = plus two two ;;
let eight : nat = plus four four ;;

(* Can we do something like this? 
      four = two + two?

  Two Challenges:
  1. How do you know the type?
        Type of (+) has been int -> int -> int. 
        We need to change it to nat -> nat -> nat?

        Given x + y? How do we know if you mean int or nat or something else? Solution: TYPE CHECKING! 

  2. Order of operations. 
        plus plus x y z -> plus plus_x y z -> plus (x + y) z 
                                                    -> (x + y) + z
        x - y - z =? (x - y) - z <> x - (y - z)

        Quick Fix is use brackets for now.
*)

(* Redefine the operators to use the custom functions *)
(* Infix operator syntax, that is x + y instead of + x y *)

(* let ( + ) = plus;; Commented out as this will conflict with a part of the code later. *)
let ( * ) = mult;;
let ( - ) = minus;;

let a: nat = two;;
let b: nat = two * three;;
let c: nat = b - one;;

(* You may test these out by running: 
print_nat(c);; 
*)

(* This is called operator overloading. Sometimes it can create certain issues. Hence, as discussed later in class, it is best used inside a module and not in general. *)

(* Complex Numbers as a Custom Type *)

type complexA = C of float * float;;

let my_number = C (2.0, 4.0);; (* represents 2 + 4i *)

let make (re : float) (im : float) : complexA = C (re, im);;

let add (C (a_re, a_im) : complexA) (C (b_re, b_im) : complexA) : complexA = 
  C (a_re +. b_re, a_im +. b_im)
;;

let add (c1 : complexA) (c2 : complexA) : complexA = 
  match (c1, c2) with 
  | (C(r1, i1), C(r2, i2)) -> C (r1 +. r2, i1 +. i2)
;;

(* If I call 'add' now, what will happen? Will it take:
    1. add: nat -> nat -> nat?
    2. add: complexA -> complexA -> complexA defined on line 98?
    3. add: complexA -> complexA -> complexA defined on line 102? 
    
    TEST IT OUT! *)


let stringify (C (a_re, a_im) : complexA) : string =
  Printf.sprintf "%f + %fi" a_re a_im
;;

(* Complex Numbers as a Record Type *)

type complexB = { re : float; im : float }

let makeB (re : float) (im : float) : complexB = { re; im }

let real_part (c: complexB) : float = c.re ;;
let im_part (c: complexB) : float = c.im ;;

let addB (c1 : complexB) (c2 : complexB) : complexB = 
  { re = c1.re +. c2.re; im = c1.im +. c2.im }

let stringifyB (c : complexB) : string =
  Printf.sprintf "%f + %fi" c.re c.im

(* Complex Numbers as a Module *)

module ComplexModule = struct
    type t = { re : float; im : float }
  
    let make re im = { re; im }
  
    let add a b = { re = a.re +. b.re; im = a.im +. b.im }
    let ( + ) = add
  
    let stringify c = Printf.sprintf "%f + %fi" c.re c.im
  end

let z1 = ComplexModule.make 3.0 4.0;;
let z2 = ComplexModule.make 1.0 (-2.0);;
let sum = ComplexModule.(z1 + z2);;

(* Test this by running: 
Printf.printf "Sum = %s\n" (ComplexModule.stringify sum);;
*)

(* Back to Natural Numbers*)

(* One May Define nat in several ways: *)
(* type nat = Z | S of nat;; *)
(* Or just using ints or float? *)

(* What is common? *)

module type NAT = sig
  type t                 (* Abstract type *)
  val zero : t
  val succ : t -> t
  val add : t -> t -> t
  val to_int : t -> int
end

(* Implementation 1 *)

module NatInt : NAT = struct
  type t = Nat of int

  let zero : t = Nat 0

  let succ (x : t) : t =
    match x with
    | Nat n -> Nat (n + 1)

  let add (x : t) (y : t) : t =
    match (x, y) with
    | (Nat n1, Nat n2) -> Nat (n1 + n2)

  let to_int (x : t) : int =
    match x with
    | Nat n -> n
end

(* Implementation 2 *)

module NatCustom : NAT = struct
  type t = Zero | Succ of t

  let zero : t = Zero
  let succ n : t = Succ n

  let rec add (x:t) (y:t) =
    match x with
    | Zero -> y
    | Succ n -> Succ (add n y)

  let rec to_int (x:t):int =
    match x with
    | Zero -> 0
    | Succ n -> 1 + (to_int n)
end

(* Functor for testing Nat *)

module TestNat (N : NAT) = struct
  let zero = N.zero
  let one = N.succ N.zero
  let two = N.add one one
  let four = N.add two two
  let five = N.add one four

  let run_tests () =
    Printf.printf "zero: %d\n" (N.to_int zero);
    Printf.printf "one: %d\n" (N.to_int one);
    Printf.printf "two: %d\n" (N.to_int two);
    Printf.printf "four: %d\n" (N.to_int four);
    Printf.printf "five: %d\n" (N.to_int five);
end

(* Instantiate the TestNat functor with both implementations *)

module TestNatInt = TestNat(NatInt)
module TestNatCustom = TestNat(NatCustom)

(* Run the tests *)

let () =
  Printf.printf "Testing NatInt:\n";
  TestNatInt.run_tests ();

  Printf.printf "\nTesting NatCustom:\n";
  TestNatCustom.run_tests ();

(* That is, the user sees both as the same *)
(* The module allows us to package the custom definitions in a very specific way such that a user cannot see implementation details. *)
