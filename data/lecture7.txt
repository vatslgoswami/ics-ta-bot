(* Native Types *)

let my_int : int = 42;;
let my_float : float = 42.0;; (* Always needs a point '.' *)
let my_char : char = 'a';;
let my_string : string = "Hello, OCaml!";;
let my_string2 : string = "a";;
let my_bool : bool = true;;
let () : unit = print_endline "This is a unit type";;
let my_tuple : int * float * string = (my_int, my_float, my_string);;

(* Maximum *)

let my_max1 (a: int) (b: int) : int =
    match (a, b) with
    | (x, y) when x >= y -> a
    | (x, y) -> b
;;

let my_max2 (a: int) (b: int) : int =
   if a >= b then a 
   else b
;;

(* Custom Types *)
type nat = Zero | Succ of nat

type day = 
  | Sunday 
  | Monday 
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

let next_weekday (d: day) : day = 
    match d with 
    | Sunday -> Monday
    | Monday -> Tuesday
    | Tuesday -> Wednesday
    | Wednesday -> Thursday
    | Thursday -> Friday
    | Friday -> Monday
    | Saturday -> Monday
;;

let two_business_days_from_today (d: day): day = next_weekday(next_weekday d);;

type shape = 
  | Circle of float
  | Rectangle of float * float;;

let pi : float = 3.141592;;

let area (s : shape) : float =
    match s with
    | Circle r -> r *. r *. pi
    | Rectangle (l, b) -> l *. b
;;

let c : shape = Circle 1.0;;
let r : shape = Rectangle (4.0, 5.0);;
Printf.printf "\n%f %f\n" (area c) (area r);;

(* Assertions *)

let one : nat = Succ Zero;;
let two : nat = Succ one;;  
let three : nat = Succ two;; 

let rec plus (n: nat) (m: nat) : nat =
    match n with
    | Zero -> m
    | Succ n' -> Succ (plus n' m)
;;

let rec plus2 (n: nat) (m: nat) : nat =
    match m with
    | Zero -> n
    | Succ m' -> Succ (plus n m')
;;

assert (plus three one = plus two two);; 

(* Currying *)

let plus3 : nat -> nat = plus three;;
let four : nat = plus3 one;;

let rec plus_new : nat -> nat -> nat =
    fun n m ->
    match n with
    | Zero -> m
    | Succ n' -> Succ (plus n' m)
;; 

let plus_new_3 : nat -> nat = plus_new three;;

let four : nat = plus_new_3 one;;

(* plus_newest: nat * nat -> nat *)
(* THIS IS NOT THE SAME AS nat -> nat -> nat *)
let rec plus_newest (p: nat * nat) : nat =
    match p with
    | (Zero, m) -> m
    | (Succ n', m) -> Succ (plus_newest (n', m))
;;

(* Induction: plus Zero m = plus m Zero = m *)

(* Induction: 
    To Prove: plus Zero m = plus m Zero = m 

    nat = Zero | Succ of nat

    Induction on m.

    Base Case: nat = Zero 

        1. plus Zero Zero = plus Zero Zero (reflexivity of equality)
        2. plus Zero Zero = Zero (Line 133. Defined it that way. )

    Induction Hypothesis: 

        If for all m', plus Zero m' = plus m' Zero = m' (IH)
        then prove that: 
            plus Zero (Succ m') = plus (Succ m') Zero = Succ m'  

    Induction Step: 
        1. plus Zero (Succ m') = Succ m' (Defined in line 133.)
        2. plus (Succ m') Zero = Succ (plus m' Zero) (Defined in line 134.)
                            = Succ (m')              (By IH)

        QED.


*)
