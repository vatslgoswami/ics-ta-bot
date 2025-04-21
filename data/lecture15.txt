(* Lecture 15, Mar 21 *)

(* Updates: 

1. Midterm grades are out! If your midterm score does not meet your expectations, please set a 1:1 with me. 
2. Regrades are open for one week. 
3. Extra Credit: meme contest and poetry contest are open. The submissions are on gradescope and open till Sunday.
4. Leetcode Saturdays are back. Attend at least 5 sessions this semester for extra credit.
5. Assignment 6 is out.
6. Aalok will conduct a discussion session on Saturday at 8:30 PM. *)

(* A tail recursive function is one where the recursive call is the last operation performed. For example: *)

let rec gcd (a: int) (b: int) : int =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
;;

(* And against my will I will use if then else statements: *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;

(* Then there is this weird tail recursive version of a layer function: *)

let rec layer_asc_helper_ite (i: int) (f : int -> unit) (n : int) : unit =
  if i > n then 
    ()
  else (
    f i;
    layer_asc_helper_ite (i + 1) f n
  )
;;

let layer_unit_tail (f : int -> unit) (n : int) : unit = layer_asc_helper_ite 1 f n ;;

(* Such a tail recursion is called a for loop. *)

let layer_unit_for (f : int -> unit) (n : int) : unit =
  (* i is the iterator *)
  (* 1 to n is iteration range *)

  (* whatever below is the body of the loop *)
  for i = 1 to n do
    f i
  done
;;

(* Now using this, let us compute factorial. *)

(* Solution: *)

let factorial (n: int) : int =
  (* references *)
  let a = ref 1 in
  for i = 1 to n do
    (* assignments and deref. *)
    a := !a * i
  done;
  !a
;;

(* We need to learn some concepts before we parse this code *)

(* Recall let statements: *)

let x = 5 ;;

(* On the left side you have a variable. On the right side of equality you have an expression. Either a primitive expression or something like: *)

let y = x + 4 ;;

(* And of course we have functions like let rec ... or let ...*)

(* In general, a let statement binds an expression with a name. OCaml supports nested let bindings and shadowing: *)

let x2 = 5 in
let y2 = x2 + 4 in
y2;;

(* This won't work let z = x2 - 2 ;; *)

(* Shadowing creates a new binding without altering the original value. That is why we can have something like: *)

let x = 5 ;;
Printf.printf "%d \n" x;;

let x = x + 1 ;; 
(* New binding that shadows the old one *)

(* Consider the set of all information that defines the condition of a program at a particular point in time. We call this snapshot of the program's memory and variable values as the program state *)

let x = 5 ;;
let y = x + 2;;

(* Here, after line 97, the state of the program is (x, y) = (5, 7). *)

(* A mutable reference in OCaml is a way to store a value that can be modified after it's created.

Everything you have seen till now has been an immutable variable. That is, once you assign a value to a variable, you can't change it. You may create a new variable with the same name and overwrite the old variable. 

A mutable reference allows you to work around this restriction by creating a container (or cell) that holds a value. This is closer to how computers actually store information in memory. *)

let x = ref 5 ;;

(* This creates a reference that holds the value 5.
x is not directly the value 5 — it's a container (or pointer) that holds the value 5. *)

(* What is the type of this x? *)
(* x : int ref *)

(* To access a value in a reference, we use dereferencing: *)

(* (!) : 'a ref -> 'a  *)
let value = !x ;; 
(* value = 5 *)

(* To update x to something else we write: *)
x := 10 ;; 

(* Changes the value stored in x to 10 *)

(* Let us revisit factorial: *)

let factorial (n: int) : int =
  let a = ref 1 in
  for i = 1 to n do
    a := !a * i
  done;
  !a
;;

(* Line 131 is the body of the for loop. By using mutable references, we ensure that there are no functions. The operations do not produce an output, that is, have a unit type. *)

(* How does it connect to tail recursion? *)

let rec factorial_helper (a : int) (i: int) (n : int) : int =
  match (i > n) with
  | true -> a
  | false -> factorial_helper (a * i) (i + 1) n
;;

let factorial_tail (n : int) : int =
  factorial_helper 1 1 n
;;

(* Observe that the state of the program is modified in a similar way in the two cases. *)

(* How do you prove the correctness of a program with a for loop? By defining properties on the state of the program. *)

(* A loop invariant is a property (or condition) of the state of the program that:

1. Holds before the loop starts.
2. Remains true after every iteration of the loop.
3. When the loop ends, it helps prove that the program produces the correct result. *)

let factorial (n: int) : int =
  let a = ref 1 in
  (* Loop Invariant: At the start of each iteration !a = factorial(i - 1) *)
  for i = 1 to n do
    (* At the start of iteration i:
       Invariant holds: !a = (i - 1)!
       We are about to compute i-th factorial by multiplying !a by i.
    *)
    a := !a * i;

    (* After this step:
       Invariant holds: !a = factorial(i)
    *)
  done;

  (* When the loop terminates:
     i = n + 1
     Therefore, !a = factorial(n)
  *)
  !a
;;

(* Loop invariants are similar to proofs using the Principle of Mathematical Induction. 

Initialization, that is, the condition must hold before the start of the loop is like the base case. 

The Maintenance Step, that is, if the condition holds before the start of an iteration it must hold at the end of it is like the inductive step.

And finally, the Termination Step completes the proof.

Going forward, every program with a for loop must have a loop invariant. Or rather, first write a loop invariant, and then write a loop. *)


(* Let us write a loop for the following: *)

let rec fib (n : int) : int =
  if n <= 0 then 0
  else if n = 1 then 1 
  else fib (n - 1) + fib (n - 2)
;;

let fib_for (n: int) : int =
  if n <= 0 then 0
  else if n = 1 then 1
  else (
    let a = ref 0 in
    let b = ref 1 in 
    for i = 2 to n do
      (* Invariant: At the start of iteration i = k,
         !a = fib(k - 2) and !b = fib(k - 1) *)
      let next = !a + !b in (* next = fib(k) *)
      a := !b; (* a = fib(k - 1) *)
      b := next; (* b = fib(k) *)
      (* Invariant: At the end of iteration i = k,
         !a = fib(k - 1) and !b = fib(k) *)
    done;
    (* Value of i = n + 1 *)
    !b (* b = fib(i - 1) = fib(n) *)
  )
;;


(* How would you do GCD? *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;

(* You do not know how many times to repeat the operation,
   because the number of recursive calls depends on the size of the inputs
   and how quickly they reduce through the modulo operation.
   
   For this, we will use the while loop! *)

let rec while_loop (condition: bool) (body: unit) =
  if condition then (
    body;
    while_loop condition body
  )
;;

(* The while loop, also has a loop body similar to the for loop. It also has a condition, which is usually a predicate on the state of the system. In the while loop, the state of the program is updated repeatedly till the state satisfies the condition. *)

(* To implement GCD, one can thing of (a, b) as the state of the system, and (b = 0) as the stopping condition. The loop body can update the state by replacing (a, b) by (b, a mod b). We will see more of this in the next class. *)

(* In general, the looping method is called iteration. Here we have a loop body that we repeatedly apply to the state of the program. This is exactly the same as tail recursion, where we have all mutable variables as the state of the program to which we apply the function repeatedly. *)

(* These new concepts of iteration, mutability, and referencing take us toward procedural programming where programs are interpreted as a set of instructions instead of functions.  *)