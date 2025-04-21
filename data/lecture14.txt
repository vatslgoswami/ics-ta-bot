(* Lecture 14, Mar 17 *)

(* Updates: 

1. Midterm grades are out! If your midterm score does not meet your expectations, please set a 1:1 with me. 
2. Regrades are open for one week. 
3. Extra Credit: meme contest and poetry contest are open. The submissions are on gradescope and open till Sunday.
4. Leetcode Saturdays are back. Attend at least 5 sessions this semester for extra credit.
5. Assignment 6 is out.
6. Aalok will conduct a discussion session on Saturday at 8:30 PM. *)

(* Let us recall what we have learned so far! *)

let rec factorial (n : int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)
;;

(* Let us look at how these functions are implemented by the computer.

When we call factorial 3, we have the following:

factorial 3 -> Not the base case -> Write that you need to compute 
'3 * factorial 2' and then compute 'factorial 2'

This process is called creating a stack frame. Basically, the computer is writing down that after the recursive call, it needs to do something else. In this case, it needs to multiply by 3. 

That is ...

factorial 3 -> Create a stack frame for 3 * factorial 2
factorial 2 -> Create a stack frame for 2 * factorial 1
factorial 1 -> Create a stack frame for 1 * factorial 0
factorial 0 -> Base case -> Return 1

Then, the process unwinds ... 

factorial 1 = 1 * 1 -> Return 1
factorial 2 = 2 * 1 -> Return 2
factorial 3 = 3 * 2 -> Return 6

When factorial is called, the computer stores all the waiting function calls. All the stack frames. This can take a lot of memory, as well as a lot of time. One way to reduce it is called tail recursion! *)

(* Tail Recursion *)
(* The recursive call is the last operation in the function *)

let rec factorial_helper (a : int) (n : int) : int =
  match n with
  | 0 -> a
  | _ -> factorial_helper (a * n) (n - 1)
;;

let factorial_tail (n : int) : int =
  factorial_helper 1 n
;;

(* 

What is happening here? 

factorial_tail 3 -> calls factorial_helper 1 3

factorial_helper 1 3 -> Not the base case -> create stack frame `factorial_helper (1 * 3) 2'
factorial_helper 3 2 -> ...

But hey, I can just reuse the stack frame, as the previous stack frame had only a recursion call, no other operations. 

Hence:

factorial_helper 3 2 -> factorial_helper (3 * 2) 1
factorial_helper 6 1 -> factorial_helper (6 * 1) 0
factorial_helper 6 0 -> Base case reached -> returns 6

So we had only one stack frame, reused. At the implementation level, this needs low memory. And is faster. 

This optimization is called Tail Call Optimization (TCO).

*)

(* Is it really faster? We can measure it as follows: *)

let measure_time (f : 'a -> 'b) (arg : 'a) : float =
  let start_time = Sys.time () in
  let _ = f arg in
  Sys.time () -. start_time
;;

(* Measure the time for both the implementations *)
let n = 50000 ;;

(*
let () =
  Printf.printf "Naive Factorial Time: %f seconds)\n" (measure_time factorial n);
  Printf.printf "Tail-Recursive Factorial Time: %f seconds)\n" (measure_time factorial_tail n)
;;

*)

(* What about triangle? *)

let rec triangle (n : int) : int = 
  match n with 
  | 0 -> 0
  | _ -> n + triangle (n - 1)
;;

(* What about fibonacci? *)

let rec fib (n : int) : int =
  match n with
  | 0 -> 0
  | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)
;;


(* Solution *)
let rec fib_helper (a : int) (b : int) (n : int) : int =
  match n with
  | 0 -> a
  | 1 -> b
  | _ -> fib_helper b (a + b) (n - 1)
;;

let fib_tail (n : int) : int =
  fib_helper 0 1 n
;;

(* 

   fib_tail 5 
-> fib_helper 0 1 5
-> fib_helper 1 (0+1) 4
= fib_helper 1 1 4
-> fib_helper 1 2 3
-> fib_helper 2 3 2
-> fib_helper 3 (2+3) 1
-> 5 (BASE CASE)

*)

(* Can map, filter, and fold be converted to tail recursions? *)

let rec fold (f : 'b -> 'a -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> fold f (f v x) xs
;;

(* Turns out fold is already tail recursive. What about fold right? *)

let rec fold_r (f : 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | x :: xs -> f x (fold_r f v xs)
;;

(* Fold left is tail recursive. Fold right is just fold right in the reverse. As the composition of tail recursive functions is tail recursive, all we need is a tail recursive list reversal *)

let rec reverse_helper (a : 'a list) (l: 'a list) : 'a list =
  match l with
  | [] -> a
  | x :: xs -> reverse_helper (x :: a) xs
;;  

let reverse (l : 'a list) : 'a list = reverse_helper [] l ;;

(* Are all recursive functions tail recursive? Clearly not by default. Can every recursive function be transformed into a tail-recursive form? Think about it. *)


(* Let us revisit a midterm problem: 

Problem 3: Don't Panic, Just Repeat! 

Write a function layer: (int -> int) -> int -> int -> int

That takes in a function f, an initial value x, and a number n, and applies f to x exactly n times. *)

let rec layer (f : int -> int) (x : int) (n: int) : int = 
  match n with 
  | 0 -> x
  | _ -> f (layer f x (n - 1))
;;

(* This can be made into a tail recursive function without new arguments or helper functions. *)

let rec layer_tail (f : int -> int) (x : int) (n : int) : int = 
  match n with 
  | 0 -> x
  | _ -> layer_tail f (f x) (n - 1)
;;

(* Now, let us look at a special case of layering. Here, the type of f is instead f : int -> unit 

Recall that a unit type is one where there may be side effects (such as assert statements or printing) but no data. *)

let rec layer_unit (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> 
    f n;
    layer_unit f (n - 1);
;;

(* Here is a concrete example:  *)

let print_n n = Printf.printf "%d\n" n ;;
let () = layer_unit print_n 4 ;;

(* This prints:

4
3
2
1

How to do it in the other order? *)

let rec layer_unit_asc (f : int -> unit) (n : int) : unit =
  match n with
  | 0 -> ();
  | _ -> 
    layer_unit_asc f (n - 1);
    f n;
;;

(* This correct. Here, the recursive function layer is called first, and then f is called. However, this is not tail recursive. How can you make it tail recursive? We will use a helper function. Instead of tracking an object representing data, we will track the number of times the recursion has been called. Consider *)

let rec layer_asc_helper (i: int) (f : int -> unit) (n : int) : unit =
  match (i = n) with 
  | true -> f n;
  | false -> f i; layer_asc_helper (i+1) f n
;;

let layer_unit_tail (f : int -> unit) (n : int) : unit = layer_asc_helper 1 f n ;;

(* Alternatively, using if ... then ... else, and with (i < n), just to cover all cases: *)

let rec layer_asc_helper_ite (i: int) (f : int -> unit) (n : int) : unit =
  if i > n then 
    ()
  else begin
    f i;
    layer_asc_helper_ite (i + 1) f n
  end
;;

(* This is a very specific type of tail recursion. Such a tail recursion is called a for loop. *)

let layer_unit_for (f : int -> unit) (n : int) : unit =
  for i = 1 to n do
    f i
  done
;;