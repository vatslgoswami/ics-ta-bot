(* Lecture 16, Mar 25 *)

(* A tail recursive function is one where the recursive call is the last operation performed. For example: *)

let rec gcd (a: int) (b: int) : int =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
;;

(* For loop is a special kind of tail recursive function: *)

let rec factorial (n: int) : int = 
  match n with
  | 0 -> 1
  | _ -> n * factorial (n - 1)
;;

let factorial_for (n: int) : int =
  let a = ref 1 in
  for i = 1 to n do
    a := !a * i;
  done;
  !a
;;

(* To prove correcntess of factorial_for, we will use the state of the system and loop invariants. 
   A loop invariant is a property of the state of the program such that:
   
   1. Holds before the loop starts.
   2. Remains true after every iteration of the loop.
   3. When the loop ends, it helps prove that the program produces the correct result.

   We will introduce a new mutable variable ghost_i which tracks the value of i. We will use it to track the
   value of i even after the end of the loop. This is required to prove the correctness. 
   
   Instead of stating the loop invariant in text, we will state it as assertions. *)

let factorial_for (n: int) : int =
  let a = ref 1 in
  (* ghost variable to track i even after the loop: *)
  let ghost_i = ref 1 in 
  for i = 1 to n do
    (* Update the ghost variable first: *)
    ghost_i := i;

    (* You can get the state : (a, i, ghost_i, n) of the program through: *)
    (* Printf.printf"(%d, %d, %d, %d) \n" !a i !ghost_i n; *)

    (* Loop invariant before update: !a = factorial(i - 1)*)
    assert(!a = factorial(!ghost_i - 1));
    a := !a * i;
    (* Loop invariant after update: !a = factorial(i - 1)*)
    assert(!a = factorial(!ghost_i));

  done;
  assert(!a = factorial(!ghost_i));
  assert(!ghost_i = n);
  !a
;;

(* Correctness Theorem:

   For all integers n >= 1:
   1. Safety: The function factorial_for(n) executes without any assertion failures
   2. Functional Correctness: The returned value equals factorial(n)
   
   Moreover, the assertions precisely capture the correctness conditions:
   - The loop invariant (!a = factorial(!ghost_i)) holds at:
     * Loop entry (after first initialization)
     * Before each iteration
     * After each iteration
     * Loop exit
   - The post-condition (!a = factorial(n)) holds at function return


   Proof:
   1. Base Case: Invariant holds before first iteration
   2. Inductive Step: If invariant holds before iteration i, 
      it holds after updating ghost_i and a
   3. Post-condition: Upon exit, invariant and loop exit 
      condition imply (!a = factorial(n))

*)


(* How would you do the same for GCD? *)

let rec gcd (a: int) (b: int) : int =
  if b = 0 then a
  else gcd b (a mod b)
;;

(* Recall that here you do not know the number of times you need to update the state. 
   For this, we introduced the while loop: *)

let rec while_loop (condition: bool) (body: unit) =
  if condition then (
    body;
    while_loop condition body
  )
;;

(* GCD can be implemented using the while loop as follows: *)

let gcd_while (a: int) (b: int) : int =
  let x = ref a in
  let y = ref b in
  while !y <> 0 do
    let temp = !y in
    y := !x mod !y;
    x := temp
  done;
  !x
;;

(* We can decorate this code with assertions and ghost variables to prove correctness: *)

let gcd_while (a: int) (b: int) : int =
  
  let x = ref a in
  let y = ref b in

  while !y <> 0 do
    (* Loop invariant: gcd(!x, !y) = gcd(a, b) *)
    assert(gcd !x !y = gcd a b);
    
    let temp = !y in
    y := !x mod !y;
    x := temp;
  done;

  (* Loop invariant: *)
  assert(gcd !x !y = gcd a b);

  (* Negation of loop condition: *)
  assert(!y = 0);
  
  (* Post-condition: result is GCD of original inputs, follows from loop invariant and the negation of loop condition *)
  assert(!x = gcd a b);
  
  !x
;;

(* 
  Correctness proof outline:
  1. Initialization: 
     - Before loop, x=a, y=b
     - gcd(a,b) = gcd(a,b) holds trivially
  2. Maintenance:
     - At each iteration, gcd(x,y) = gcd(old_x, old_y)
     - Because gcd(x,y) = gcd(y, x mod y)
  3. Termination:
     - When y=0, gcd(x,0) = x
     - By invariant, x = gcd(a, b)
*)


(* While loops are more expressive than for loops. Because one can write a for loop in terms of a while loop: *)
let for_using_while (init: int) (n: int) (body: int -> unit) : unit =
  let i = ref init in
  while !i <= n do
    body !i;
    i := !i + 1;
  done;
;;

(* There is a subtle mistake in this. The value of !i gets updated to (n+1) at the end of the loop, which we do not want in our for loop. You can test it out by printing the state of the program (that is, the value of !i in this case). There are (at least) four ways to fix it! *)

(* Three correct ways to implement for-loop using while-loop in OCaml: *)

(* Method 1: Use strict inequality and handle final iteration separately *)
let for_using_while1 (init: int) (n: int) (body: int -> unit) : unit =
  let i = ref init in
  while !i < n do  (* < instead of <= *)
    body !i;
    i := !i + 1
  done;
  body !i
;;

(* Method 2: Decrement after loop completes *)
let for_using_while2 (init: int) (n: int) (body: int -> unit) : unit =
  let i = ref init in
  while !i <= n do
    body !i;
    i := !i + 1
  done;
  i := !i - 1 
;;

(* Method 3: Conditional increment *)
let for_using_while3 (init: int) (n: int) (body: int -> unit) : unit =
  let i = ref init in
  while !i <= n do
    body !i;
    if !i < n then i := !i + 1  (* Only increment if not last iteration *)
    else () 
  done
;;

(* There are several variations of the above three that one can use. One key differences remains: 
    Native for-loops don't expose the loop variable after termination but these implementations all expose i. 
    
    The best practice is if you need for-loop behavior, use OCaml's native for-loops. *)



(* Let us prove correctness of Fibonacci using while loop *)

let rec fib (n: int) : int = 
  match n with 
  | 0 | 1 -> 1
  | _ -> fib (n - 1) + fib (n - 2)
;;

let fibonacci (n: int) : int =
  let a = ref 0 and b = ref 1 in
  let count = ref n in
  while !count > 1 do
    b := !a + !b;
    a := !b - !a;
    count := !count - 1
  done;
  !b
;;


(* For this, let us introduce a ghost variable which is ref (n - !count). Let us call it diff. *)

let fibonacci (n: int) : int =
  let a = ref 0 and b = ref 1 in
  let count = ref n in
  let diff = ref (n - !count) in 
  while !count > 1 do
    
    (* Loop invariant: a = fib(diff) and b = fib(diff + 1) *)
    assert(!a = fib(!diff));
    assert(!b = fib(!diff + 1));
    assert(!count >= 1);
    assert(!diff = (n - !count));
    b := !a + !b;
    a := !b - !a;
    count := !count - 1
  done;

  (* Here, we know condition is false. *)
  assert(!count <= 1);

  (* However, we need that !count = 1. For this, we can change our loop invariant to have !count >= 1. *)
  
  (* From Loop invariant: *)
  assert(!count >= 1);

  (* Therefore: *)
  assert(!count = 1);
  assert(!diff = (n - 1));
  assert(!b = fib(n));
  
  !b
;;

(*  Proof of Correctness with Ghost Variable:
    
    I. Loop Invariants:
    1. !diff = n - !count
    2. !a = fib(!diff)
    3. !b = fib(!diff + 1)
    4. !count >= 0
    
    II. Initialization (Before loop):
    - !count = n
    - !diff = 0 = n - n   (Invariant 1)
    - !a = 0 = fib(0)     (Invariant 2)
    - !b = 1 = fib(1)     (Invariant 3)
    - !count = n >= 0     (Invariant 4)
    
    III. Maintenance (Each iteration):
    Assume invariants hold at start of iteration where !count = k > 0:
    - !diff = n - k
    - !a = fib(n - k)
    - !b = fib(n - k + 1)
    
    After update:
    - new_b = fib(n - k) + fib(n - k + 1) = fib(n - k + 2)
    - new_a = fib(n - k + 1)
    - new_count = k - 1
    - new_diff = n - (k - 1) = n - k + 1
    
    Now verify:
    1. new_diff = n - new_count = n - (k - 1)
    2. new_a = !b = fib(n - k + 1) = fib(new_diff)
    3. new_b = fib(new_diff + 1) = fib(n - k + 2)
    4. new_count = k - 1 >= 0 (since k > 0)
    
    IV. Termination:
    Loop exits when !count = 1
    From Invariant 1:
    !diff = n - 1
    From Invariant 2:
    !b = fib(!diff + 1) = fib(n - 1)
  *)


(* Is Prime? *)

let is_prime (n: int) : bool =
  if n < 2 then false
  else
    let i = ref 2 in
    let has_divisor = ref false in
    while !i * !i <= n && not !has_divisor do
      if n mod !i = 0 then has_divisor := true;
      i := !i + 1
    done;
    not !has_divisor
;;

let () = Printf.printf "%b\n" (is_prime 97);;  (* Should print true *)
let () = Printf.printf "%b\n" (is_prime 100);; (* Should print false *)


(* What would be the loop invariants? *)