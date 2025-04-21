(* Recall Custom Types *)
type nat = Zero | Succ of nat;;

let rec print_nat (n: nat): unit =
    let rec nat_to_int (n: nat) (acc: int): int =
      match n with
      | Zero -> acc
      | Succ x -> nat_to_int x (acc + 1)
    in
    print_int (nat_to_int n 0);;
    print_newline ();;
;;

(* Assertions *)

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

(* A First and Naive Implementation of Division *)
let rec div (n: nat) (m: nat): nat =
    match n with
    | Zero -> Zero
    | Succ n' -> Succ (div (minus n m) m)
;;

(* A First and Naive Implementation of Division *)

(* You can define new exception messages such as: *)
exception Custom_Division_by_zero ;;

let rec savyamm_div (n: nat) (m: nat): nat =
    match (n, m) with
    | (_ , Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | (Succ n', _) -> Succ (div (minus n m) m)
;;

print_nat (savyamm_div (Succ Zero) Zero);;

(* However, this code is still wrong, for example consider: *)

assert (savyamm_div two three = Zero);;

(* We want to differentiate between the following: *)
(* 'minus two three = Zero' and 'minus three three = Zero' *)

(* Let us introduce comparison! *)
let rec less_than (n: nat) (m: nat): bool = 
    match (n, m) with 
    | (_, Zero) -> false
    | (Zero, _) -> true
    | (Succ n', Succ m') -> less_than n' m'
;;

let rec safe_and_correct_div (n: nat) (m: nat): nat =
    match (n, m) with
    | (_, Zero) -> raise Custom_Division_by_zero
    | (Zero, _) -> Zero
    | _ when (less_than n m) -> Zero  (* Stop when n < m *)
    | _ -> Succ (safe_and_correct_div (minus n m) m)
;;

(* But I don't like this. Let us clean it up *)

let rec clean_div (n: nat) (m: nat): nat =
  match (n, m) with
  | (_, Zero) -> raise Custom_Division_by_zero
  | (Zero, _) -> Zero
  | _ ->
      match (minus n m) with
      | Zero when not (n = m) -> Zero              (* not (n = m) is same as (n <> m) *)
      | diff -> Succ (clean_div diff m)  (* Only count full subtractions *)
;;

print_nat (clean_div two three);;
print_nat (clean_div three two);;
print_nat (clean_div two Zero);;
print_nat (clean_div Zero two);;

(* Our code ends with an exception as soon as one is raised! *)
(* The same will happen with nested functions. The world breaks with exceptions! *)
(* We instead want to support a way to handle exceptions well. *)

(* For this, we will use option types! *)
(* Option types in OCaml represent values that may or may not be present. *)
(* They are defined as 'type 'a option = None | Some of 'a' where 'a' is any type. *)
(* This is useful for functions that may not return a valid result, such as division by zero. *)

let safe_minus_int (n: int) (m: int): int option =
    if n >= m then Some (n - m) else None
;;

let safe_reciprocal (n: float): float option =
    if n = 0.0 then None else Some (1.0 /. n)
;;

let rec safe_minus (n: nat) (m: nat): nat option = 
    match (n, m) with 
    | (_, Zero) -> Some n
    | (Zero, _ ) -> None
    | (Succ n', Succ m') -> safe_minus n' m'
;;

let rec new_safe_div (n: nat) (m: nat) : nat option = 
    match (n, m) with 
    | (_, Zero) -> None
    | (Zero, _) -> Some Zero 
    | (Succ n', Succ m') ->
        match (safe_minus n m) with
        | None -> Some Zero
        | Some diff -> Some (Succ (new_safe_div (diff) m))
;;

(* What about the remainder? *)

let rec divmod (n : nat) (m: nat) : (nat*nat) option =
    match (n, m) with
    | (_, Zero) -> None
    | (Zero, _) -> Some (Zero, Zero)
    | _ -> 
        match (safe_minus n m) with
        | None -> Some (Zero, n)
        | Some diff -> 
            match (divmod diff m) with
            | None -> None
            (* If safe_minus is correct, this case should never match! 
            Hence we can introduce a custom exception statement here. 
            This would be a good use of an exception. *)
            | Some (q, r) -> Some (Succ (q),r)
;;

(* Prove the correctness of division, that is prove that: *)
(* divmod n m = Some (q, r) if and only if n = plus (mult m q) r and (m <> Zero) *)
(* This requires the proof of correctness of safe_minus, as well as induction on both n and m *)

(*  First prove that:
    
    if (n <= m),    safe_minus n m = Some r 
    else,           safe_minus n m = None

    Use Induction on n as well as m. Here is a sketch:

    Base Case:  n = Zero, then safe_minus Zero m = None  (by definition)
                m = Zero, then safe_minus n Zero = n     (by definition)

    Induction Hypothesis: Assume safe_minus n' m' is correct for all n' < n, m' < m.
    
    Inductive Step: By definition, for n = Succ n' and m = Succ m', 
    
        safe_minus (Succ n') (Succ m') = safe_minus n' m'.
        
        By the inductive hypothesis, safe_minus n' m' is correct, so safe_minus (Succ n') (Succ m') is also correct.
*)

(*  Then prove:

    divmod n m = Some (q,r) if and only if n = plus (mult m q) r and (m <> Zero)
    
    Here is a sketch for your reference. This is an incomplete proof. Attempt to write it down. 
    First let us get rid of the case of m = Zero. Then, focus on m <> Zero.
    
    Proof by Induction on n:
    
    Base Case: n = Zero. This case can be completed by definition.
                    
            So for all m <> Zero, divmod Zero m = Some (Zero, Zero)

    Induction Hypothesis: 

            For all n' < n, 
                for all m <> Zero, divmod n' m = Some (q, r) if and only if n = plus (mult m q) r

    Induction Step: 

        If safe_minus n m = None, then ...
        If safe_minus n m = Some diff, then consider (divmod diff m).
            It must be the cas (divmod diff m) = Some (q, r)          (In which case apply Induction Hypothesis)

*)

