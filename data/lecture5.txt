type nat = Zero | Succ of nat

let a : nat = Succ (Succ (Succ (Succ (Succ (Zero)))));;
let b : nat = Succ (Succ (Succ (Succ (Zero))));;

let rec nat_to_int (n : nat) = 
  match n with 
  | Zero -> 0
  | Succ m -> 1 + (nat_to_int m)
;;

let plusTwo (n : nat) : nat = Succ (Succ (n));;
let plusThree (n : nat) : nat = Succ (plusTwo (n));;

let rec plus (n : nat) (m : nat) : nat = 
  match n with 
  | Zero -> m
  | Succ n' -> Succ (plus n' m)
;;

let rec mult (n : nat) (m : nat) : nat = 
  match n with 
  | Zero -> Zero 
  | Succ p -> plus (m) (mult p m)
;;

print_int (nat_to_int (mult a b)) ;;
print_newline () ;;