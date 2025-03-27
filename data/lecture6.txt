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

type bin = Empty | BinZero of bin | BinOne of bin

let bin_2 = BinZero(BinOne(Empty));; 
let bin_13 = BinOne(BinZero(BinOne(BinOne(Empty))));;

let rec bin_to_int (n:bin):int = 
  match n with
  | Empty -> 0
  | BinZero x -> 2 * bin_to_int(x)
  | BinOne x -> 2 * bin_to_int(x) + 1
;;

let rec bin_to_nat (n:bin):nat = 
  match n with
  | Empty -> Zero
  | BinZero x -> mult (Succ (Succ Zero)) (bin_to_nat(x))
  | BinOne x -> plus (mult (Succ (Succ Zero)) (bin_to_nat(x))) (Succ Zero)
;;

let one : nat = Succ Zero;;

let rec fib (n:nat): nat = 
  match n with 
  | Zero -> one
  | Succ (Zero) -> one
  | Succ (Succ (x)) -> plus (fib (Succ x)) (fib x) 
;;

print_int (bin_to_int (bin_plus bin_13 bin_2)) ;;
print_newline () ;;