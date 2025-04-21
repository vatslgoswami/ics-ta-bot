(* Lecture 13, Mar 3 *)

(* Map, Filter, and Fold: *)

let rec map (f: 'a -> 'b) (l: 'a list) : 'b list =
  match l with
  | [] -> []
  | h :: t -> (f h) :: (map f t)
;;

let rec filter (pred: 'a -> bool) (l: 'a list) : 'a list =
  match l with
  | [] -> []
  | x :: xs -> 
    match pred x with
    | true -> x :: filter pred xs
    | false -> filter pred xs
;;

let rec fold_right (f: 'a -> 'b -> 'b) (v: 'b) (l: 'a list) : 'b =
  match l with
  | [] -> v
  | h :: t -> f h (fold_right f v t)
;;

(* Some useful functions *)

let length (l : 'a list) : int =
  fold_right (fun _ s -> 1 + s) 0 l ;; 

let forall (pred : 'a -> bool) (l : 'a list) : bool =
  fold_right (fun x s -> pred x && s) true l ;;

let exists (pred : 'a -> bool) (l : 'a list) : bool =
  fold_right (fun x s -> pred x || s) false l ;;

let concatenate (l1: 'a list) (l2: 'a list): 'a list =
  fold_right (fun x s -> x :: s) l2 l1 ;;

(* Init as Unfolding *) 
let rec init (n : int) (f : int -> 'a) : 'a list =
  match n with 
  | _ when n <= 0 -> []
  | _ -> concatenate (init (n - 1) f) [f (n - 1)]
;;

(*

  init 5 (fun x -> x*x) 
= concatenate (init 4 (fun x -> x)) [16]
= concatenate (concatenate (init 3 (fun x -> x)) [9]) [16]
...

= concatenate (concatenate (init 3 (fun x -> x)) [3]) [4]
= [0] @ [1] @ [2] @ [3] @ [4]
= [0; 1; 4; 9; 16]

Let us say there is a g such that g (f (x)) = x. 
Then, 

map g (init n f) = [0; 1; ... ; (n - 1)]

*)

(* From Origami to Plumbing *)

let cube (x: int): int = x * x * x;;
let result1 : int = cube (12) + cube (1) ;;
let result2 : int = (12 |> cube) + (1 |> cube) ;;

(* Folding with Lists and Pipes *)

let cumbersome_result : int = fold_right (fun x s -> x + s) 0 (map (fun x -> x * x) [1; 2; 3; 4])

let result : int =
  [1; 2; 3; 4]
  |> map (fun x -> x * x)
  |> fold_right (fun x s -> x + s) 0 
;;

(* This can be written as just one fold *)

let result : int =
  [1; 2; 3; 4]
  |> fold_right (fun x s -> (x * x) + s) 0 
;;

(* Can all maps be written as fold? *)

let map_fold (f: 'a -> 'b) (l: 'a list) : 'b list = 
  fold_right (fun x s -> (f x) :: s) [] l ;;

(* How about filter? *)

let filter_fold (p: 'a -> bool) (l: 'a list) : 'a list = 
  fold_right 
  (fun x s -> if (p x) then (x :: s) else s) 
  [] 
  l 
;;

(* The n-queens problem *)
(* Max Bezzel published the eight queens puzzle in 1848. Franz Nauck published the first solution in 1850 and extended the puzzle to the n queens problem. Since then, many mathematicians, including Carl Friedrich Gauss, have worked it. 

Consider a 4x4 chess board. Can you place four queens that do not threaten each other? 

        - Q - - 
        - - - Q 
        Q - - - 
        - - Q - 
*)

(* Given n, can you find a nxn chess board configuration where n queens do not threaten each other? How do you even represent such a chessboard? *)

(* As a chess board with "-" and "Q" *)
let representation1 = [
  ["-"; "Q"; "-"; "-"]; 
  ["-"; "-"; "-"; "Q"]; 
  ["Q"; "-"; "-"; "-"]; 
  ["-"; "-"; "Q"; "-"]
] ;;

(* As just booleans *)
let representation2 = [
  [false; true; false; false]; 
  [false; false; false; true]; 
  [true; false; false; false]; 
  [false; false; true; false]
] ;;

(* As a list of positions of the queens, where index is the row. *)
let representation3 = [
  1; 
  3; 
  0; 
  2
] ;;

(* As pairs (col, row). We will use this going ahead. *)
let representation4 = [
  (1, 0); 
  (3, 1); 
  (0, 2); 
  (2, 3)
] ;;


(* Check if two queens threaten each other *)
let threatens ((col1, row1) : int * int) ((col2, row2) : int * int) : bool =
  col1 = col2 || row1 = row2 || abs (col1 - col2) = abs (row1 - row2) ;;
  
(* Check if a new queen is safe with respect to existing queens *)
let is_safe (new_queen : int * int) (existing_queens : (int * int) list) : bool =
  forall (fun q -> not (threatens new_queen q)) existing_queens ;;
  
(* Helper function: Generate all possible positions for the next row *)
let next_row (existing_queens: (int * int) list) (n: int) (r: int) : (int * int) list  =
  init n (fun x-> (x, r))                             (* make a list of all positions *)
  |> filter (fun q -> is_safe q existing_queens)      (* and then you filter *)
;;

(* Solve the n-Queens problem starting with a given row *)
let rec solve (n: int) (r: int) (existing_queens: (int * int) list) : ((int * int) list) list =
  match r with
  | _ when r = n -> [existing_queens]
  | _ ->
    next_row existing_queens n r
    |> map (fun r -> (r :: existing_queens))
    |> fold_right (fun qs s -> (solve n (r + 1) qs @ s)) []
;;
  
let n_queens n = solve n 0 [] ;;

(* Representation of a chessboard as a list of lists of strings *)

let chessboard_row (c : int) (n : int) : string = 
  init n (fun x -> x)
  |> map (fun x -> if (x = c) then "Q" else "-")
  |> fold_right (fun x s -> x ^ " " ^ s) ""
;;

let chessboard (queens: (int * int) list) : string = 
  queens
  |> map (fun (c, r) -> (chessboard_row c (length queens)))
  |> fold_right (fun x s -> x ^ "\n" ^ s) ""
;;

(* And now finally: pretty-print all solutions *)
let rec print_solutions solutions =
  match solutions with
  | [] -> ()
  | x :: xs ->
      print_endline (chessboard x);
      print_endline "----------";
      print_solutions xs
;;
  
let n = 9 ;;
let () = print_solutions (n_queens n) ;;