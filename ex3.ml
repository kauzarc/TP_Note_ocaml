(*
Nom : ROLLAND Julien
Groupe : 1bis
*)

let length l = 
  let rec length l acc =
    match l with
    |[] -> acc
    |hd::lt -> length lt (acc + 1)
    in
  length l 0
;;

let reverse l =
  let rec reverse l r =
    match l with
    |[] -> r
    |hd::lt -> reverse lt (hd::r)
  in
  reverse l []
;;

let print_bool b =
  match b with
  |true -> Printf.printf "%s\n" "true"
  |false -> Printf.printf "%s\n" "false"
;;

type coin = Head | Tails;;
type board = coin list;;

let rec print_board board =
  match board with
  |[] -> ()
  |hd::lt -> match hd with
    |Head -> Printf.printf "%s\n" "Head"; print_board lt
    |Tails -> Printf.printf "%s\n" "Tails"; print_board lt
;;

type plays = Flip of int;;

let flip c =
  match c with
  |Head -> Tails
  |Tails -> Head
;;

let play plays board =
  let len = length board in
  let Flip n = plays in
  let n1 = n mod len in
  let n2 = (n1 + 1) mod len in

  let rec play l count return =
    match l with
    |[] -> return
    |hd::lt -> if count = n1 || count = n2
      then play lt (count + 1) ((flip hd)::return)
      else play lt (count + 1) (hd::return)
  in
  
  let inv = play board 0 [] in
  reverse inv
;;

let rec play_multiple plays_l board =
  match plays_l with
  |[] -> board
  |hd::lt -> play_multiple lt (play hd board)
;;

let rec is_wining board =
  match board with
  |[] -> true
  |Head::lt -> is_wining lt
  |Tails::lt -> false
;;

let b = [Head; Tails; Head; Tails];;
let pl = [Flip 1; Flip 2];;

print_bool (is_wining (play_multiple pl b));;

let has_sol board =
  match board with
  |x1::x2::x3::x4::lt -> (x1 = x3) = (x2 = x4)
  |_ -> failwith "Board size not suported"
;;

let find_sol board = 
  if not (has_sol board) then failwith "There is no solution";
  []
;;

print_bool (has_sol [Head; Tails; Tails; Head]);;