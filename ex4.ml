(*
Nom : ROLLAND Julien
Groupe : 1bis
*)

(*
Fonction non demandé mais utile pur la suite.
*)
let reverse l =
  let rec reverse l r =
    match l with
    |[] -> r
    |hd::lt -> reverse lt (hd::r)
  in
  reverse l []
;;

type couleur = Pique | Carreau | Coeur | Trefle;;
type valeur = Roi | Reine | Valet | Nombre of int;;
(* avec Nombre n tel que 1 ≤ n ≤ 10  *)
type carte = { couleur : couleur; valeur : valeur };;

(*
1)
*)
let exemple_carte = {couleur = Pique; valeur = Roi};;

(*
2)
*)
type joueur = Joueur of int;;
type comparaison = Gagnant of joueur | Egalite;;

(*
3)
*)
let meilleure_carte carte1 carte2 =
  let rang carte_v =
    match carte_v with
    |Nombre 1 -> 1
    |Roi -> 2
    |Reine -> 3
    |Valet -> 4
    |Nombre n -> 15 - n
  in
  let r1 = rang carte1.valeur in
  let r2 = rang carte2.valeur in
  if r1 = r2 then Egalite
  else if r1 < r2 then Gagnant (Joueur 1)
  else Gagnant (Joueur 2)
;;

(*
4)
*)
let est_paire carte1 carte2 =
  carte1.valeur = carte2.valeur
;;

(*
5)
*)
let est_brelan carte1 carte2 carte3 =
  carte1.valeur = carte2.valeur && carte2.valeur = carte3.valeur
;;

(*
6)
*)
let est_couleur carte1 carte2 carte3 =
  carte1.couleur = carte2.couleur && carte2.couleur = carte3.couleur
;;

(*
7)
*)
Random.self_init ();;

let tirage_valeur () =
  let rand = (Random.int 13) + 1 in
  match rand with
  |13 -> Roi
  |12 -> Reine
  |11 -> Valet
  |n -> Nombre n
;;

(*
8)
*)
let tirage_couleur () =
  let rand = Random.int 4 in
  match rand with
  |0 ->  Pique
  |1 -> Carreau
  |2 -> Coeur
  |3 -> Trefle
  |_ -> failwith "Random bug"
;;

(*
9)
*)
let tirage_carte () =
  { couleur = tirage_couleur (); valeur = tirage_valeur ()}
;;

(*
10)
*)
let rec play carte_l1 carte_l2 main =
  let print_v v =
    match v with
    |Roi -> Printf.printf "Roi de "
    |Reine -> Printf.printf "Reine de "
    |Valet -> Printf.printf "Valet de "
    |Nombre n -> Printf.printf "%d de " n
  in
  let print_c c =
    match c with
    |Pique -> Printf.printf "Pique "
    |Carreau -> Printf.printf "Carreau "
    |Coeur -> Printf.printf "Coeur "
    |Trefle -> Printf.printf "Trefle "
  in
  let print_cartes c1 c2 =
    Printf.printf "Le joueur 1 a un "; print_v c1.valeur; print_c c1.couleur;
    Printf.printf "et le joueur 2 a un "; print_v c2.valeur; print_c c2.couleur;
    Printf.printf "\n"
  in
  
  match carte_l1 with
  |[] -> Printf.printf "Joueur 2 gagne la partie\n"
  |hd1::lt1 -> match carte_l2 with
    |[] -> Printf.printf "Joueur 1 gagne la partie\n"
    |hd2::lt2 -> print_cartes hd1 hd2; match meilleure_carte hd1 hd2 with
      |Gagnant (Joueur 1) -> Printf.printf "Joueur 1 gagne la manche\n"; play (reverse (hd1::hd2::(reverse lt1))) lt2 main
      |Gagnant (Joueur 2) -> Printf.printf "Joueur 2 gagne la manche\n"; play lt1 (reverse (hd1::hd2::(reverse lt2))) main
      |Gagnant (Joueur n) -> failwith "Non valid player num"
      |Egalite -> match main with
        |true -> Printf.printf "Egalite ! Joueur 1 garde les cartes\n"; play (reverse (hd1::hd2::(reverse lt1))) lt2 (not main)
        |false -> Printf.printf "Egalite ! Joueur 2 garde les cartes\n"; play lt1 (reverse (hd1::hd2::(reverse lt2))) (not main)
;;

let p1 = [tirage_carte (); tirage_carte (); tirage_carte ()];;
let p2 = [tirage_carte (); tirage_carte (); tirage_carte ()];;

play p1 p2 true;;

(*
11)
*)
let rec carte_presente carte carte_l =
  match carte_l with
  |[] -> false
  |hd::lt -> if hd = carte then true
    else carte_presente carte lt
;;