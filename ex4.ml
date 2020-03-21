(*
Nom : ROLLAND Julien
Groupe : 1bis
*)

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
let rang carte =
  match carte with
  |Nombre 1 -> 1
  |Roi -> 2
  |Reine -> 3
  |Valet -> 4
  |Nombre n -> 15 - n
;;

let meilleure_carte carte1 carte2 =
  let r1 = rang carte1 in
  let r2 = rang carte2 in
  if r1 = r2 then Egalite
  else if r1 > r2 then Gagnant (Joueur 1)
  else Gagnant (Joueur 2)
;;