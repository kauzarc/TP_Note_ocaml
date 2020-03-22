(*
Nom : ROLLAND Julien
Groupe : 1bis
*)

(*
1)
Car dans la ligne : "y -> y", le y match avec toutes les valeurs. Donc il match avec 2 et la fonction retourne 2.
*)

(*
2)
Dans le cas "y ->", on pourais utiliser la valeur de y a droite de la fleche. Tansi que dans le cas "_ ->", on ne peut pas.
*)

(*
3)
let f3 x =
  match x with
    | 1 -> 9
    | 2 -> 4
;;
Cette fonction devrais declancher le warning.
*)

(*
4)
let rec r1 x =
  match x with
    | 0 -> 1
    | _ -> 10 + r1 (x-1)

Pas terminal, car "r1 (x - 1)" n'est pas la derniere instruction de la fonction a etre evaluée. C'est "(10) + (r1 (x-1))".

let rec r2 a b =
  match a with
    | true -> true
    | false -> a || (r2 (not a) (not b))
  
Pas terminal, car "r2 (not a) (not b)" n'est pas la derniere instruction de la fonction a etre evaluée. C'est "(a) || (r2 (not a) (not b))".

let r3 x l =
  let rec aux y acc =
    match y with
      | [] -> acc
      | hd::tl -> if hd > x then aux tl (acc+hd) else aux tl acc
  in aux l 0

Pas recursive mais "aux" est recursive terminal, car "aux tl (acc+hd)" ou "aux tl acc" serons les derniere instruction a etre evaluée.

let rec r4 x y =
  if x +. 1. < y then 3
  else 5

Pas recursive, car la fonction ne fait aucun appel a elle meme.
*)

(*
5)
La ";" premiere ligne.
Le "in" est absant dans "let y = x + 3 in".
Le "Printf.printf "%d" x;" n'a aucun sens ici.

let return x = x

let rec f x =
  let y = x + 3 in
  if y = 0 then 100
  else y + f (x - 1)

let g x y =
  let v =
    if x < y then x else y
 in
return (v + 10)

*)