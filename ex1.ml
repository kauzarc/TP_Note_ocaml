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
r1 n'est pas recursive terminal car elle n'est pas recursive. Elle apelle f1.
r2 n'est pas recursive non plus. Elle n'apelle que r3 et les type ne coresponde meme pas.
r3 n'est pas recursive non plus mais aux est recursive terminale. Car il n'est pas necessaire de conserver les valeur de la pile pour evaluer "aux tl (acc+hd)" ou "aux tl acc".
r4 n'est pas recursive non plus. Elle n'apelle aucune autres fonction durant son execution.
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