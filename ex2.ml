(*
Nom : ROLLAND Julien
Groupe : 1bis
*)

(*
1)
*)

let rec gcd a b =
  if a = b then a
  else if a > b then gcd (a - b) b
  else gcd b a
;;

Printf.printf "pgcd(24, 64) = %d\n" (gcd 24 64);;

(*
2)
Au debut a = 24, b = 64, a < b donc gcd b a.
Ensuite a = 64, b = 24, a > b donc gcd (a - b) b.
a = 40, b = 24, a > b donc gcd (a - b) b.
a = 16, b = 24, a < b donc gcd b a.
a = 24, b = 16, a > b donc gcd (a - b) b.
a = 8, b = 16, a < b donc gcd b a.
a = 16, b = 8, a > b donc gcd (a - b) b.
a = 8, b = 8, a = b donc le resultat est a = b = 8.
*)


(*
3)
*)
let prime a b =
  (gcd a b) = 1
;;

(*
4)
*)
let phi n =
  let rec aux s acc = 
    if s > n then acc
    else match prime s n with
      |true -> aux (s + 1) (acc + 1)
      |false -> aux (s + 1) acc
  in
  aux 2 0
;;