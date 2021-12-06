(* Kamil Herbetko *)

(* Zadanie 1 *)

let reverse4 (x, y, z, t) =
    (t, z, y, x);;

reverse4((6, 6.0, true, "ala")) = ("ala", true, 6.0, 6);;

(* Zadanie 2 *)

let rec substitute (xs, x, y) =
    if xs = [] then []
    else if List.hd xs = x then y :: substitute(List.tl xs, x, y)
    else List.hd xs :: substitute(List.tl xs, x, y);;

substitute([1; 2; 3; 4; 5; 6; 5; 5; 7], 5, 1) = [1; 2; 3; 4; 1; 6; 1; 1; 7];;
substitute([1; 2; 3], 5, 3) = [1; 2; 3];;
substitute([], 2, 3) = [];;
substitute(['a'; 'b'; 'c'; 'c'; 'd'; 'e'; 'e'], 'c', 'g') = ['a'; 'b'; 'g'; 'g'; 'd'; 'e'; 'e'] ;;

(* Zadanie 3*)

let rec insert (xs, x, y) =
    if y < 1 then x :: xs
    else if xs = [] then  [x]
    else List.hd xs :: insert (List.tl xs, x, y-1);;

insert([1; 2; 3; 4], 5, 1) = [1; 5; 2; 3; 4];;
insert([1; 2; 3; 4], 5, 7) = [1; 2; 3; 4; 5];;
insert(['a'; 'b'; 'c'], 'd', -3) = ['d'; 'a'; 'b'; 'c'];;
