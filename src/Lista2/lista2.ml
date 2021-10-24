(* Kamil Herbetko *)

(* Zadanie 1 *)
let rec find xs x =
    match xs with
    | h::t when h = x -> true
    | [] -> false
    | h::t -> find t x;;
let findnull = find [];;
findnull 3;;
findnull 'a';;
let find123 = find [1; 2; 3];;
find123 3;;
find123 4;;
let findabc = find ['a'; 'b'; 'c'];;
findabc 'a';;
findabc 'd';;


(* Zadanie 2*)
let rec split2Rec xs =
    match xs with
    | [] -> ([], [])
    | h :: t -> let(l1, l2) = split2Rec t in (h :: l2, l1);;

split2Rec ([]);;
split2Rec ([1]);;
split2Rec ([1;2]);;
split2Rec ([1; 2; 3]);;
split2Rec ([1; 2; 3; 4]);;
split2Rec (['a'; 'b'; 'c']);;

let split2Tail xs =
    let rec split2Tail_iter (xs, accum)=
        match xs with
        | [] -> ([], [])
        | h :: t -> let accum = (l1, l2) in split2Tail_iter(t, (h::l2, l1))
    in split2Tail_iter(xs, ([], []));;

split2Tail_iter[];;
split2Tail [1; 2];;
split2Tail [1];;
split2Tail [1;1;2;2;3;3];;
split2Tail [1; 2; 3];;
split2Tail [1; 2; 3; 4];;
split2Tail [1; 2; 3; 4; 5; 6];;
split2Tail ['a'; 'b'; 'c'];;


