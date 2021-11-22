(* Kamil Herbetko *)
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let rec lfrom k = LCons (k, function () -> lfrom (k + 1));;

let rec toLazyList = function
    | [] -> LNil
    | x :: xs -> LCons (x, function () -> (toLazyList xs));;

let rec ltake = function
    | (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons (x, xsfunc)) -> x :: ltake (n-1, xsfunc());;


(* Zadanie 1 *)
let rec buyTicket xs n =
    match (xs, n) with
    | ([], _) -> []
    | ((_, xsfun) :: t, 1) -> (xsfun(), xsfun) :: t
    | (h :: t, n) -> h :: (buyTicket t (n - 1));;


let xs = [("?", function () -> "komputer"); ("?", function () -> "laptop"); ("?", function () -> "ziemniaki"); ("?", function () -> "Kot"); ("?", function () -> "Nic")];;
let ys = [];;
buyTicket xs 1;;
buyTicket (buyTicket xs 1) 3;;
buyTicket ys 3;;