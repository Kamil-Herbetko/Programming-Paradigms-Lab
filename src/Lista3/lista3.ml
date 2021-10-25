(* Kamil Herbetko *)

(* Zadanie 1 *)
let revNComp f n x =
    let rec revNComp_iter (f, n, x) =
        if n <= 0 then []
        else if n = 1 then [x]
        else x :: revNComp_iter (f, n-1, f x)
    in List.rev (revNComp_iter (f, n, x));;

revNComp (fun a -> a*2) (5) (1);;
revNComp (fun a -> a *. a) (5) (2.0);;

(* Zadanie 2 *)
let rec listing (a, b, n, first_a, iterator) =
    if iterator = 1 then (a, 0.) :: (listing (a +. ((b -. first_a) /. float(n - 1)), b, n, first_a, iterator + 1))
    else if (iterator < n) then (a, (b -. first_a) /. float(n - 1)) :: (listing (a +. ((b -. first_a) /. float(n - 1)), b, n, first_a, iterator + 1))
    else [(b, ((b -. first_a) /. float(n - 1)))];;


let area (a, b) f n =
    let xs = listing(a, b, n, a, 1) in
    List.fold_left (+.) 0. (List.map (fun (x, y) -> f(x) *. y) xs);;

area (1.0, 4.0) (fun x -> x *. x) 2;;
area (0.0, 1.0) (fun x -> x *. x *. x) 1000;;



