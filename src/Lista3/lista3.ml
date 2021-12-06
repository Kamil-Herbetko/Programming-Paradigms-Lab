(* Kamil Herbetko *)

(* Zadanie 1 *)
let revNComp f n x =
    let rec revNComp_iter (f, n, x, accum) =
        if n <= 0 then accum
        else revNComp_iter (f, n-1, f x, x :: accum)
    in revNComp_iter (f, n, x, []);;

revNComp (fun a -> a*2) (5) (1);;
revNComp (fun a -> a *. a) (5) (2.0);;

(* Zadanie 2 *)
let area (a, b) f n =
    let rec listing iter =
        if iter < n then iter :: listing (iter + 1)
        else []
    in let xs = (listing 1) and diff = (b -. a) /. float(n - 1)
    in List.fold_left (+.) 0. (List.map (fun x -> f(a +. diff *. float(x)) *. diff) xs);;

area (1.0, 4.0) (fun x -> x *. x) 2;;
area (0.0, 1.0) (fun x -> x *. x *. x) 1000;;



