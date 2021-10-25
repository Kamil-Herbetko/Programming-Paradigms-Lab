let rec quicksort = function
 [] -> []
 | [x] -> [x]
 | h::t -> let small = List.filter (fun y -> y < h ) t
 and same = List.filter(fun y -> y = h) tl
 and large = List.filter (fun y -> y > h ) tl
 in quicksort small @ (h::same) @ quicksort large;;


let rec bubblesort lst =
  let sortstep = function
  | hd1 :: hd2 :: tl when hd1 > hd2 ->
        hd2 :: bubblesort (hd1 :: tl)
  | hd1 :: tl ->
        hd1 :: bubblesort tl
  | tl -> tl
in
if lst = sortstep then
    lst
else
    bubblesort sortstep

let rec select x = function
    | [] -> x, []
    | h :: t ->
        let x, h = if x < h then x, h else h, x in
        let x, t = select x t in
        x, h :: t;;

let rec selectionsort = function
    | [] -> []
    | h :: t -> match select h t with
    | h, t -> h :: sellectionsort t;;
