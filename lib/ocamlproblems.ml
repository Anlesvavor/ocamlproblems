
let rec last = function
  | [] -> None
  | [x] -> Some x
  | _ :: tail -> last tail

let%test _ = last ["a" ; "b" ; "c" ; "d"] = Some "d"
let%test _ = last [] = None
let%test _ = last [1] = Some 1

let rec last_two l =
  match l with
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: tail -> last_two tail

let%test _ = last_two ["a" ; "b" ; "c" ; "d"] = Some ("c", "d")
let%test _ = last_two ["a"] = None
let%test _ = last_two [] = None

let rec nth index = function
  | [] -> None
  | head::tail -> (match index with
      | 0 -> Some head
      | _ -> nth (index - 1) tail
    )
;;

let%test _ = nth 2 ["a" ; "b" ; "c" ; "d"] = Some "c"
let%test _ = nth 2 [] = None

let rec length ?(carry=0) = function
  | [] -> carry
  | _::tail -> length ~carry:(carry + 1) tail


let%test _ = length ["a" ; "b" ; "c" ; "d"] = 4
let%test _ = length [] = 0

let rec rev = function
  | [] -> []
  | [x] -> [x]
  | head::tail -> (rev tail) @ [head]

let%test _ = rev ["a" ; "b" ; "c" ; "d"] = ["d" ; "c" ; "b" ; "a"]
let%test _ = rev ["a"] = ["a"]
let%test _ = rev [] = []

let is_palindrome list =
  let rev_list = rev list in
  let eq_with_right_option a opt = match opt with
    | Some q -> a == q
    | None -> false
  in
  list
  |> List.mapi (fun i x -> (x, nth i rev_list))
  |> List.for_all (fun (p, q) -> eq_with_right_option p q)

let%test _ = is_palindrome [] = true
let%test _ = is_palindrome ["a"] = true
let%test _ = is_palindrome ["a"; "a"] = true
let%test _ = is_palindrome ["a"; "b"; "a"] = true
let%test _ = is_palindrome ["a" ; "b" ; "c" ; "d"] = false
let%test _ = is_palindrome ["a"; "b"] = false
let%test _ = is_palindrome ["b"; "b"; "a"] = false

type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let rec flatten (list : 'a node list) : 'a list =
  match list with
  | [] -> []
  | [One x] -> [x]
  | [Many xs] -> flatten xs
  | head::tail -> (flatten [head]) @ (flatten tail)
;;

let flatten_solution list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many xs :: t -> aux (aux acc xs) t
  in
  List.rev (aux [] list)

let%test _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] =
             ["a"; "b"; "c"; "d"; "e"]

let compress (list : 'a list) =
  let rec aux mem acc = function
    | [] -> acc
    | h :: t ->
      match mem with
      | None -> aux (Some h) (h :: acc) t
      | Some m -> if h == m
        then aux (Some m) acc t
        else aux (Some h) (h :: acc) t
  in
  match list with
  | [] -> []
  | _ :: t -> List.rev (aux None [] t)

let%test _ = compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
             = ["a"; "b"; "c"; "a"; "d"; "e"]

let rec compress_solution = function
  | a :: (b :: _ as t) -> if a = b then compress_solution t else a :: compress_solution t
  | smaller -> smaller

let%test _ = compress_solution ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
             = ["a"; "b"; "c"; "a"; "d"; "e"]
(* let pack = *)
(*   let rec aux acc = function *)
(*     | [] -> acc *)
(*     | h :: t -> *)
(*       match acc with *)
(*       | [(a::_ as tt)]::ttt -> if h == a then h :: tt else [h] :: ttt *)
(*       | x -> x *)

(* let pack list = *)
(*   let llist = List.map (fun x -> [x]) list in *)
(*   let aux acc list = *)
(*     let [(acc_head :: acc_tail as head)] :: tail = acc in *)
(*     let new_head = match list with *)
(*       | [x] :: _ -> if acc_head = x then head @ [x] else [x ; head] *)
(*       | [] -> [] *)
(*     in *)

let pack list =
  let rec aux acc acc2 list =
    let g = match acc2 with
      | g :: _ -> Some g
      | [] -> None
    in
    match list with
    | [] -> (acc2 :: acc)
    | x :: xs ->
      match g with
      | None -> aux acc [x] xs
      | Some g ->
        if x = g
        then aux acc (x :: acc2) xs
        else aux (acc2 :: acc) [x] xs
  in
  aux [] [] list
  |> List.rev
;;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
;;

let%test _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
             = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"]; ["e"; "e"; "e"; "e"]]

let pack_solution list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] list);;

let%test _ = pack_solution ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
             = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                ["e"; "e"; "e"; "e"]]

let encode list =
  let rec aux (count, _) acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count + 1, Some a) acc t
      else aux (0, Some b) ((count + 1, a) :: acc) t
  in
  List.rev (aux (0, None) [] list)
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

let%test _ = encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
             = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]


let encode_solution list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count + 1) acc t
      else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

encode_solution ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

let%test _ = encode_solution ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
             = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]

module Rle = struct
  type 'a rle =
    | One of 'a
    | Many of int * 'a

  let create a = function
    | 1 -> One a
    | n -> Many (n, a)
end

open Rle

let encode_b list =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (Rle.create x (count + 1)) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count + 1) acc t
      else aux 0 ((Rle.create a (count + 1)) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

let%test _ = encode_b ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
             = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
                Many (4, "e")]

let const a _ = a
;;

let decode list =
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many (c, x) :: xs -> aux ((List.init c (const x)) @ acc) xs
  in
  List.rev (aux [] list)
;;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
;;

let%test _ = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
             = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

let decode_solution list =
  let rec many acc n x =
    if n = 0 then acc else many (x :: acc) (n - 1) x
  in
  let rec aux acc = function
    | [] -> acc
    | One x :: xs -> aux (x :: acc) xs
    | Many (c, x) :: xs -> aux (many acc c x) xs
  in
  List.rev (aux [] list)
;;

decode_solution [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
;;

let%test _ = decode_solution [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
             = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

(* let duplicate list = *)
(*   let rec aux state acc list = match state, list with *)
(*     | None  , x :: _  -> aux (Some x) (x :: acc) list *)
(*     | Some s, _ :: xs -> aux (None  ) (s :: acc) xs *)
(*     | None, [] | Some _, [] -> acc *)
(*   in *)
(*   List.rev (aux None [] list) *)

(* type dup = | Repeat | No ;; *)

let duplicate list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (x :: x :: acc) xs
  in
  (* List.rev (aux [] list) *)
  aux [] (List.rev list)
;;

duplicate ["a"; "b"; "c"; "c"; "d"]
;;

let%test _ = duplicate ["a"; "b"; "c"; "c"; "d"]
             = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]

let replicate list times =
  let rec repeat count acc x =
    if count > 0
    then repeat (count - 1) (x :: acc) x
    else acc
  in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (repeat times acc x) xs
  in
  aux [] (List.rev list)
;;

replicate ["a"; "b"; "c"] 3;;

let%test _ = replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
;;

let drop list every =
  let rec aux count acc = function
    | [] -> acc
    | x :: xs ->
      if count = every
      then aux 1 acc xs
      else aux (count + 1) (x :: acc) xs
  in
  aux 1 [] list
  |> List.rev
;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
;;

let%test _ = drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
             = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

let drop_solution list every =
  let rec aux count = function
    | [] -> []
    | x :: xs ->
      if count = every
      then aux 1 xs
      else x :: aux (count + 1) xs
  in
  aux 1 list
;;

drop_solution ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
;;

let%test _ = drop_solution ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
             = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]

let split list at =
  let rec aux count (acc_l, acc_r) = function
    | [] -> (acc_l, acc_r)
    | x :: xs ->
      if count < at
      then aux (count + 1) (x :: acc_l, acc_r) xs
      else aux (count + 1) (acc_l, x :: acc_r) xs
  in
  let (acc_l, acc_r) = aux 0 ([], []) list in
  (List.rev acc_l, List.rev acc_r)
;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
;;

let%test _ = split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
             = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

let%test _ = split ["a"; "b"; "c"; "d"] 5
             = (["a"; "b"; "c"; "d"], [])

let split_solution list n =
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h :: t as l ->
      if i = 0
      then List.rev acc, l
      else aux (i - 1) (h :: acc) t
  in
  aux n [] list
;;

let%test _ = split_solution ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
             = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])

let%test _ = split_solution ["a"; "b"; "c"; "d"] 5
             = (["a"; "b"; "c"; "d"], [])

let slice list from until =
  let rec aux index = function
    | [] -> []
    | x :: xs ->
      if from <= index
      then (
        if index <= until
        then x :: aux (index + 1) xs
        else []
      )
      else aux (index + 1) xs
  in
  aux 0 list
;;

let%test _ = slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
             = ["c"; "d"; "e"; "f"; "g"]
;;

(* Tail recursive *)
let slice_2 list from until =
  let rec aux index acc = function
    | [] -> acc
    | x :: xs ->
      if from <= index
      then (
        if index <= until
        then aux (index + 1) (x :: acc) xs
        else acc
      )
      else aux (index + 1) acc xs
  in
  aux 0 [] list
  |> List.rev
;;

slice_2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
;;

let%test _ = slice_2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
             = ["c"; "d"; "e"; "f"; "g"]
;;

let slice_solution list i k =
  let rec take n = function
    | [] -> []
    | h :: t -> if n = 0 then [] else h :: take (n - 1) t
  in
  let rec drop n = function
    | [] -> []
    | _ :: t as l -> if n = 0 then l else drop (n - 1) t
  in
  take (k - i + 1) (drop i list)
;;


let%test _ = slice_solution ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
             = ["c"; "d"; "e"; "f"; "g"]
;;

let rotate list places =
  let places = places mod (List.length list) in
  let (left_side, right_side) = split list places in
  right_side @ left_side
;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
;;

let%test _ = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
             = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]

(* Tail recursive *)
let remove_at at list =
  let rec aux count acc = function
    | [] -> []
    | x :: xs ->
      if count = at
      then (List.rev acc) @ xs
      else aux (count + 1) (x :: acc) xs
  in
  aux 0 [] list
;;

let remove_at_2 at = function
  | [] -> []
  | x :: xs -> if at = 0 then xs else x :: remove_at (at - 1) xs
;;

let%test _ = remove_at 1 ["a";"b";"c";"d"]
             = ["a";"c";"d"]
;;

let insert_at value at list =
  let len = List.length list in
  if at >= len
  then list @ [value]
  else (
    let rec aux count acc = function
      | [] -> List.rev acc
      | x :: xs as t ->
        if count = at
        then (List.rev acc) @ (value :: t)
        else aux (count + 1) (x :: acc) xs
    in
    aux 0 [] list
  )
;;

let%test _ = insert_at "alfa" 1 ["a"; "b"; "c"; "d"]
             = ["a"; "alfa"; "b"; "c"; "d"]

let range from until =
  let rec aux from until acc = if from <= until
    then aux (from + 1) until (from :: acc)
    else acc
  in
  if from > until
  then aux until from []
  else List.rev (aux from until [])
;;

let%test _ = range 4 9
             = [4; 5; 6; 7; 8; 9]
;;
let%test _ = range 9 4
             = [9; 8; 7; 6; 5; 4]

let rand_select list amount =
  let upper_bound = List.length list in
  let rec aux acc = function
    | 0 -> acc
    | n -> aux ((List.nth list (Random.int upper_bound)) :: acc) (n - 1)
  in
  aux [] amount
;;

rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;

let lotto_select amount upper_bound =
  let rec aux acc = function
    | 0 -> acc
    | n -> aux ((Random.int (1 + upper_bound)) :: acc) (n - 1)
  in
  aux [] amount
;;

lotto_select 6 49;;

lotto_select 10 1;;

let%test _ = lotto_select 10 0
             = [0; 0; 0; 0; 0; 0; 0; 0; 0; 0]

(* Just utop *)
let remove_at at list =
  let rec aux count acc = function
    | [] -> []
    | x :: xs ->
      if count = at
      then (List.rev acc) @ xs
      else aux (count + 1) (x :: acc) xs
  in
  aux 0 [] list
;;

let permutation list =
  let rec aux acc = function
    | [] -> acc
    | l ->
      (* let l = List.mapi (fun i x -> x, i) l in *)
      let len = List.length l in
      let i = Random.int len in
      let item = List.nth l i in
      let new_list = remove_at i l in
      aux (item :: acc) new_list
  in
  aux [] list
;;

permutation ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"]
;;

(* let rec combination chosen list = *)
(*   let rec aux acc count list = *)
(*     match list with *)
(*     | [] -> acc *)
(*     | x :: xs -> *)
(*       if count = 0 *)
(*       then x :: acc *)
(*       else aux (x :: acc) (count - 1) xs *)
(*   in *)
(*   aux [] chosen list *)
(* ;; *)

(* let rec combination chosen list = *)
(*   let len = List.length list in *)
(*   let depth = len - (chosen - 1) in *)
(*   let rec aux (count : int) (acc : 'a list list) (list : 'a list) = *)
(*     match list with *)
(*     | [] -> acc |> List.rev *)
(*     | _ :: xs as t -> if count = 0 *)
(*       then aux 0 (t :: acc)  [] *)
(*       else aux (count - 1) (t :: acc) xs *)
(*   in *)
(*   let rec aux2 (acc : 'a list) (list : 'a list) : 'a list list = match list with *)
(*     | [] -> [] (\* Yeah, ignore this *\) *)
(*     | x :: xs as t -> *)
(*       let len2 = List.length t in *)
(*       if len2 = chosen *)
(*       then List.map (fun el -> el :: acc ) xs *)
(*       else aux2 (x :: acc) xs *)
(*   in *)

(* let rec combination (choosen : int) (list : 'a list) : 'a list list = *)
(*   let rec aux acc = function *)
(*     | [] -> [[]] *)
(*     | x :: xs as t -> *)
(*       if (List.length acc) = (choosen - 1) *)
(*       then List.map (fun el -> el :: acc) t *)
(*       else aux (x :: acc) xs *\) *)
(* in *)
(* aux [] list |> List.map List.rev *)
(* ;; *)
(* combination 3 ["a"; "b"; "c"; "d"; "e"];; *)
(* combination 3 [ "b"; "c"; "d"; "e"];; *)

(* combination 3 ["a"; "b"; "c"; "d"];; *)
(* combination 3 [ "b"; "c"; "d"];; *)


let slice_2 (list : 'a list) (from : int) (until : int) =
  let rec aux (index : int) (acc : 'a list) (list : 'a list) = match list with
    | [] -> acc
    | x :: xs ->
      if from <= index
      then (
        if index <= until
        then aux (index + 1) (x :: acc) xs
        else acc
      )
      else aux (index + 1) acc xs
  in
  aux 0 [] list
  |> List.rev
;;

let merge (getter : 'a -> int) (alist : 'a list) (blist : 'a list) : 'a list =
  let rec aux (acc : 'a list) (alist : 'a list) (blist : 'a list) =
    match alist, blist with
    | [], [] -> acc
    | t, [] -> (List.rev t) @ acc
    | [], t -> (List.rev t) @ acc
    | (x :: xs as tx), (y :: ys as ty) ->
      if (getter x) <= (getter y)
      then aux (x :: acc) xs ty
      else aux (y :: acc) tx ys
  in
  aux [] alist blist
  |> List.rev
;;

merge ((fun x -> x) : int -> int) [1;3;5;7] [0;2;4;6;8;10];;

(* let%test _ = merge ((fun x -> x) : int -> int) [1;3;5;7] [0;2;4;6;8;10] *)
(*              = [0;1;2;3;4;5;6;7;8;10] *)

let rec merge_sort (getter : 'a -> int) (list : 'a list) : 'a list =
  match list with
  | [] -> []
  | [x] -> [x]
  | [x; y] -> if (getter x) < (getter y) then [x; y] else [y; x]
  | _ ->
    let len = List.length list in
    let pivot = (len / 2) in
    let left_size = merge_sort getter (slice_2 list 0 (pivot - 1)) in
    let right_size = merge_sort getter (slice_2 list pivot (len - 1)) in
    merge getter left_size right_size
;;

merge_sort ((fun x -> x) : 'a -> int) [4;1;8;4;2;0;56;3];;

let length_sort (list : 'a list list) : 'a list list =
  merge_sort ((fun el -> List.length el) : 'a list -> int) list
;;

length_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

(* let%test _ = length_sort  [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"]; *)
(*                            ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]] *)
(*              = [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; *)
(*                 ["i"; "j"; "k"; "l"]] *)
(* ;; *)

let frequency (list : 'a list) : ((int * 'a) list) =
  let f (facc : ((int * 'a) list)) (el : 'a) =
    match List.find_opt (fun (_,x) -> x = el) facc with
    | Some _ -> List.map (fun (n,y) -> if y = el then (n + 1,y) else (n,y) ) facc
    | None -> (1, el) :: facc
  in
  List.fold_left f [] list
;;

let frequency_sort (list : 'a list) : ('a list) =
  frequency list
  |> merge_sort (fun (freq, _el) -> freq) (* Sort using the frequency *)
  |> List.map (fun (_freq, el) -> el) (* Forget frequency of each elment *)
;;

let list = [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
            ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

frequency list;;

frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]];;

let%test _ = frequency_sort [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
                             ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
             = [["o"]; ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["f"; "g"; "h"]; ["a"; "b"; "c"];
                ["d"; "e"]]

let is_prime (n : int) =
  let int_sqrt (n : int) =
    n
    |> float_of_int
    |> sqrt
    |> int_of_float
  in
  (* "obvious" cases *)
  match n with
  | 0 | 1 -> false
  | 2 -> true
  | _ ->
    let upper_bound = int_sqrt n in
    let rec aux (i : int) =
      if (n mod i) = 0
      then false
      else if i < upper_bound
      then aux (i + 1)
      else true
    in
    aux 2
;;

let%test _ = is_prime 1 = false;;
let%test _ = is_prime 60 = false;;
let%test _ = is_prime 1024 = false;;
let%test _ = is_prime 31 = true;;
let%test _ = is_prime 17 = true;;

let rec gcd (a : int) (b : int) : int =
  match a, b with
  | 0, b -> b
  | a, 0 -> a
  | a, b ->
    let r = a mod b in
    gcd b r
;;

gcd 13 27 = 1;;
gcd 20536 7826 = 2;;

let%test _ = gcd 13 27 = 1;;
let%test _ = gcd 20536 7826 = 2;;

let coprime (a : int) (b : int) : bool =
  gcd a b = 1
;;

let%test _ = coprime 13 27 = true;;
let%test _ = coprime 20536 7826 = false;;

let phi (m : int) : int =
  let rec aux (count : int) (curr : int) : int =
    if curr < m
    then if gcd curr m = 1
      then aux (succ count) (succ curr)
      else aux count (succ curr)
    else count in aux 0 1
;;

let%test _ = phi 10 = 4
;;

let factors (n : int) : int list =
  let rec aux (acc : int list) (f : int) (n : int) : int list =
    if n = 1
    then acc
    else if n mod f = 0
    then aux (f :: acc) 2 (n / f)
    else aux acc (succ f) n
  in
  aux [] 2 n |> List.rev
;;

factors 315
;;

let%test _ = factors 315 = [3; 3; 5; 7]
;;

let encode_solution list =
  let rec aux count acc =
    function
    | [] -> []
    | [x] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (count + 1) acc t
      else aux 0 ((count + 1, a) :: acc) t
  in
  List.rev (aux 0 [] list)
;;

let factorsb (n : int) : ((int * int) list) =
  n |> factors |> encode_solution |> List.map (fun (a,b) -> b,a)
;;

factorsb 315
;;

let%test _ = factorsb 315 = [(3, 2); (5, 1); (7, 1)]
;;

let phi_improved (n : int) : int =
  let int_pow (n : int) (p : int) : int =
    Float.pow (float_of_int n) (float_of_int p)
    |> int_of_float
  in
  n
  |> factorsb
  |> List.fold_left (fun acc (p, m) -> ((p - 1) * (int_pow p (m - 1))) * acc) 1
;;

let%test _ = phi_improved 10 = 4
;;

let%test _ = phi_improved 13 = 12
;;

let const _a b = b
;;

let rec from (n : int) : int Seq.node =
  Seq.cons n (fun () -> from (n + 1)) ()
;;

let prime_seq () : int Seq.node =
  Seq.filter is_prime (fun () -> from 2) ()
;;

(* Seq.iter (fun x -> Printf.printf "%d\n" x) (Seq.take 5 prime_seq) *)
(* ;; *)

let goldbach n =
  prime_seq
  |> Seq.take_while (fun x -> (x + 2) < n)
  |> Seq.map (fun x -> (x, n - x))
  |> Seq.find (fun (a,b) -> a + b = n && (is_prime b))
  |> function
  | None -> failwith "I guess that goldbach is wrong ;)"
  | Some x -> x
;;

goldbach 28;;

let%test _ = goldbach 28 = (5, 23)
;;
let%test _ = goldbach 68 = (7, 61)
;;
let%test _ = goldbach 54 = (7, 47)
;;
let golbach_list (from : int) (until : int) : (int * (int * int)) list =
  let p1 = prime_seq in
  let p2 = prime_seq in
  Seq.flat_map (fun a -> Seq.map (fun b -> (a + b, (a, b))) p2) p1
  |> Seq.filter (fun (n, _) -> n mod 2 = 0 && (from <= n && n <= until ))
  |> List.of_seq
;;

goldbach 28;;

let%test _ = goldbach 28 = (5, 23)
;;
let%test _ = goldbach 68 = (7, 61)
;;
let%test _ = goldbach 54 = (7, 47)
;;

let is_odd (n : int) : bool = n mod 2 = 0;;
let is_in_range (n : int) (from : int) (until : int) : bool = from <= n && n <= until;;

let goldbach_list (from : int) (until : int) : (int * (int * int)) list =
  let p1 = Seq.take_while (fun n -> n <= until) prime_seq in
  let p2 = Seq.take_while (fun n -> n <= until) prime_seq in
  Seq.flat_map (fun a -> Seq.map (fun b -> (a + b, (a, b))) p2) p1
  |> Seq.filter (fun (n, _) -> is_odd n && is_in_range n from until)
  |> List.of_seq
  |> List.sort_uniq (fun (n1, _) (n2, _) -> compare n1 n2)
;;

goldbach_list 9 20
;;

let%test _ = goldbach_list 9 20
             = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
                (20, (3, 17))]
;;

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;

let table2 (a : string) (b : string) (expr : bool_expr) : ((bool * bool * bool) list) =
  let a_list : ((string * bool) list) = [a,true; a,false] in
  let b_list : ((string * bool) list) = [b,true; b,false] in
  let ab_list = List.flatten (List.map (fun x -> List.map (fun y -> (x,y)) b_list) a_list) in
  let f (((a: string) ,(a_value : bool)),((_b : string), (b_value : bool))) =
    let rec aux (expr : bool_expr) = match expr with
      | And ((x : bool_expr), (y : bool_expr)) -> (aux x) && (aux y)
      | Or ((x : bool_expr), (y : bool_expr)) -> (aux x) || (aux y)
      | Not (x : bool_expr) -> not (aux x)
      | Var (x : string) -> if x = a then a_value else b_value
    in
    let result = aux expr in
    (a_value, b_value, result)
  in
  ab_list
  |> List.map f
;;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
;;

let%test _ = table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
             = [(true, true, true); (true, false, true); (false, true, false);
                (false, false, false)]
;;

let power_list (n : int) (list : 'a list) : 'a list list =
  let rec aux (n : int) (acc : 'a list list) = match n with
    | 0 -> acc
    | _ ->
      let acc =
        list
        |> List.map (fun phi ->
            acc |> List.map (fun psi -> phi :: psi))
        |> List.flatten
      in
      aux (pred n) acc
  in
  aux n [[]]
;;

(* (power_list 3 [true;false])|> List.map (fun ell -> List.combine ["a";"b";"c"] ell );; *)

let table (var_list : (string list)) (expr : bool_expr) : (((string * bool) list * bool) list) =
  let var_list = (power_list (List.length var_list) [true;false])
                 |> List.map (fun ell -> List.combine var_list ell )
  in
  let f vars =
    let rec aux (expr : bool_expr) = match expr with
      | And ((x : bool_expr), (y : bool_expr)) -> (aux x) && (aux y)
      | Or ((x : bool_expr), (y : bool_expr)) -> (aux x) || (aux y)
      | Not (x : bool_expr) -> not (aux x)
      | Var (x : string) ->
        (List.find_map (fun (str, value) -> if x = str then (Some value) else None) vars)
        |> function Some v -> v | None -> failwith "Variable not found!!!"
    in
    let result = aux expr in
    (vars, result)
  in
  var_list
  |> List.map f
;;

table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")));;

let%test _ = table ["a"; "b"] (And (Var "a", Or (Var "a", Var "b")))
             = [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
                ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]

(* Thanks wikipedia ;) https://en.wikipedia.org/wiki/Gray_code#Constructing_an_n-bit_Gray_code *)
let rec gray (n : int) : string list =
  let prefix_zeros = List.map (fun el -> "0" ^ el) in
  let prefix_ones = List.map (fun el -> "1" ^ el) in
  match n with
  | 1 -> ["0"; "1"]
  | _ ->
    let normal_pred_list = gray (pred n) in
    let reversed_pred_list = List.rev normal_pred_list in
    (prefix_zeros normal_pred_list) @ (prefix_ones reversed_pred_list)
;;

gray 1;;
gray 2;;
gray 3;;

let%test _ = gray 1 = ["0"; "1"];;
let%test _ = gray 2 = ["00"; "01"; "11"; "10"];;
let%test _ = gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"];;

type ('a, 'b) hufftree =
  | None
  | Node of 'a * ('a, 'b) hufftree * ('a, 'b) hufftree

let huffman (list : (string * int) list) : ((string * string) list) =
  let sorted = List.sort (fun (_, f1) (_, f2) -> compare f1 f2) list in
  let leaf (str, freq) = Node ((Some str, freq), None, None) in
  let get_weight_node x = match x with
    | Node ((_, w), _, _) -> w
    | None -> -1 (* """Minus infinity""" would be correct? *)
  in
  let rec construct acc = match acc with
    | [] -> failwith "Invaid state"
    | [x] -> x
    | a :: b :: rest ->
      let a_freq = get_weight_node a in
      let b_freq = get_weight_node b in
      let weight = a_freq + b_freq in
      let node = if a_freq < b_freq
        then Node ((Option.None, weight), a, b)
        else Node ((Option.None, weight), b, a)
      in
      let f a b = compare (get_weight_node a) (get_weight_node b) in
      construct (List.sort f (node :: rest))
  in
  let tree = construct (List.map (leaf) sorted) in
  let huffman_from_tree tree =
    let rec aux acc tree =
      match tree with
      | None -> failwith "Invalid state"
      | Node ((Some str, _), _, _) -> [(str, acc)]
      | Node (_, left, right) ->
        (aux (acc ^ "0") left) @ (aux (acc ^ "1") right)
    in
    aux "" tree
  in
  huffman_from_tree tree
;;

let fs = [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)];;

huffman fs;;

let%test _ = huffman fs
             = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100");
                ("e", "1101"); ("d", "111")]

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
  (* idk *)
(* let rec cbal_tree (n : int) = *)
(* ;; *)

(* cbal_tree 3 *)

(* let%test _ = cbal_tree 4 *)
(*              = [Node ('x', Node ('x', Empty, Empty), *)
(*                       Node ('x', Node ('x', Empty, Empty), Empty)); *)
(*                 Node ('x', Node ('x', Empty, Empty), *)
(*                       Node ('x', Empty, Node ('x', Empty, Empty))); *)
(*                 Node ('x', Node ('x', Node ('x', Empty, Empty), Empty), *)
(*                       Node ('x', Empty, Empty)); *)
(*                 Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)), *)
(*                       Node ('x', Empty, Empty))] *)

let is_symmetric a =
  match a with
  | Empty
  | Node (_, Empty, Empty) -> true
  | Node (_, Node _, Empty) | Node (_, Empty, Node _) -> false
  | Node (_, l, r) ->
    let rec is_mirror l r =
      match l, r with
      | Empty, Empty
      | Node (_, Empty, Empty), Node (_, Empty, Empty) -> true
      | Node (_, _, _), Empty | Empty, Node (_, _, _) -> false
      | Node (_, al, ar), Node (_, bl, br) -> is_mirror al br && is_mirror ar bl
    in
    is_mirror l r
;;

is_symmetric (Node (1, Node (2, Empty, Empty), Node (2, Empty, Empty)))
;;

is_symmetric (Node (1, Node (2, Node (3, Empty, Empty), Empty), Node (2, Empty, Empty)))
;;

let construct (list : int list) : int binary_tree =
  let rec aux acc list =
    match list with
    | [] -> acc
    | x :: xs ->
      let rec insert acc x =
        match acc with
        | Empty -> Node (x, Empty, Empty)
        | Node (value, left, right) ->
          if x < value
          then Node (value, (insert left x), right)
          else Node (value, left, (insert right x))
      in
      aux (insert acc x) xs
  in
  aux Empty list
;;

construct [3; 2; 5; 7; 1];;


is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]);;
not (is_symmetric (construct [3; 2; 5; 7; 4]));;


let%test _ = is_symmetric (construct [5; 3; 18; 1; 4; 12; 21]) = true;;
let%test _ = not (is_symmetric (construct [3; 2; 5; 7; 4])) = true;;

let rec count_leaves tree =
  match tree with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> (count_leaves l) + (count_leaves r)
;;

let%test _ = count_leaves (Node ('x', Node ('x', Empty, Empty),
                                 Node ('x', Node ('x', Empty, Empty), Empty)))
             = 2

let rec leaves tree =
  match tree with
  | Empty -> []
  | Node (_, Empty, Empty) as node -> [node]
  | Node (_, l, r) -> (leaves l) @ (leaves r)
;;

let%test _ = leaves (Node ('x', Node ('a', Empty, Empty),
                           Node ('x', Node ('b', Empty, Empty), Empty)))
             = [Node ('a', Empty, Empty); Node ('b', Empty, Empty)]
;;

let internals tree =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node (_, Empty, Empty) -> acc
    | Node (v, l, r) -> aux (aux (v :: acc) l) r
  in
  aux [] tree
;;

let%test _ = internals (Node ('x', Node ('a', Empty, Empty),
                              Node ('x', Node ('b', Empty, Empty), Empty)))
             = ['x'; 'x']
;;
