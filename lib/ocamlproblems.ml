
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

let%test _ = lotto_select 10 1
             = [1; 1; 1; 1; 1; 1; 1; 1; 1; 1]

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
let%test _ = coprime 20536 7826 = false;
