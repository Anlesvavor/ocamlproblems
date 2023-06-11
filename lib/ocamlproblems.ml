
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

let flatten_2 list =
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

let rec compress_2 = function
  | a :: (b :: _ as t) -> if a = b then compress_2 t else a :: compress_2 t
  | smaller -> smaller

let%test _ = compress_2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
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

let pack_2 list =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
      if a = b
      then aux (a :: current) acc t
      else aux [] ((a :: current) :: acc) t
  in
  List.rev (aux [] [] list);;

let%test _ = pack_2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
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


let encode_2 list =
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

encode_2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
;;

let%test _ = encode_2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
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

let decode_2 list =
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

decode_2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
;;

let%test _ = decode_2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
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

