
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
  |> List.map (List.rev)
  |> List.rev
;;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
;;

let%test _ = pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
             = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
                ["e"; "e"; "e"; "e"]]
