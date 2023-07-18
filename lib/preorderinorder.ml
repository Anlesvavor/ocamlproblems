
type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  (Node ('a', Node ('b', leaf 'd', leaf 'e'),
         Node ('c', Empty, Node ('f', leaf 'g', Empty))))
;;

let rec preorder tree =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node (v, l, r) ->
      aux (aux (v :: acc) l) r
  in
  aux [] tree
  |> List.fold_left (fun acc x -> String.make 1 x ^ acc) ""
;;

preorder example_layout_tree;;

let rec inorder tree =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node (v, l, r) ->
      let acc = v :: aux acc l in
      let acc = aux acc r in
      acc
  in
  aux [] tree
  |> List.fold_left (fun acc x -> String.make 1 x ^ acc) ""
;;

inorder example_layout_tree;;

(* abdecfg *)
(* dbeacgf *)

let split_l_r_at at list =
  let rec aux acc list =
    match list with
    | [] -> (acc, [])
    | x :: xs ->
      if x = at
      then (acc, xs)
      else aux (x :: acc) xs
  in
  aux [] list
;;

split_l_r_at 'a' ['d';'b';'e';'a';'c';'g';'f'];;

let rec contains el = function
  | [] -> false
  | x :: xs -> if x = el
    then true
    else contains el xs
;;

let pre_in_tree (i : string) (p : string) : (char binary_tree) =
  let (p : char list) =
    String.to_seq p
    |> Seq.fold_left (fun acc x -> x :: acc) []
    |> List.rev
  in
  let (i : char list) =
    String.to_seq i
    |> Seq.fold_left (fun acc x -> x :: acc) []
    |> List.rev
  in
  let rec aux (i : char list) (p : char list) : (char binary_tree) =
    match i with
    | [] -> Empty
    | [y] -> Node (y, Empty, Empty)
    | _ :: _ ->
      match p with
      | [] -> Empty
      | x :: xs ->
        if contains x i
        then
          let (li, ri) = split_l_r_at x i in
          Node (x, aux li xs, aux ri xs)
        else
          aux i xs
  in
  aux i p
;;



pre_in_tree "dbeacgf" "abdecfg"
;;

preorder example_layout_tree
;;

inorder example_layout_tree
;;

(pre_in_tree (inorder example_layout_tree) (preorder example_layout_tree))
= example_layout_tree
;;
