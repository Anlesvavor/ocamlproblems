
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
