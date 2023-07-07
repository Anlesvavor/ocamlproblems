type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let example_layout_tree =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                              Node ('h', Node ('g', leaf 'e', Empty), Empty)),
                   leaf 'm'),
        Node ('u', Node ('p', Empty, Node ('s', leaf 'q', Empty)), Empty))
;;

let rec tree_size tree =
  match tree with
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, l, r) -> (tree_size l) + (tree_size r) + 1
;;

let layout_binary_tree_1 tree =
  let rec aux parent_x depth tree =
    match tree with
    | Empty -> Empty
    | Node (value, l, r) ->
      let x = 1 + parent_x + tree_size l in
      let left_node = aux parent_x (succ depth) l in
      let right_node = aux x (succ depth) r in
      let y = depth in (* Clarity? *)
      Node ((value, x, y), left_node, right_node)
  in
  aux 0 1 tree
;;

layout_binary_tree_1 example_layout_tree;;

let max_depth tree =
  let rec aux curr tree =
    match tree with
    | Empty -> curr
    | Node (_, Empty, Empty) -> curr
    | Node (_, l, r) ->
      let left_max = aux (succ curr) l in
      let right_max = aux (succ curr) r in
      max left_max right_max
  in
  (aux 0 tree)
;;

max_depth (Node ('a', Empty, Empty));;
max_depth (Node ('a', Empty, (Node ('b', Empty, Empty))));;
max_depth (Node ('a', (Node ('b', Empty, Empty)), (Node ('b', Empty, Empty))));;
max_depth (Node ('a', (Node ('b', (Node ('c', Empty, Empty)), Empty)), (Node ('b', Empty, Empty))));;

max_depth example_layout_tree;;

let compute_separation depth =
  let int_pow (n : int) (p : int) : int =
    Float.pow (float_of_int n) (float_of_int p)
    |> int_of_float
  in
  int_pow 2 depth
;;

compute_separation 2;;

let rec left_most_node tree =
  match tree with
  | Empty -> Empty
  | Node (_, Empty, _) -> tree
  | Node (_, l, _) -> left_most_node l
;;

let left_length tree =
  let rec aux depth tree =
    match tree with
    | Empty -> compute_separation depth
    | Node (_, l, _) -> aux (succ depth) l
  in
  aux 0 tree
;;

let layout_binary_tree_2 tree =
  let max_depth = max_depth tree in
  let separation depth = compute_separation depth in
  let rec aux is_right parent_x depth tree =
    match tree with
    | Empty -> Empty
    | Node (value, l, r) ->
      let left_x = parent_x - (separation (max_depth - (succ depth))) in
      let right_x = parent_x + (separation (max_depth - (succ depth))) in
      let left_node = aux false left_x (succ depth) l in
      let right_node = aux true right_x (succ depth) r in
      let x = parent_x in
      let y = succ depth in
      Node ((value, x, y), left_node, right_node)
  in
  let root_node_x = (left_length tree) -1 in
  aux true root_node_x 0 tree
;;

let example_layout_tree_2 =
  let leaf x = Node (x, Empty, Empty) in
  Node ('n', Node ('k', Node ('c', leaf 'a',
                              Node ('e', leaf 'd', leaf 'g')),
                   leaf 'm'),
        Node ('u', Node ('p', Empty, leaf 'q'), Empty))
;;

left_length example_layout_tree_2;;

max_depth example_layout_tree_2;;

compute_separation 4;;

layout_binary_tree_2 example_layout_tree_2;;
