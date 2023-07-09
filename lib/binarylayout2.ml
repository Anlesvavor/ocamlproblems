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
  let rec aux parent_x depth tree =
    match tree with
    | Empty -> Empty
    | Node (value, l, r) ->
      let left_x = parent_x - (separation (max_depth - (succ depth))) in
      let right_x = parent_x + (separation (max_depth - (succ depth))) in
      let left_node = aux left_x (succ depth) l in
      let right_node = aux right_x (succ depth) r in
      let x = parent_x in
      let y = succ depth in
      Node ((value, x, y), left_node, right_node)
  in
  let root_node_x = (left_length tree) -1 in
  aux root_node_x 0 tree
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

let coords tree =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node ((_, x, y), l, r) -> aux (aux ((x,y) :: acc) l) r
  in
  aux [] tree
;;

let all_unique (list : _ list) : bool =
  let rec aux list =
    match list with
    | [] -> true
    | x :: xs ->
      if List.exists ((=) x) xs
      then false
      else aux xs
  in
  aux list
;;

let rec apply_until
    ~(predicate : 'a -> bool)
    ~(f : 'b -> 'a)
    ~(arg_update : 'b -> 'b)
    ~(arg : 'b)
  : 'a
  =
  let result = f arg in
  if predicate result
  then result
  else apply_until
      ~predicate
      ~f
      ~arg_update
      ~arg: (arg_update arg)
;;

(* Test *)
apply_until
  ~predicate: ((=) 5)
  ~f: (fun b -> let () = print_int b in b)
  ~arg_update: succ
  ~arg: 0
;;

let min_x tree =
  let rec aux acc tree =
    match tree with
    | Empty -> acc
    | Node ((_, x, _), l, r) ->
      aux (aux (min x acc) r) l
  in
  aux Int.max_int tree
;;

let rec shift_layout amount tree =
  match tree with
  | Empty -> Empty
  | Node ((v, x, y), l, r) ->
    let l = shift_layout amount l in
    let r = shift_layout amount r in
    Node ((v, x + amount, y), l, r)
;;

let layout_binary_tree_3 (tree : (char binary_tree)) : ((char * int * int) binary_tree) =
  let rec aux
      (parent_x : int)
      (separation : int)
      (depth : int)
      (tree : (char binary_tree))
    : ((char * int * int) binary_tree)
    =
    match tree with
    | Empty -> Empty
    | Node (value, l, r) ->
      let left_x = parent_x - separation in
      let right_x = parent_x + separation in
      let left_node = apply_until
          ~predicate: (fun node -> node |> coords |> all_unique)
          ~f: (fun arg -> aux left_x arg (succ depth) l)
          ~arg_update: succ
          ~arg: separation
      in
      let right_node = apply_until
          ~predicate: (fun node -> node |> coords |> all_unique)
          ~f: (fun arg -> aux right_x arg (succ depth) r)
          ~arg_update: succ
          ~arg: separation
      in
      let x = parent_x in
      let y = succ depth in
      Node ((value, x, y), left_node, right_node)
  in
  (* let left_length tree = *)
  (*   let rec aux acc tree = *)
  (*     match tree with *)
  (*     | Empty -> acc *)
  (*     | Node (_, l, _) -> aux (succ acc) l *)
  (*   in *)
  (*   aux 0 tree *)
  (* in *)
  (* let root_node_x = (left_length tree) -1 in *)
  (* let tree = apply_until *)
  (*     ~predicate: (fun node -> node |> coords |> all_unique) *)
  (*     ~f: (fun arg -> aux 0 arg 0 tree) *)
  (*     ~arg_update: succ *)
  (*     ~arg: 1 *)
  (* in *)
  let tree = aux 0 1 0 tree in
  let min = min_x tree in
  shift_layout (((-1) * min) + 1) tree
;;

let leaf x = Node (x, Empty, Empty)
;;

let example_layout_tree =
  Node ('n'
       , Node ('k'
              , Node ('c'
                     , leaf 'a'
                     , Node ('h'
                            , Node ('g'
                                   , leaf 'e'
                                   , Empty
                                   )
                            , Empty
                            )
                     )
              , leaf 'm'
              )
       , Node ('u'
              , Node ('p'
                     , Empty
                     , Node ('s'
                            , leaf 'q'
                            , Empty
                            )
                     )
              , Empty
              )
       )
;;

layout_binary_tree_3 example_layout_tree;;

(layout_binary_tree_3 example_layout_tree)
|> coords
|> all_unique
;;
