type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec string_of_tree tree =
  match tree with
  | Empty -> ""
  | Node (value, Empty, Empty) -> Printf.sprintf "%c" value
  | Node (value, l, r) ->
    Printf.sprintf "%c(%s,%s)" value (string_of_tree l) (string_of_tree r)
;;

let is_lower letter =
  let code = int_of_char letter in
  (code >= (int_of_char 'a')) && (code <= (int_of_char 'z'))
;;

let tree_of_string (tree_string : string) =
  let rec aux = function
    | ',' :: xs -> (Empty, xs)
    | x :: ')' :: xs when (is_lower x) -> (Node (x, Empty, Empty), xs)
    | x :: ',' :: xs when (is_lower x) -> (Node (x, Empty, Empty), xs)
    | x :: '(' :: xs ->
      let (l, xs') = aux xs in
      let (r, xs'') = aux xs' in
      (Node (x, l, r), xs'')
    | _ -> (Empty, [])
  in
  aux (List.of_seq (String.to_seq tree_string))
;;

let leaf x = Node (x, Empty, Empty);;

let example_layout_tree =
  (Node ('a', Node ('b', leaf 'd', leaf 'e'),
         Node ('c', Empty, Node ('f', leaf 'g', Empty))));;

string_of_tree example_layout_tree;;

tree_of_string "a(b(d,e),c(,f(g,)))";;
