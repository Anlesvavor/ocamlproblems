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

let tail = function
  | [] -> []
  | _ :: xs -> xs
;;

let tree_of_string (tree_string : string) =
  let peel_parenthesis tree_string =
    tree_string
    |> tail
    |> List.rev
    |> tail
    |> List.rev
  in
  let rec parts depth acc str =
    match str with
    | [] -> (acc, [])
    | c :: cs ->
      match c with
      | '(' -> parts (succ depth) (c :: acc) cs
      | ')' -> parts (pred depth) (c :: acc) cs
      | ',' when (depth = 0) -> (List.rev acc, cs)
      | _ -> parts depth (c :: acc) cs
  in
  let rec aux arr =
    match arr with
    | [] -> Empty
    | x :: xs ->
      let (root_value, arr) = (x, peel_parenthesis xs) in
      let (l_string, r_string) = parts 0 [] arr in
      Node (root_value, aux l_string, aux r_string)
  in
  let arr = List.of_seq (String.to_seq tree_string) in
  aux arr
;;

let leaf x = Node (x, Empty, Empty);;

let example_layout_tree =
  (Node ('a', Node ('b', leaf 'd', leaf 'e'),
         Node ('c', Empty, Node ('f', leaf 'g', Empty))));;

string_of_tree example_layout_tree;;

tree_of_string "a(b(d,e),c(,f(g,)))";;
