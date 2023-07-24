type 'a mult_tree = T of 'a * 'a mult_tree list;;

let example = T ('a', [T ('f', [T ('g', [])]); T ('c', []);
                       T ('b', [T ('d', []); T ('e', [])])])
;;

let string_of_tree (tree : char mult_tree) : string =
  let rec aux tree =
    let T (v, ts) = tree in
    (v :: (List.flatten (List.map (fun el -> aux el) ts))) @ ['^']
  in
  aux tree
  |> List.fold_left (fun acc el -> Printf.sprintf "%s%c" acc el) ""
;;

string_of_tree example
;;

assert (string_of_tree example = "afg^^c^bd^e^^^")
;;

let char_list_of_string string =
  string
  |> String.to_seq
  |> List.of_seq

(* let rec tree_of_string (string : string) : char mult_tree = *)
