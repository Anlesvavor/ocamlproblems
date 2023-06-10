
let rec last l =
  match l with
  | [] -> None
  | [x] -> Some x
  | head :: tail -> last tail
