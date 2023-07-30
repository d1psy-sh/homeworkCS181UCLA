let big = [ 1; 2; 3; 4; 5 ]
let little = [ 1; 2; 3 ]
let little1 = [ 1; 2; 3; 8 ]
let bigChar = [ "one"; "two"; "three"; "four"; "five" ]
let littleChar = [ "one"; "two"; "three" ]
let littleChar1 = [ "something"; "one"; "two"; "three" ]

let rec subset a b =
  match a with
  | [] -> true
  | h :: t -> if List.mem h b then subset t b else false

let () = 
  print_endline (string_of_bool (subset little big));
  print_endline (string_of_bool (subset little1 big));
  print_endline (string_of_bool (subset littleChar bigChar));
  print_endline (string_of_bool (subset littleChar1 bigChar));
