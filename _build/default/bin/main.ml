let rec subset a b =
  if a == b then true
  else
    match a with
    | [] -> true
    | h :: t -> if List.mem h b then subset t b else false

let equal_sets a b =
  let rec aux a b =
    match a with
    | [] -> if a == [] && b == [] then true else false
    | h :: t ->
        if List.mem h b then aux t (List.filter (fun x -> x != h) b) else false
  in
  aux a b

let set_union a b = a @ b

(* subset test cases *)
let subset_test0 = subset [] [ 1; 2; 3 ]
let subset_test1 = subset [ 3; 1; 3 ] [ 1; 2; 3 ]
let subset_test2 = not (subset [ 1; 3; 7 ] [ 4; 1; 3 ])

(* equal_sets test cases *)
let equal_sets_test0 = equal_sets [ 1; 3 ] [ 3; 1; 3 ]
let equal_sets_test1 = not (equal_sets [ 1; 3; 4 ] [ 3; 1; 3 ])

(* set_union test cases *)
let set_union_test0 = equal_sets (set_union [] [ 1; 2; 3 ]) [ 1; 2; 3 ]
let set_union_test1 = equal_sets (set_union [ 3; 1; 3 ] [ 1; 2; 3 ]) [ 1; 2; 3 ]
let set_union_test2 = equal_sets (set_union [] []) []

let test =
  print_endline (string_of_bool subset_test0);
  print_endline (string_of_bool subset_test1);
  print_endline (string_of_bool subset_test2);
  print_endline (string_of_bool equal_sets_test0);
  print_endline (string_of_bool equal_sets_test1);
  print_endline (string_of_bool set_union_test0);
  print_endline (string_of_bool set_union_test1);
  print_endline (string_of_bool set_union_test2)

let () = test
