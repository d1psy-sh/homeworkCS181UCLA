let rec _printi list =
  match list with
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      _printi t

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

let set_union a b =
  let rec aux a b a_wo_b =
    match a with
    | [] -> a_wo_b
    | h :: t -> (
        match List.mem h b with
        | true -> aux t b a_wo_b
        | false -> aux t b (h :: a_wo_b))
  in
  let a_wo_b = aux a b [] in
  a_wo_b @ b

let set_intersection a b =
  (* build the intersection of two sets*)
  let rec aux a b acc =
    match a with
    | [] -> acc
    | h :: t -> (
        match List.mem h b with
        | true -> if List.mem h acc then aux t b acc else aux t b (h :: acc)
        | false -> aux t b acc)
  in
  aux a b []

let set_diff a b =
  if List.length a < List.length b then []
  else
    let rec aux a b acc =
      match a with
      | [] -> acc
      | h :: t -> (
          match List.mem h b with
          | true -> aux t b acc
          | false -> aux t b (h :: acc))
    in
    aux a b []

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

(* set_intersection test cases *)
let set_intersection_test0 = equal_sets (set_intersection [] [ 1; 2; 3 ]) []

let set_intersection_test1 =
  equal_sets (set_intersection [ 3; 1; 3 ] [ 1; 2; 3 ]) [ 1; 3 ]

let set_intersection_test2 =
  equal_sets (set_intersection [ 1; 2; 3; 4 ] [ 3; 1; 2; 4 ]) [ 4; 3; 2; 1 ]

let set_replicate list times =
  let n_times element times =
    let rec aux element times acc =
      match times with 0 -> acc | _ -> aux element (times - 1) (element :: acc)
    in
    aux element times []
  in
  let rec aux list acc =
    match list with [] -> acc | h :: t -> aux t (acc @ n_times h times)
  in
  aux list []

let drop list nth =
  match nth with
  | 0 -> []
  | 1 -> []
  | _ ->
      let rec aux list counter acc =
        match list with
        | [] -> acc
        | h :: t ->
            if counter mod nth == 0 then aux t (counter + 1) acc
            else aux t (counter + 1) (acc @ [ h ])
      in
      aux list 1 []

(* set_diff test cases *)
let set_diff_test0 = equal_sets (set_diff [ 1; 3 ] [ 1; 4; 3; 1 ]) []
let set_diff_test1 = equal_sets (set_diff [ 4; 3; 1; 1; 3 ] [ 1; 3 ]) [ 4 ]
let set_diff_test2 = equal_sets (set_diff [ 4; 3; 1 ] []) [ 1; 3; 4 ]
let set_diff_test3 = equal_sets (set_diff [] [ 4; 3; 1 ]) []

(* computed fixed point test cases *)
let set_replicate_test0 = set_replicate [ 1 ] 3 = [ 1; 1; 1 ]
let set_replicate_test1 = set_replicate [ 1; 2 ] 3 = [ 1; 1; 1; 2; 2; 2 ]
let set_replicate_test2 = set_replicate [] 3 = []
let set_replicate_test3 = set_replicate [ 1; 2 ] 0 = []
let set_replicate_test4 = set_replicate [ 1; 2 ] 1 = [ 1; 2 ]

(* drop from list *)
let list_drop_test0 = drop [ 1; 2; 3 ] 0 = []

let list_drop_test1 =
  drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = [ "a"; "b"; "d"; "e"; "g"; "h"; "j" ]

let list_drop_test2 = drop [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] 1 = []

let list_drop_test3 =
  drop [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] 2 = [ 1; 3; 5; 7; 9 ]

let test =
  print_endline (string_of_bool subset_test0);
  print_endline (string_of_bool subset_test1);
  print_endline (string_of_bool subset_test2);
  print_endline (string_of_bool equal_sets_test0);
  print_endline (string_of_bool equal_sets_test1);
  print_endline (string_of_bool set_union_test0);
  print_endline (string_of_bool set_union_test1);
  print_endline (string_of_bool set_union_test2);
  print_endline (string_of_bool set_intersection_test0);
  print_endline (string_of_bool set_intersection_test1);
  print_endline (string_of_bool set_intersection_test2);
  print_endline (string_of_bool set_diff_test0);
  print_endline (string_of_bool set_diff_test1);
  print_endline (string_of_bool set_diff_test2);
  print_endline (string_of_bool set_diff_test3);
  print_endline (string_of_bool set_replicate_test0);
  print_endline (string_of_bool set_replicate_test1);
  print_endline (string_of_bool set_replicate_test2);
  print_endline (string_of_bool set_replicate_test3);
  print_endline (string_of_bool set_replicate_test4);
  print_endline (string_of_bool list_drop_test0);
  print_endline (string_of_bool list_drop_test1);
  print_endline (string_of_bool list_drop_test2);
  print_endline (string_of_bool list_drop_test3)

let () = test
