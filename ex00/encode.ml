let encode elements =
  let rec reverse_append accumulator = function
  | [] -> accumulator
  | head::tail -> reverse_append (head :: accumulator) tail
in
  let rec aux accumulator count l =
  match l with
  | [] -> reverse_append [] accumulator
  | [x] -> reverse_append [] ((count, x) :: accumulator)
  | x::y::tail ->
      if
        x = y
      then
        aux accumulator (count + 1) (y :: tail)
      else
        aux ((count, x) :: accumulator) 1 (y :: tail)
  in
  aux [] 1 elements

let () = 
let print_encoded_string encoded =
  List.iter
    (fun (count, value) ->
      Printf.printf "(%d, %s); " count value)
    encoded;
  print_newline ()
  in
  print_encoded_string (encode []);
  print_encoded_string ([]);
  if encode [] = [] then print_string "True\n" else print_string "False\n";
  print_encoded_string (encode ["a"]);
  print_encoded_string ([(1, "a")]);
  if encode ["a"] = [(1, "a")] then print_string "True\n" else print_string "False\n";
  print_encoded_string (encode ["a"; "a"]);
  print_encoded_string ([(2, "a")]);
  if encode ["a"; "a"] = [(2, "a")] then print_string "True\n" else print_string "False\n";
  print_encoded_string (encode ["a"; "a"; "a"; "b"; "b"; "b"]);
  print_encoded_string ([(3, "a"); (3, "b")]);
  if encode ["a"; "a"; "a"; "b"; "b"; "b"] = [(3, "a"); (3, "b")] then print_string "True\n" else print_string "False\n";
  print_encoded_string (encode ["a"; "a"; "a"; "b"; "b"; "b"; "a"; "a"; "a"]);
  print_encoded_string ([(3, "a"); (3, "b"); (3, "a")]);
  if encode ["a"; "a"; "a"; "b"; "b"; "b"; "a"; "a"; "a"] = [(3, "a"); (3, "b"); (3, "a")] then print_string "True\n" else print_string "False\n";
  print_encoded_string (encode ["a"; "b"; "c"; "a"; "a"]);
  print_encoded_string ([(1, "a"); (1, "b"); (1, "c"); (2, "a")]);
  if encode ["a"; "b"; "c"; "a"; "a"] = [(1, "a"); (1, "b"); (1, "c"); (2, "a")] then print_string "True\n" else print_string "False\n";

let print_encoded_int encoded =
  List.iter
    (fun (count, value) ->
      Printf.printf "(%d, %d); " count value)
    encoded;
  print_newline ()
    in
  print_encoded_int (encode [1; 2; 3; 1; 1]);
  print_encoded_int ([(1, 1); (1, 2); (1, 3); (2, 1)]);
  if encode [1; 2; 3; 1; 1] = [(1, 1); (1, 2); (1, 3); (2, 1)] then print_string "True\n" else print_string "False\n";
  ()
