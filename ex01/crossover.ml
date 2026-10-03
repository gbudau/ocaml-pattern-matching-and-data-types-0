let crossover list1 list2 =
  let rec is_in_list  x list = match list with
    | [] -> false
    | head::tail -> (x = head) || (is_in_list x tail)
in
  let rec reverse_append accumulator = function
  | [] -> accumulator
  | head::tail -> reverse_append (head :: accumulator) tail
in
  let rec crossover_aux accumulator list = match list with
  | [] -> reverse_append [] accumulator
  |  [head] -> if is_in_list head list2 then reverse_append [] (head :: accumulator) else reverse_append [] accumulator
  |  head::tail -> if is_in_list head list2 then crossover_aux (head :: accumulator) tail else crossover_aux accumulator tail
  in 
  crossover_aux [] list1

let () =
let print_crossover_int encoded =
  List.iter
    (fun (value) ->
      Printf.printf "(%d); " value)
    encoded;
  print_newline ()
  in
  print_crossover_int (crossover [] []);
  if crossover [] [] = [] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1] []);
  if crossover [1] [] = [] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [] [1]);
  if crossover [] [1] = [] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1; 2; 3] [1; 2; 3]);
  if crossover [1; 2; 3] [1; 2; 3] = [1; 2; 3] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1; 2; 3] [3; 4; 5]);
  if crossover [1; 2; 3] [3; 4; 5] = [3] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1; 2; 3] [1; 0; -1]);
  if crossover [1; 2; 3] [1; 0; -1] = [1] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1; 2; 3] [0; 2; 4]);
  if crossover [1; 2; 3] [0; 2; 4] = [2] then print_endline "True" else print_endline "False";
  print_crossover_int (crossover [1; 2; 3] [4; 5; 6]);
  if crossover [1; 2; 3] [4; 5; 6] = [] then print_endline "True" else print_endline "False";

let print_crossover_string encoded =
  List.iter
    (fun (value) ->
      Printf.printf "(%s); " value)
    encoded;
  print_newline ()
  in
  print_crossover_string (crossover ["a"; "b"; "c"] ["b"; "c"; "d"]);
  if crossover ["a"; "b"; "c"] ["b"; "c"; "d"] = ["b"; "c"] then print_endline "True" else print_endline "False";
()
