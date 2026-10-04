let gray n =
  if n < 0 then print_endline ""
  else if n = 0 then print_endline "0"
  else
    let rec print_list list =
      match list with
      | [] -> print_newline ()
      | [x] -> print_endline x
      | e::l -> print_string e ; print_char ' ' ; print_list l
  in
    let rec map_prepend prefix list =
      match list with
      | [] -> []
      | head :: tail -> (prefix ^ head) :: map_prepend prefix tail
  in
    let rec reverse_map_prepend prefix list accumulator =
    match list with
    | [] -> accumulator
    | head::tail -> reverse_map_prepend prefix tail ((prefix ^ head) :: accumulator)
  in
    let rec concat first second =
      match first with
      | [] -> second
      | head :: tail -> head :: concat tail second
  in
    let rec gray_aux n =
    if n = 1 then ["0"; "1"]
    else
      concat
        (map_prepend "0" (gray_aux (n - 1)))
        (reverse_map_prepend "1" (gray_aux (n - 1)) [])
  in
  print_list (gray_aux n)

let () =
  print_endline "";
  gray (-1); (* Should print a newline *)
  print_endline "---";
  print_endline "0";
  gray 0; (* Should print 0 *)
  print_endline "---";
  print_endline "0 1";
  gray 1; (* Should print 0 1 *)
  print_endline "---";
  print_endline "00 01 11 10";
  gray 2; (* 00 01 11 10 *)
  print_endline "---";
  print_endline "000 001 011 010 110 111 101 100";
  gray 3; (* Should print 000 001 011 010 110 111 101 100 *)
  print_endline "---";
  print_endline "0000 0001 0011 0010 0110 0111 0101 0100 1100 1101 1111 1110 1010 1011 1001 1000";
  gray 4; (* Should print 0000 0001 0011 0010 0110 0111 0101 0100 1100 1101 1111 1110 1010 1011 1001 1000 *)
()
