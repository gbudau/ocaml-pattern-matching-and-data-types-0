let sequence n =
  if n <= 0 then ""
  else
    let rec reverse accumulator = function
      | [] -> accumulator
      | head::tail -> reverse (head :: accumulator) tail
    in

    let encode elements =
      let rec aux accumulator count l =
      match l with
      | [] -> reverse [] accumulator
      | [x] -> reverse [] (x :: count :: accumulator)
      | x::y::tail when x = y ->
            aux accumulator (count + 1) (y :: tail)
      | x::y::tail ->
            aux (x :: count :: accumulator) 1 (y :: tail)
      in
      aux [] 1 elements
    in

    let rec sequence_aux accumulator i =
      if i = n then accumulator
      else sequence_aux (encode accumulator) (i + 1)
    in

    let rec int_list_to_string accumulator list =
        match list with
        | [] -> accumulator
        | e::l -> int_list_to_string (accumulator ^ string_of_int e) l
    in

    int_list_to_string "" (sequence_aux [1] 1)

let () =
  print_endline (sequence 1);
  print_endline (sequence 2);
  print_endline (sequence 3);
  print_endline (sequence 4);
  print_endline (sequence 5);
  print_endline (sequence 6);
  print_endline (sequence 7);
  print_endline (sequence 8);
  print_endline (sequence 9);
  print_endline (sequence 10);
()
