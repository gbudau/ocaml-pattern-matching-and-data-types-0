type phosphate = string

type deoxyribose = string

type nucleobase =
 | A
 | T
 | C
 | G
 | None

type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase;
}

type helix = nucleotide list

let generate_helix (n: int) : helix =
  if n <= 0 then []
  else
    let random_nucleotide () =
      match Random.int 4 with
      | 1 -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A}
      | 2 -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T}
      | 3 -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C}
      | _ -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G}
    in

    let rec generate_helix_aux accumulator i =
      if i = n then accumulator
      else generate_helix_aux (random_nucleotide () :: accumulator) (i + 1)
    in
    generate_helix_aux [] 0

let helix_to_string (h: helix) : string =
  let nucleobase_to_string n = match n with
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> ""
in
  let rec helix_to_string_aux accumulator l =
    match l with
    | [] -> accumulator
    | head::tail -> helix_to_string_aux (accumulator ^ (nucleobase_to_string head.nucleobase)) tail
  in
  helix_to_string_aux "" h

let complementary_helix (h: helix) : helix =
  let complementary_nucleotide n =
    match n.nucleobase with
    | A -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T}
    | T -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A}
    | C -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G}
    | G -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C}
    | _ -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None}
  in

  let rec reverse accumulator = function
    | [] -> accumulator
    | head::tail -> reverse (head :: accumulator) tail
  in

  let rec complementary_helix_aux accumulator l =
    match l with
    | [] -> reverse [] accumulator
    | head::tail -> complementary_helix_aux ((complementary_nucleotide head) :: accumulator) tail
  in complementary_helix_aux [] h

let () =
Random.self_init ();
print_endline (helix_to_string (generate_helix 1));
print_endline (helix_to_string (generate_helix 2));
print_endline (helix_to_string (generate_helix 3));
print_endline (helix_to_string (generate_helix 4));
print_endline (helix_to_string (generate_helix 5));
print_endline (helix_to_string (generate_helix 6));
print_endline (helix_to_string (generate_helix 7));
print_endline (helix_to_string (generate_helix 8));
print_endline (helix_to_string (generate_helix 9));
print_endline (helix_to_string (generate_helix 10));
let h = generate_helix 42 in
print_endline (helix_to_string h);
print_endline (helix_to_string (complementary_helix h));
()
