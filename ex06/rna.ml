type phosphate = string

type deoxyribose = string

type nucleobase =
 | A
 | T
 | C
 | G
 | U
 | None

type nucleotide = {
  phosphate: phosphate;
  deoxyribose: deoxyribose;
  nucleobase: nucleobase;
}

type helix = nucleotide list

type rna = nucleobase list

let make_nucleotide base = { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = base }
let nucleobase_to_string n = match n with
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | _ -> ""

let generate_helix (n: int) : helix =
  if n <= 0 then []
  else
    let random_nucleotide () =
      match Random.int 4 with
      | 0 -> make_nucleotide A
      | 1 -> make_nucleotide T
      | 2 -> make_nucleotide C
      | _ -> make_nucleotide G
    in

    let rec generate_helix_aux accumulator n_left =
      if n_left <= 0 then accumulator
      else generate_helix_aux (random_nucleotide () :: accumulator) (n_left - 1)
    in
    generate_helix_aux [] n

let helix_to_string (h: helix) : string =
  let rec helix_to_string_aux accumulator l =
    match l with
    | [] -> accumulator
    | head::tail -> helix_to_string_aux (accumulator ^ (nucleobase_to_string head.nucleobase)) tail
  in
  helix_to_string_aux "" h

let complementary_helix (h: helix) : helix =
  let complementary_nucleotide n =
    match n.nucleobase with
    | A -> make_nucleotide T
    | T -> make_nucleotide A
    | C -> make_nucleotide G
    | G -> make_nucleotide C
    | _ -> make_nucleotide None
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

let generate_rna (h: helix) : rna =

  let rna_base_from_dna_base base =
    match base with
    | A -> U
    | T -> A
    | C -> G
    | G -> C
    | _ -> None
  in

  let rec reverse accumulator = function
    | [] -> accumulator
    | head::tail -> reverse (head :: accumulator) tail
  in

  let rec generate_rna_aux accumulator l =
    match l with
    | [] -> reverse [] accumulator
    | head::tail -> generate_rna_aux ((rna_base_from_dna_base head.nucleobase) :: accumulator) tail
  in
    generate_rna_aux [] h

let () =
Random.self_init ();
let h = generate_helix 42 in
print_endline (helix_to_string h);
print_endline (helix_to_string (complementary_helix h));

let rna_to_string (r: rna) : string =
  let rec rna_to_string_aux accumulator l =
    match l with
    | [] -> accumulator
    | head::tail -> rna_to_string_aux (accumulator ^ (nucleobase_to_string head)) tail
  in
  rna_to_string_aux "" r
in
print_endline (rna_to_string (generate_rna h));
()
