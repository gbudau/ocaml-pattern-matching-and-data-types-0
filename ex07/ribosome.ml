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

type aminoacid =
 | Stop
 | Ala
 | Arg
 | Asn
 | Asp
 | Cys
 | Gln
 | Glu
 | Gly
 | His
 | Ile
 | Leu
 | Lys
 | Met
 | Phe
 | Pro
 | Ser
 | Thr
 | Trp
 | Tyr
 | Val

type protein = aminoacid list

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

let rec reverse_list accumulator = function
  | [] -> accumulator
  | head::tail -> reverse_list (head :: accumulator) tail

let complementary_helix (h: helix) : helix =
  let complementary_nucleotide n =
    match n.nucleobase with
    | A -> make_nucleotide T
    | T -> make_nucleotide A
    | C -> make_nucleotide G
    | G -> make_nucleotide C
    | _ -> make_nucleotide None
  in

  let rec complementary_helix_aux accumulator l =
    match l with
    | [] -> reverse_list [] accumulator
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


  let rec generate_rna_aux accumulator l =
    match l with
    | [] -> reverse_list [] accumulator
    | head::tail -> generate_rna_aux ((rna_base_from_dna_base head.nucleobase) :: accumulator) tail
  in
    generate_rna_aux [] h

let generate_bases_triplets (r: rna) : (nucleobase * nucleobase * nucleobase) list =
  let rec generate_bases_triplets_aux accumulator l =
    match l with
    | [] -> reverse_list [] accumulator
    | first::second::third::tail -> generate_bases_triplets_aux ((first, second, third) :: accumulator) tail
    | _ -> reverse_list [] accumulator
  in
  generate_bases_triplets_aux [] r

let string_of_aminoacid (a: aminoacid) : string =
  match a with
  | Stop -> "Stop"
  | Ala -> "Ala"
  | Arg -> "Arg"
  | Asn -> "Asn"
  | Asp -> "Asp"
  | Cys -> "Cys"
  | Gln -> "Gln"
  | Glu -> "Glu"
  | Gly -> "Gly"
  | His -> "His"
  | Ile -> "Ile"
  | Leu -> "Leu"
  | Lys -> "Lys"
  | Met -> "Met"
  | Phe -> "Phe"
  | Pro -> "Pro"
  | Ser -> "Ser"
  | Thr -> "Thr"
  | Trp -> "Trp"
  | Tyr -> "Tyr"
  | Val -> "Val"

let string_of_protein (p: protein) : string =
  let rec aux accumulator l =
    match l with
    | [] -> accumulator
    | head::tail ->
        let separator = if accumulator = "" then "" else "-" in
        aux (accumulator ^ separator ^ (string_of_aminoacid head)) tail
  in
    aux "" p

let decode_arn (r: rna) : protein =
  let translate_triplet triplet =
    match triplet with
    | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
    | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
    | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
    | (A, A, C) | (A, A, U) -> Asn
    | (G, A, C) | (G, A, U) -> Asp
    | (U, G, C) | (U, G, U) -> Cys
    | (C, A, A) | (C, A, G) -> Gln
    | (G, A, A) | (G, A, G) -> Glu
    | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
    | (C, A, C) | (C, A, U) -> His
    | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
    | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
    | (A, A, A) | (A, A, G) -> Lys
    | (A, U, G) -> Met
    | (U, U, C) | (U, U, U) -> Phe
    | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
    | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
    | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
    | (U, G, G) -> Trp
    | (U, A, C) | (U, A, U) -> Tyr
    | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
    | _ -> Stop
  in
  let rec aux accumulator l =
    match l with
    | [] -> reverse_list [] accumulator
    | head::tail -> match translate_triplet head with
                    | Stop -> reverse_list [] accumulator
                    | amino -> aux (amino :: accumulator) tail
  in
  aux [] (generate_bases_triplets r)

let () =
  (* Multiple of 3 with no Stop *)
  (* AUG (Met) -> GCC (Ala) -> UUU (Phe) *)
  let rna_normal : rna = [A; U; G; G; C; C; U; U; U] in
  print_endline "Met-Ala-Phe";
  print_endline (string_of_protein (decode_arn rna_normal));
  print_endline "";

  (* Early Stop *)
  (* AUG (Met) -> UAA (Stop) -> GCC (Ala should be ignored) *)
  let rna_stop : rna = [A; U; G; U; A; A; G; C; C] in
  print_endline "Met";
  print_endline (string_of_protein (decode_arn rna_stop));
  print_endline "";

  (* Incomplete triplet at the end *)
  (* AUG (Met) -> GCC (Ala) -> U (Ignored because it's not a full triplet) *)
  let rna_incomplete : rna = [A; U; G; G; C; C; U] in
  print_endline "Met-Ala";
  print_endline (string_of_protein (decode_arn rna_incomplete));
  print_endline "";

  (* Empty RNA *)
  let rna_empty : rna = [] in
  print_endline "";
  print_endline (string_of_protein (decode_arn rna_empty));
  print_endline "";

  (* Random generation *)
  Random.self_init ();
  print_endline "(Random Helix -> RNA -> Protein):";

  let random_h = generate_helix 30 in (* 30 bases = 10 possible triplets *)
  let random_r = generate_rna random_h in

  let base_triplets_to_string (t: ((nucleobase * nucleobase * nucleobase) list)): string =
    let triplet_to_string (first, second, third) =
       (nucleobase_to_string first) ^ (nucleobase_to_string second) ^ (nucleobase_to_string third)
    in

    let rec base_triplets_to_string_aux accumulator l =
      match l with
      | [] -> accumulator
      | head::tail -> base_triplets_to_string_aux (accumulator ^ "(" ^ (triplet_to_string head) ^ ")") tail
    in
    base_triplets_to_string_aux "" t
  in
  print_endline (base_triplets_to_string (generate_bases_triplets random_r)); 
  print_endline (string_of_protein (decode_arn random_r));
()
