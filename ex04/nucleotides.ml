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

let generate_nucleotide c = match c with
  | 'A' -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A}
  | 'T' -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T}
  | 'C' -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C}
  | 'G' -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G}
  | _ -> {phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None}
  
let () = 
 if (generate_nucleotide 'A').nucleobase = A then print_endline "A" else print_endline "None";
 if (generate_nucleotide 'T').nucleobase = T then print_endline "T" else print_endline "None";
 if (generate_nucleotide 'C').nucleobase = C then print_endline "C" else print_endline "None";
 if (generate_nucleotide 'G').nucleobase = G then print_endline "G" else print_endline "None";
 if (generate_nucleotide 'a').nucleobase = None then print_endline "None" else print_endline "Error";
 if (generate_nucleotide 'Z').nucleobase = None then print_endline "None" else print_endline "Error";
()
