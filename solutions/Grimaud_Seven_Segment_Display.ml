let p_A e_3 e_2 e_1 e_0 = e_3 || e_1 || (not e_2 && not e_0) || (e_2 && e_0)
let p_B e_3 e_2 e_1 e_0 = not e_2 || (not e_1 && not e_0) || (e_1 && e_0)
let p_C e_3 e_2 e_1 e_0 = e_2 || not e_1 || e_0
let p_D e_3 e_2 e_1 e_0 = e_3 || (e_1 && not e_0) || (not e_2 && e_1) || (e_2 && not e_1 && e_0) || (not e_2 && not e_0)
let p_E e_3 e_2 e_1 e_0 = (e_1 && not e_0) || (not e_2 && not e_0)
let p_F e_3 e_2 e_1 e_0 = e_3 || (e_2 && not e_1) || (not e_1 && not e_0) || (e_2 && not e_0)
let p_G e_3 e_2 e_1 e_0 = e_3 || (e_2 && not e_1) || (e_1 && not e_0) || (not e_2 && e_1)


let truth_table_A () =
  Printf.printf "Truth table for p_A.\n";
  for i=0 to 9 do
    let e_0 = (i mod 2) = 1 in
    let e_1 = ((i lsr 1) mod 2) = 1 in
    let e_2 = ((i lsr 2) mod 2) = 1 in
    let e_3 = ((i lsr 3) mod 2) = 1 in
    Printf.printf "%B\n" (p_A e_3 e_2 e_1 e_0)
  done;
  Printf.printf "\n"

let truth_table p =
  Printf.printf "Truth table for a given proposition.\n";
  for i=0 to 9 do
    let e_0 = (i mod 2) = 1 in
    let e_1 = ((i lsr 1) mod 2) = 1 in
    let e_2 = ((i lsr 2) mod 2) = 1 in
    let e_3 = ((i lsr 3) mod 2) = 1 in
    Printf.printf "%B\n" (p e_3 e_2 e_1 e_0)
  done;
  Printf.printf "\n"

let truth_tables ()=
  Printf.printf "Truth tables.\n\n";
  Printf.printf "e_3\t e_2\t e_1\t e_0\t p_A\t p_B\t p_C\t p_D\t p_E\t p_F\t p_G\n";
  for i=0 to 9 do
    let e_0 = (i mod 2) = 1 in
    let e_1 = ((i lsr 1) mod 2) = 1 in
    let e_2 = ((i lsr 2) mod 2) = 1 in
    let e_3 = ((i lsr 3) mod 2) = 1 in
    Printf.printf "%B\t %B\t %B\t %B\t %B\t %B\t %B\t %B\t %B\t %B\t %B \n" e_3 e_2 e_1 e_0 (p_A e_3 e_2 e_1 e_0) (p_B e_3 e_2 e_1 e_0) (p_C e_3 e_2 e_1 e_0) (p_D e_3 e_2 e_1 e_0) (p_E e_3 e_2 e_1 e_0) (p_F e_3 e_2 e_1 e_0) (p_G e_3 e_2 e_1 e_0)
  done;
  Printf.printf "\n"

let print_dec n =
  if (n < 10) && (n >= 0) then
    let e_0 = (n mod 2) = 1 in
    let e_1 = ((n lsr 1) mod 2) = 1 in
    let e_2 = ((n lsr 2) mod 2) = 1 in
    let e_3 = ((n lsr 3) mod 2) = 1 in
    if (p_A e_3 e_2 e_1 e_0) then
      Printf.printf " - \n"
    else
      Printf.printf "   \n";
    if (p_F e_3 e_2 e_1 e_0) then
      Printf.printf "| "
    else
      Printf.printf "  ";
    if (p_B e_3 e_2 e_1 e_0) then
      Printf.printf "|\n"
    else
      Printf.printf " \n";
    if (p_G e_3 e_2 e_1 e_0) then
      Printf.printf " - \n"
    else
      Printf.printf "   \n";
    if (p_E e_3 e_2 e_1 e_0) then
      Printf.printf "| "
    else
      Printf.printf "  ";
    if (p_C e_3 e_2 e_1 e_0) then
      Printf.printf "|\n"
    else
      Printf.printf " \n";
    if (p_D e_3 e_2 e_1 e_0) then
      Printf.printf " - \n"
    else
      Printf.printf "   \n";
  else Printf.eprintf "Wrong number value.\n"

let ()=
  truth_table_A ();
  truth_table p_A;
  truth_tables ();
  print_dec 0;
  print_dec 1;
  print_dec 2;
  print_dec 3;
  print_dec 4;
  print_dec 5;
  print_dec 6;
  print_dec 7;
  print_dec 8;
  print_dec 9

