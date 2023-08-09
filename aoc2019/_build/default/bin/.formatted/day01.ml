let input = Advent.read_lines "static/day01.txt" "\n"

(* Pt. 1 *)
let get_sum input =
  let rec get_sum_aux acc = function
    | [] -> acc
    | h :: t -> get_sum_aux (acc + (int_of_string h / 3) - 2) t
  in
  get_sum_aux 0 input
;;

(* Pt. 2 *)
let get_compounded_sum input =
  let rec adjust_fuel_requirement m acc' =
    if m > 0 then
      adjust_fuel_requirement ((m / 3) - 2) (m + acc')
    else
      acc'
  in
  let rec get_compounded_fuel_sum acc = function
    | [] -> acc
    | h :: t ->
      get_compounded_fuel_sum
        (acc + adjust_fuel_requirement ((int_of_string h / 3) - 2) 0)
        t
  in
  get_compounded_fuel_sum 0 input
;;

let _ = Printf.printf "PT 1: %i\n" (get_sum input)
let _ = Printf.printf "PT 2: %i\n" (get_compounded_sum input)
