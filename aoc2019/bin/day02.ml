let input = Advent.read_lines "static/day02.txt" ","

let array_helper f x y z array =
  let x' = array.(x) in
  let y' = array.(y) in
  let z' = array.(z) in
  let f' =
    f (int_of_string array.(int_of_string x')) (int_of_string array.(int_of_string y'))
  in
  array.(int_of_string z') <- string_of_int f'
;;

(** Takes two array indexes and a position.  It then adds the  two elems and overwrites the elem in the position *)
let add_array_elements x y z array = array_helper ( + ) x y z array

(** Takes two array indexes and a position.  It then multiplies the two elems and overwrites the elem in the position *)
let multiply_array_elements x y z array = array_helper ( * ) x y z array

(* Pt. 1 *)
let _ =
  let lines_array = Array.of_list input in
  let i = ref 0 in
  let f index _ =
    if !i mod 4 = 0 then
      if lines_array.(index) = "1" then (
        let () = incr i in
        add_array_elements (index + 1) (index + 2) (index + 3) lines_array
      ) else if lines_array.(index) = "2" then (
        let () = incr i in
        multiply_array_elements (index + 1) (index + 2) (index + 3) lines_array
      ) else
        incr i
    else
      incr i
  in
  Array.iteri f lines_array;
  Printf.printf "PT 1: %s\n" lines_array.(0)
;;

(* Pt. 2 *)
let set_noun_verb x y array =
   array.(1) <- string_of_int x;
   array.(2) <- string_of_int y
   ;;

let check_results array =
    if array.(0) = "19690720" then
        let x = array.(1) in let y = array.(2) in
        fun () -> Printf.printf "PT 2: %i\n" (100 * (int_of_string x) + (int_of_string y))
    else fun () -> ()
   ;;

let check_array lines_array =
  let i = ref 0 in
  let continue = ref true in
  let f index _ =
    if !i mod 4 = 0 && !continue then
      if lines_array.(index) = "99" then
        continue := false
      else if lines_array.(index) = "1" then (
        let () = incr i in
        add_array_elements (index + 1) (index + 2) (index + 3) lines_array
      ) else if lines_array.(index) = "2" then (
        let () = incr i in
        multiply_array_elements (index + 1) (index + 2) (index + 3) lines_array
      ) else
        incr i
    else
      incr i
  in
  Array.iteri f lines_array;
  check_results lines_array
;;


let rec check_noun i j =
    let array = Array.of_list input in
    if (i > 99 && j > 99) then
        failwith "DAY 02 COMPLETE...There has to be a better way"
    else if (j > 99) then
         check_noun (i + 1) 0 array
      else
         set_noun_verb i j array;
         check_array array ();
         let () = check_noun i (j + 1) array;
    in
    check_noun 0 0;
;;

let () = check_noun 0 0 [||]
