(* Read a BSC problem from a file *)

let read filename = (* Read formulas from .bsc file *)
	let ic = open_in filename in
	let target = input_line ic in (* First line of file is the target *)
	let formulas = ref [] in
  let seen = Hashtbl.create 1 in
	begin try
    let b = ref true in
		while !b do
			let formula = input_line ic in
      if formula = "" then b := false else formulas := formula :: !formulas
		done;
    while true do
      match String.split_on_char ',' (input_line ic) with
        | [ps; ns] -> Hashtbl.add seen (Z.of_string ps, Z.of_string ns) ()
        | _ -> failwith "Error : read"
    done
	with End_of_file -> close_in ic end;
	target, !formulas, seen

let read_target target = (* Warning : Must be of the form 1..10..0 *)
	let len = String.length target in
	let c = ref 0 in
	while target.[!c] = '1' do incr c done;
	!c, len - !c

let read_formulas formulas = (* Formulas are stocked in a hash table *)
  let split formula = (* Split the formula in name, positives and negatives *)
    match String.split_on_char ',' formula with
      | [label; size; dens; ps; ns] -> label, int_of_string size, int_of_string dens, (Z.of_string ps, Z.of_string ns)
      | _ -> failwith "Error : read" in
  List.rev_map split formulas

let get filename =
  try
    let target, formulas, seen = read ((String.sub filename 0 (String.length filename - 3)) ^ "rbsc") in
    let n_p, n_n = read_target target in
    let forms_list = read_formulas formulas in
    let domin_list = List.rev_map (fun (_, _, dens, s) -> (s, dens)) forms_list in
    let form_array = Array.init 100 (fun _ -> Hashtbl.create 1) in
    let forms = Hashtbl.create 1 in
    let add_form (label, size, _, s) = Hashtbl.add form_array.(size) s label; Hashtbl.add forms s (label, size) in
    List.iter add_form forms_list;
    ((n_p, n_n), forms, form_array, seen, domin_list)
  with Sys_error _ -> Read_reduce.get filename