(* Read a BSC problem from a file *)

let read filename = (* Read formulas from .bsc file *)
	let ic = open_in filename in
	let target = input_line ic in (* First line of file is the target *)
	let formulas = ref [] in
	begin try
		while true do
			let formula = input_line ic in
			formulas := formula :: !formulas
		done
	with End_of_file -> close_in ic end;
	target, !formulas

let read_target target = (* Warning : Must be of the form 1..10..0 *)
	let len = String.length target in
	let c = ref 0 in
	while target.[!c] = '1' do incr c done;
	!c, len - !c

(* Array structure to check domination efficiently *)

let is_dominated_by (ps, ns) (pt, nt) = Z.logor ps pt = pt && Z.logor ns nt = ns

let insert form_array seen domin_list (s, label, size, dens) =
	if Hashtbl.mem seen s then domin_list
	else begin
		Hashtbl.add seen s ();
		let rec insert_aux = function
			| [] -> Hashtbl.add form_array.(size) s label; [(s, dens)]
			| (t, d) :: q ->
				if dens > d then begin Hashtbl.add form_array.(size) s label; (s, dens) :: (t, d) :: q
				end else if is_dominated_by s t then (t, d) :: q
				else (t, d) :: insert_aux q in
		insert_aux domin_list end

(* Return a hash table with formulas of the BSC file *)

let read_formulas formulas n_p n_n = (* Formulas are stocked in a hash table *)
	let formula_size label = (* Size of a formula *)
		let size = ref 0 in
		for i = 0 to (String.length label - 1) do
			match label.[i] with
				| ' ' | '(' | ')' -> ()
				| _ -> incr size
		done;
		!size in
	let neg_int = Cmn.init_mask n_n in
	let split formula = (* Split the formula in name, positives and negatives *)
		let c = ref 3 in
		while formula.[!c - 2] <> ',' do incr c done;
		let ps = Z.of_string_base 2 (String.sub formula !c n_p) in
		let ns = Z.of_string_base 2 (String.sub formula (!c + n_p) n_n) in
		let label = String.sub formula 0 (!c - 2) in
		let size = formula_size label in
		let dens = Z.popcount ps - Z.popcount ns in
		if ps <> Z.zero && ns <> neg_int then Some ((ps, ns), label, size, dens) else None in
	let cmp (_, _, size1, dens1) (_, _, size2, dens2) = (* Sort formulas by size, then by sparcity *)
		match compare size1 size2 with
			| 0 -> compare dens2 dens1
			| c -> c in
	List.sort cmp (List.filter_map split formulas)

(* Write the reduction in a file *)

let write filename target forms seen domin_list =
	let oc = open_out ((String.sub filename 0 (String.length filename - 3)) ^ "rbsc") in
	Printf.fprintf oc "%s\n" target;
	let print_set (ps, ns) = Z.output oc ps; Printf.fprintf oc ","; Z.output oc ns; Printf.fprintf oc "\n" in
	let print_form (s, dens) =
		let label, size = Hashtbl.find forms s in
		Printf.fprintf oc "%s,%i,%i," label size dens; print_set s in
	let print_seen s _ = print_set s in
	List.iter print_form domin_list;
	Printf.fprintf oc "\n";
	Hashtbl.iter print_seen seen; close_out oc

(* Return the desired formulas *)

let get filename = (* Return hash table with formulas and number of positives and negatives *)
	let target, formulas = read filename in
	let n_p, n_n = read_target target in
	let form_list = read_formulas formulas n_p n_n in
	let len = List.length form_list in
	let form_array = Array.init 100 (fun _ -> Hashtbl.create len) in
	let seen = Hashtbl.create len in
	let domin_list = List.fold_left (insert form_array seen) [] form_list in
	let forms = Hashtbl.create (List.length domin_list) in
	let add_form size set label = Hashtbl.add forms set (label, size) in
	for i = 0 to 19 do Hashtbl.iter (add_form i) form_array.(i) done;
	write filename target forms seen domin_list;
	((n_p, n_n), forms, form_array, seen, domin_list)