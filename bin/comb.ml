(* Array structure to check domination efficiently *)

let is_dominated_by (ps, ns) (pt, nt) = Z.logor ps pt = pt && Z.logor ns nt = ns

let insert form_array seen size dens domin_list (s, label) =
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

(* Next depth for the comb *)

let step form_array n_p n_n size seen domin_list =
	let neg_int = Z.sub (Z.pow (Z.succ Z.one) n_n) Z.one in
	let dens_tbl = Array.make (n_p + n_n + 1) [] in
	let generate_formulas (size1, size2) =
		let first set1 label1 =
			let partial_u, partial_i = Cmn.union set1, Cmn.inter set1 in
			let second set2 label2 =
				let (u_ps, u_ns), (i_ps, i_ns) = partial_u set2, partial_i set2 in
				let u_dens, i_dens = Z.popcount u_ps - Z.popcount u_ns + n_n, Z.popcount i_ps - Z.popcount i_ns + n_n in
				if u_ps <> Z.zero && u_ns <> neg_int then
					dens_tbl.(u_dens) <- ((u_ps, u_ns), "(" ^ label1 ^ ") \\/ (" ^ label2 ^ ")") :: dens_tbl.(u_dens);
				if i_ps <> Z.zero && i_ns <> neg_int then
					dens_tbl.(i_dens) <- ((i_ps, i_ns), "(" ^ label1 ^ ") /\\ (" ^ label2 ^ ")") :: dens_tbl.(i_dens) in
			Hashtbl.iter second form_array.(size2) in
		Hashtbl.iter first form_array.(size1) in
	let rec incr_insert dens domin_list =
		let form_tbl = dens_tbl.(dens) in
		let domin_list = List.fold_left (insert form_array seen size (dens - n_n)) domin_list form_tbl in
		if dens = 0 then domin_list else incr_insert (dens - 1) domin_list in
  List.iter generate_formulas (List.init ((size - 1) / 2) (fun x -> (x + 1, size - x - 2)));
	incr_insert (n_p + n_n) domin_list

exception Comb

let start form_array =
	(* Return the maximum size of given formulas *)
	let size = ref 10 in
	while Hashtbl.length form_array.(!size - 1) = 0 do decr size done;
	!size

let suplim filename lim =
  (* Return the best solution's size for filename *)
  try begin
    let ic = open_in (filename ^ ".bsol") in
    let line = input_line ic in
    let c = ref (String.length line) in
    while line.[!c - 1] <> ' ' do decr c done;
    int_of_string (String.sub line !c (String.length line - !c)) end
  with Sys_error _ -> lim

let comb form_array n_p n_n seen domin_list filename lim =
  let lim = min (suplim filename lim) lim in
  let size = start form_array in
	print_int (List.length domin_list); print_char ' '; flush stdout;
	let rec comb_aux size domin_list =
		if size > lim then raise Comb
		else
			let domin_list = step form_array n_p n_n size seen domin_list in
			print_int (List.length domin_list); print_char ' '; flush stdout;
			let (set, dens) = List.hd domin_list in
			if dens = n_p then
				let label = Hashtbl.find form_array.(size) set in
				if set <> (Cmn.init_mask n_p, Z.zero) then failwith "Error : Invalid solution"
				else label, size
			else comb_aux (size + 1) domin_list in
	comb_aux size domin_list