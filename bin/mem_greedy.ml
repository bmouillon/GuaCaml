let ui_score (pt, nt) (p_u_mask, n_u_mask, p_i_mask, n_i_mask) p n = (* Score of a given set *)
  let p_u_score = Z.popcount (Z.logand p_u_mask pt) in
  let p_i_score = Z.popcount (Z.logand p_i_mask (Z.lognot pt)) in
  let n_u_score = Z.popcount (Z.logand n_u_mask nt) in
  let n_i_score = Z.popcount (Z.logand n_i_mask (Z.lognot nt)) in
  if p = 1 && p_i_score = 1 then (0, 0) (* Always keep the unique positive *)
  else if n = 1 && n_u_score = 1 then (0, 0) (* Never take the unique negative *)
  else (p_u_score - n_u_score, n_i_score - p_i_score)

(* Set of sets with label, size and score *)

module SetDesc = struct
	type t = (Z.t * Z.t) * string * int * int
	let compare (_, label1, sz1, sc1) (_, label2, sz2, sc2) =
    match compare sc1 sc2 with
    | 0 -> if compare sz2 sz1 <> 0 then compare sz2 sz1 else compare label2 label1
    | c -> c
end

module S = Set.Make(SetDesc)

let add_shrink elt set lim =
  if S.cardinal set < lim then S.add elt set
  else if S.cardinal set = lim then S.remove (S.min_elt set) (S.add elt set)
  else failwith "Set too large !"

let best_sets s_set (pmask, nmask) p n forms lim = (* Choose set and op with the best score *)
  let add_positive (s, form, total_size, score) masks t (label, size) set =
    let u_sc, i_sc = ui_score t masks p n in
    if i_sc > 0 && u_sc > 0 then
			add_shrink (Cmn.inter s t, "(" ^ form ^ " /\\ " ^ label ^ ")", size + total_size + 1, i_sc + score)
      (add_shrink (Cmn.union s t, "(" ^ form ^ " \\/ " ^ label ^ ")", size + total_size + 1, u_sc + score) set lim) lim
		else if i_sc > 0 then
			add_shrink (Cmn.inter s t, "(" ^ form ^ " /\\ " ^ label ^ ")", size + total_size + 1, i_sc + score) set lim
    else if u_sc > 0 then
			add_shrink (Cmn.union s t, "(" ^ form ^ " \\/ " ^ label ^ ")", size + total_size + 1, u_sc + score) set lim
		else
			set in
  let add_positives ((ps, ns), constr, total_size, total_score) set =
    let p_u_mask = Z.logand pmask (Z.lognot ps) in
    let p_i_mask = Z.logand pmask ps in
    let n_u_mask = Z.logand nmask (Z.lognot ns) in
    let n_i_mask = Z.logand nmask ns in
    Hashtbl.fold (add_positive ((ps, ns), constr, total_size, total_score) (p_u_mask, n_u_mask, p_i_mask, n_i_mask)) forms set in
  S.fold add_positives s_set S.empty

let mem_greedy (pmask, nmask) p n forms lim =
  let set = (* Initialize the set of good formulas *)
    let add_to_set (ps, ns) (label, size) set =
      let p_score = Z.popcount (Z.logand ps pmask) in
      let n_score = Z.popcount (Z.logand ns nmask) in
      if p_score = 0 || n_score = n then set (* Do not add a useless set *)
      else add_shrink ((ps, ns), label, size, p_score - n_score) set lim in
    Hashtbl.fold add_to_set forms S.empty in
  let sol = ref Cmn.empty in
  let form = ref "" in
  let size = ref (-1) in
  let keep_best (s, label, total_size, total_score) =
    if total_score = p && (total_size < !size || !size = -1) then begin
      sol := s; form := label; size := total_size end in
  let rec loop_until_empty set =
    S.iter keep_best set;
    let new_set = best_sets set (pmask, nmask) p n forms lim in
    if new_set <> S.empty then loop_until_empty new_set in
  loop_until_empty set;
  !sol, !form, !size

exception MemoryGreedy

let mem_algo n_p n_n forms lim =
  let rec dc_aux pmask nmask =
    let p, n = Z.popcount pmask, Z.popcount nmask in
    let (ps, ns), form, size = mem_greedy (pmask, nmask) p n forms lim in
    if form <> "" then (ps, ns), form, size
    else if p = 1 || n = 1 then raise MemoryGreedy (* If it fails here there is no solution *)
    else if p >= n then begin (* Cut through positives *)
      let pmask1, pmask2 = Cmn.random_cut_mask pmask n_p p in
      (* Try solving left then right, and right then left *)
      let ((ps11, ns11), form11, size11) = dc_aux pmask1 nmask in
      let ((ps22, ns22), form22, size22) = dc_aux pmask2 nmask in
      (* Compute the other side mask using the result of the computed side *)
      let pmask1 = Z.logand pmask1 (Z.lognot ps22) in
      let pmask2 = Z.logand pmask2 (Z.lognot ps11) in
      (* If the computed side solves the entire problem, stop here *)
      if pmask1 = Z.zero && pmask2 = Z.zero then
        if size11 <= size22 then (ps11, ns11), form11, size11 else (ps22, ns22), form22, size22
      else if pmask1 = Z.zero then (ps22, ns22), form22, size22
      else if pmask2 = Z.zero then (ps11, ns11), form11, size11
      else
        (* Solve the other side *)
        let ((ps12, ns12), form12, size12) = dc_aux pmask2 nmask in
        let ((ps21, ns21), form21, size21) = dc_aux pmask1 nmask in
        (* Keep the shortest formula *)
        if size11 + size12 <= size21 + size22 then
          Cmn.union (ps11, ns11) (ps12, ns12), form11 ^ " \\/ " ^ form12, size11 + size12 + 1
        else
          Cmn.union (ps21, ns21) (ps22, ns22), form21 ^ " \\/ " ^ form22, size21 + size22 + 1
    end else begin (* Cut through negatives *)
      let nmask1, nmask2 = Cmn.random_cut_mask nmask n_n n in
      let ((ps11, ns11), form11, size11) = dc_aux pmask nmask1 in
      let ((ps22, ns22), form22, size22) = dc_aux pmask nmask2 in
      let nmask1 = Z.logand nmask1 ns22 in
      let nmask2 = Z.logand nmask2 ns11 in
      if nmask1 = Z.zero && nmask2 = Z.zero then
        if size11 <= size22 then (ps11, ns11), form11, size11 else (ps22, ns22), form22, size22
      else if nmask1 = Z.zero then (ps22, ns22), form22, size22
      else if nmask2 = Z.zero then (ps11, ns11), form11, size11
      else
        let ((ps12, ns12), form12, size12) = dc_aux pmask nmask2 in
        let ((ps21, ns21), form21, size21) = dc_aux pmask nmask1 in
        if size11 + size12 <= size21 + size22 then
          Cmn.inter (ps11, ns11) (ps12, ns12), form11 ^ " /\\ " ^ form12, size11 + size12 + 1
        else
          Cmn.inter (ps21, ns21) (ps22, ns22), form21 ^ " /\\ " ^ form22, size21 + size22 + 1 end in
  let pmask = Cmn.init_mask n_p in
  let nmask = Cmn.init_mask n_n in
  let set, form, size = dc_aux pmask nmask in
  if set <> (pmask, Z.zero) then failwith "Error : Invalid solution"; (* Check if the solution is valid *)
  form, size