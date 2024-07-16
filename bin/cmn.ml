let two = Z.succ Z.one
let empty = Z.zero, Z.zero
let union (sp, sn) (tp, tn) = (Z.logor sp tp, Z.logor sn tn)
let inter (sp, sn) (tp, tn) = (Z.logand sp tp, Z.logand sn tn)
let init_mask n = Z.sub (Z.pow two n) Z.one

(* Cut a mask in two masks of equal size *)

let rec random_cut_mask mask n k =
  let random_cut = Z.random_bits n in
  let lmask, rmask = (Z.logand mask random_cut, Z.logand mask (Z.lognot random_cut)) in
  if Z.popcount lmask = k / 2 || Z.popcount rmask = k / 2 then lmask, rmask
  else random_cut_mask mask n k

(* Convert a list of operations in a string *)

let write_seq seq =
	let rec cons_seq seq_str = function
		| [] -> seq_str
		| (f, b) :: t ->
			let op = if b = 1 then " \\/ " else " /\\ " in
			cons_seq ("(" ^ seq_str ^ op ^ f ^ ")") t in
	match seq with
	| (f, 1) :: t -> cons_seq f t 
	| _ -> failwith "Error : write_seq"

(* Debug *)

let print_set (ps, ns) n_p n_n =
  let rec aux z c lim =
    if c = lim then
      ()
    else
      let q, r = Z.ediv_rem z two in
      aux q (c + 1) lim; print_int (Z.to_int r) in
  aux ps 0 n_p; print_string " "; aux ns 0 n_n; print_newline ()