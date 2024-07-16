(* Randomized hash tables *)

let () = Hashtbl.randomize ()

(* Parse command line *)

let usage_msg = "dune exec -- bin/BSC.exe <file> [-v] options"
let verbose = ref false
let input_file = ref ""
let mg_mem = ref 0
let cb_lim = ref 0
let anon_fun filename = input_file := filename
let speclist =
	[("-v", Arg.Set verbose, "Output debug information");
	 ("-mg", Arg.Set_int mg_mem, "Run greedy algo with k memory");
	 ("-cb", Arg.Set_int cb_lim, "Run comb algo until length k")]
let () = Arg.parse speclist anon_fun usage_msg

(* Extenstions list *)

let ext_list =
	[".bsol", "???";
	 ".mgsol", "greedy algo with memory";
	 ".cbsol", "comb algo";]

(* Read the BSC problem *)

let filename = !input_file
(* let () = print_string filename *)
let () = print_string "Reading the problem..."; flush stdout
let (n_p, n_n), forms, form_array, seen, domin_list = Read.get filename
let () = print_string "done\n"

(* Write the solutions found in a file *)

let gr_mem_write k = (* Solution using the greedy algorithm with memory of size k *)
	print_string "MemoryGreedy..."; flush stdout;
	try
		let seq_str, size = Mem_greedy.mem_algo n_p n_n forms k in
		let oc = open_out (filename ^ ".mgsol") in
		Printf.fprintf oc "Greedy algorithm with memory - Found solution of size %i\n" size;
		Printf.fprintf oc "%s" seq_str;
		close_out oc; print_string "done\n"; flush stdout
	with Mem_greedy.MemoryGreedy -> print_string "FAIL\n"; flush stdout

let cb_write lim = (* Solution using the comb algorithm *)
	print_string "Comb..."; flush stdout;
	try
		let seq_str, size = Comb.comb form_array n_p n_n seen domin_list filename lim in
		let oc = open_out (filename ^ ".cbsol") in
		Printf.fprintf oc "Comb algorithm - Found solution of size %i\n" size;
		Printf.fprintf oc "%s" seq_str;
		close_out oc; print_string "done\n"; flush stdout
	with Comb.Comb -> print_string "FAIL\n"; flush stdout

let write_best () = (* If a new best solution is found, keep it ! *)
	let best_name, best_size = Best_sols.write_best_sol filename ext_list in
	if best_name <> "" then Printf.fprintf stdout "New best solution of size %i using %s !\n" best_size best_name

let () =
	if !mg_mem > 0 then gr_mem_write !mg_mem;
	if !cb_lim > 0 then cb_write !cb_lim;
	write_best ()