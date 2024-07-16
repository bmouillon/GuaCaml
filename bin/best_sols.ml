let write_best_sol filename ext_list =
  let get_size ext =
    let ic = open_in (filename ^ ext) in
    let first_line = input_line ic in
    close_in ic;
    let len = String.length first_line in
    let c = ref len in
    while first_line.[!c - 1] <> ' ' do decr c done;
  int_of_string (String.sub first_line !c (len - !c)) in
  let keep_shortest (best_ext, best_name, best_size) (ext, name) =
    try
      let size = get_size ext in
      if best_size = -1 || size < best_size then
        (ext, name, size)
      else
        (best_ext, best_name, best_size)
    with Sys_error _ -> (best_ext, best_name, best_size) in
  let (best_ext, best_name, best_size) = List.fold_left keep_shortest ("", "", -1) ext_list in
  if best_ext <> ".bsol" && best_ext <> "" then
    let _ = Sys.command ("cp " ^ filename ^ best_ext ^ " " ^ filename ^ ".bsol") in
    best_name, best_size
  else
    ("", -1)