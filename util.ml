let default_width = 80

let with_formatter cb =
  let open Format in
  let b = Buffer.create 32 in
  let f = formatter_of_buffer b in
  pp_set_margin f default_width;
  cb f;
  pp_print_flush f ();
  Buffer.to_seq b |> String.of_seq

let pp_list f printer sep lst =
  let lasti = List.length lst - 1 in
  List.iteri
    (fun i item ->
      printer item;
      if i <> lasti then Format.fprintf f sep)
    lst
