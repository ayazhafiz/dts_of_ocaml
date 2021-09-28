open Collect
open Typegen
open Util

let pp_jsoo_export f { name; ty; jsdoc } =
  let open Format in
  fprintf f "@[<v 0>";
  Option.iter (fprintf f "@[/** %s */@]@,") jsdoc;
  fprintf f "@[<hov 2>export declare const %s:@ " name;
  tygen ty |> pp_ts_type f;
  fprintf f "@]@,@]"

let pp_all_jsoo_exports f exports =
  let open Format in
  fprintf f "@[<v 0>";
  pp_list f (pp_jsoo_export f) "@," exports;
  fprintf f "@]"

let get_exports str =
  let exports =
    collect_apply_of_str str |> List.filter_map get_export_of_texp_apply
  in
  let f = Format.formatter_of_out_channel stdout in
  pp_all_jsoo_exports f exports;
  Format.pp_print_flush f ()

let process_file filename =
  let cmt = Cmt_format.read_cmt filename in
  match cmt.cmt_annots with
  | Implementation structure -> get_exports structure
  | _ -> failwith "must pass a cmt file of an implementation"

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse []
    (fun s ->
      match !inFile with
      | Some _ -> raise (Arg.Bad "You must specify at most one input file")
      | None -> inFile := Some s)
    "";
  !inFile

let main () =
  match parseArgs () with
  | Some inFile ->
      let _ = process_file inFile in
      ()
  | None -> failwith "no file"

let () = main ()
