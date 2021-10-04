(** Pretty printing for TS types. *)

open Typegen
open Util

(** Determines whether an array type is "complex". A type is complex if we
    prefer to emit [ Array<T> ] over [ T[] ]. *)
let rec is_array_complex_type = function
  | TsNumber | TsString | TsBool | TsNull | TsUndefined -> false
  | TsFun _ | TsArrow _ | TsRecord _ | TsTyApp _ -> true
  | TsArray t -> is_array_complex_type t (* inhereted *)
  | TsTup ts | TsUnion ts -> List.exists is_array_complex_type ts

let is_array_parened_type = function
  | TsNumber | TsString | TsBool | TsNull | TsUndefined | TsRecord _ | TsTyApp _
  | TsArray _ | TsTup _ ->
      false
  | TsFun _ | TsArrow _ | TsUnion _ -> true

let pp_ts_type f =
  let open Format in
  let rec go = function
    | TsNumber -> pp_print_string f "number"
    | TsString -> pp_print_string f "string"
    | TsBool -> pp_print_string f "boolean"
    | TsNull -> pp_print_string f "null"
    | TsUndefined -> pp_print_string f "undefined"
    | TsArrow { param; param_optional; param_ty; ret_ty } ->
        go (TsFun ([ (param, param_optional, param_ty) ], ret_ty))
    | TsFun (ps, ret) ->
        fprintf f "@[<hov 2>(@[<hov 0>";
        pp_list f
          (fun (p, opt, ty) ->
            let param = Option.value p ~default:"_" in
            let opt = if opt then "?" else "" in
            fprintf f "@[<hov 2>%s%s:@ " param opt;
            go ty;
            fprintf f "@]")
          ",@ " ps;
        fprintf f "@]) =>@ ";
        go ret;
        fprintf f "@]"
    | TsArray t ->
        if is_array_complex_type t then (
          fprintf f "@[<hov 2>Array<@,";
          go t;
          fprintf f ">@]")
        else if is_array_parened_type t then (
          fprintf f "@[(";
          go t;
          fprintf f ")[]@]")
        else (
          fprintf f "@[";
          go t;
          fprintf f "[]@]")
    | TsTup ts ->
        fprintf f "@[(@[<hov>";
        pp_list f go ",@ " ts;
        fprintf f "@])@]"
    | TsRecord fields ->
        fprintf f "@[{ @[<hov>";
        pp_list f
          (fun { name; readonly; ty } ->
            let readonly = if readonly then "readonly " else "" in
            fprintf f "@[<hov 2>%s%s:@ " readonly name;
            go ty;
            fprintf f "@]")
          ",@ " fields;
        fprintf f "@] }@]"
    | TsTyApp (t, tps) ->
        fprintf f "@[";
        go t;
        fprintf f "<@[<hov>";
        pp_list f go ",@ " tps;
        fprintf f "@]>@]"
    | TsUnion ts ->
        fprintf f "@[";
        pp_list f go "@,|" ts;
        fprintf f "@]"
  in
  go
