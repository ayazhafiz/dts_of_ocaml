open Parsetree
open Typedtree

let jsoo_export_call = "Js_of_ocaml.Js.export"

type texp_apply = { head : expression; args : expression option list }

type jsoo_export = {
  name : string;
  ty : Types.type_expr;
  shape : Typedtree.expression_desc;
  jsdoc : string option;
}

let collect_apply_of_exp exp =
  let rec go all exp =
    match exp.exp_desc with
    | Texp_ident _ -> all
    | Texp_constant _ -> all
    | Texp_let (_, _, e) -> go all e
    | Texp_function _ -> all
    | Texp_apply (e, es) -> { head = e; args = List.map snd es } :: all
    | Texp_match (e, _, _) -> go all e
    | Texp_try (e, _) -> go all e
    | Texp_tuple es -> List.fold_left go all es
    | Texp_construct (_, _, es) -> List.fold_left go all es
    | Texp_variant (_, Some e) -> go all e
    | Texp_variant (_, None) -> all
    | Texp_record { extended_expression = Some e; _ } -> go all e
    | Texp_record _ -> all
    | Texp_field (e, _, _) -> go all e
    | Texp_setfield (e1, _, _, e2) -> List.fold_left go all [ e1; e2 ]
    | Texp_array es -> List.fold_left go all es
    | Texp_ifthenelse (e1, e2, Some e3) -> List.fold_left go all [ e1; e2; e3 ]
    | Texp_ifthenelse (e1, e2, None) -> List.fold_left go all [ e1; e2 ]
    | Texp_sequence (e1, e2) -> List.fold_left go all [ e1; e2 ]
    | Texp_while (e1, e2) -> List.fold_left go all [ e1; e2 ]
    | Texp_for (_, _, e1, e2, _, e3) -> List.fold_left go all [ e1; e2; e3 ]
    | Texp_send (e, _) -> go all e
    | Texp_new _ -> all
    | Texp_instvar _ -> all
    | Texp_setinstvar (_, _, _, e) -> go all e
    | Texp_override (_, es) ->
        List.fold_left go all (List.map (fun (_, _, e) -> e) es)
    | Texp_letmodule (_, _, _, _, e) -> go all e
    | Texp_letexception (_, e) -> go all e
    | Texp_assert e -> go all e
    | Texp_lazy e -> go all e
    | Texp_object _ -> all
    | Texp_pack _ -> all
    | Texp_letop _ -> all
    | Texp_unreachable -> all
    | Texp_extension_constructor _ -> all
    | Texp_open (_, e) -> go all e
  in
  go [] exp |> List.rev

let collect_apply_of_str_item str_item =
  let go = collect_apply_of_exp in
  match str_item.str_desc with
  | Tstr_eval (e, _) -> go e
  | Tstr_value (_, vbs) ->
      List.concat_map (fun { vb_expr; _ } -> go vb_expr) vbs
  | Tstr_primitive _ -> []
  | Tstr_type _ -> []
  | Tstr_typext _ -> []
  | Tstr_exception _ -> []
  | Tstr_module _ -> []
  | Tstr_recmodule _ -> []
  | Tstr_modtype _ -> []
  | Tstr_open _ -> []
  | Tstr_class _ -> []
  | Tstr_class_type _ -> []
  | Tstr_include _ -> []
  | Tstr_attribute _ -> []

let collect_apply_of_str { str_items; _ } =
  List.concat_map collect_apply_of_str_item str_items

let get_jsdoc_of_annotation { attr_name; attr_payload; _ } =
  match (attr_name.txt, attr_payload) with
  | ( "jsdoc",
      PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ( { pexp_desc = Pexp_constant (Pconst_string (jsdoc, _, _)); _ },
                  _ );
            _;
          };
        ] ) ->
      Some jsdoc
  | _ -> None

let get_export_of_texp_apply { head; args } =
  match (head.exp_desc, args) with
  | ( Texp_ident (head, _, _),
      [
        Some { exp_desc = Texp_constant (Const_string (export_name, _, _)); _ };
        Some export_expr;
      ] )
    when Path.name head = jsoo_export_call ->
      let jsdoc =
        export_expr.exp_attributes |> List.filter_map get_jsdoc_of_annotation
      in
      let jsdoc = List.nth_opt jsdoc 0 in
      Some
        {
          name = export_name;
          ty = export_expr.exp_type;
          shape = export_expr.exp_desc;
          jsdoc;
        }
  | _ -> None
