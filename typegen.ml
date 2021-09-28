open Types
open Util

type ts_type =
  | TsBool
  | TsNumber
  | TsString
  | TsNull
  | TsUndefined
  | TsArrow of {
      param : string option;
      param_optional : bool;
      param_ty : ts_type;
      ret_ty : ts_type;
    }
  | TsArray of ts_type
  | TsTup of ts_type list
  | TsRecord of ts_record_field list
  | TsTyApp of ts_type * ts_type list
  | TsUnion of ts_type list

and ts_record_field = { name : string; readonly : bool; ty : ts_type }

(** Determines whether an array type is "complex". A type is complex if we
    prefer to emit [ Array<T> ] over [ T[] ]. *)
let rec is_array_complex_type = function
  | TsNumber | TsString | TsBool | TsNull | TsUndefined -> false
  | TsArrow _ | TsRecord _ | TsTyApp _ -> true
  | TsArray t -> is_array_complex_type t (* inhereted *)
  | TsTup ts | TsUnion ts -> List.exists is_array_complex_type ts

let is_array_parened_type = function
  | TsNumber | TsString | TsBool | TsNull | TsUndefined | TsRecord _ | TsTyApp _
  | TsArray _ | TsTup _ ->
      false
  | TsArrow _ | TsUnion _ -> true

let get_jsoo_name = function
  | "Js_of_ocaml.Js.t" -> `JsType
  | "Js_of_ocaml.Js.js_string" -> `String
  | "Js_of_ocaml.Js.number" -> `Number
  | "bool" -> `Bool
  | "Js_of_ocaml.Js.js_array" -> `Array
  | "Js_of_ocaml.Js.readonly_prop" -> `ReadonlyProp
  | "Js_of_ocaml.Js.opt" -> `PossiblyNull
  | n -> `Name n

let rec tygen t =
  let rec go ~set_readonly_prop t =
    let go' = go ~set_readonly_prop in
    let tygen_poly' = tygen_tapp ~set_readonly_prop in
    match t.desc with
    (* [Tvar (Some "a")] ==> ['a] or ['_a] *)
    | Tvar (Some _) -> failwith "cannot translate type variables"
    (* [Tvar None]       ==> [_] *)
    | Tvar None -> failwith "cannot translate unbound type variables"
    (* [Tarrow (Nolabel,      e1, e2, c)] ==> [e1    -> e2] *)
    (* [Tarrow (Labelled "l", e1, e2, c)] ==> [l:e1  -> e2] *)
    (* [Tarrow (Optional "l", e1, e2, c)] ==> [?l:e1 -> e2] *)
    | Tarrow (lab, e1, e2, _) ->
        let param, param_optional =
          match lab with
          | Nolabel -> (None, false)
          | Labelled l -> (Some l, false)
          | Optional l -> (Some l, true)
        in
        TsArrow { param; param_optional; param_ty = go' e1; ret_ty = go' e2 }
    (* [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)
    | Ttuple ts -> TsTup (List.map go' ts)
    (* [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t] *)
    | Tconstr (t, tps, _) -> tygen_poly' (Path.name t) (List.map tygen tps)
    (* [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
       f1, fn are represented as a linked list of types using Tfield and Tnil
       constructors.

       [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
       where A.ct is the type of some class.

       There are also special cases for so-called "class-types", cf. [Typeclass]
       and [Ctype.set_object_name]:

         [Tobject (Tfield(_,_,...(Tfield(_,_,rv)...),
                          Some(`A.#ct`, [rv;t1;...;tn])]
              ==> [(t1, ..., tn) #A.ct]
         [Tobject (_, Some(`A.#ct`, [Tnil;t1;...;tn])] ==> [(t1, ..., tn) A.ct]

       where [rv] is the hidden row variable. *)
    | Tobject (t1, t2) -> (
        match !t2 with
        | Some (t, tps) -> tygen_poly' (Path.name t) (List.map tygen tps)
        | None ->
            let rec walk_fields all t =
              match t.desc with
              (* [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)
              | Tfield (f, _, t, rst) ->
                  let readonly_prop = ref false in
                  let set_readonly_prop () = readonly_prop := true in
                  let ty = go ~set_readonly_prop t in
                  let f = { name = f; readonly = !readonly_prop; ty } in
                  walk_fields (f :: all) rst
              (* [Tnil] ==> [<...; >] *)
              | Tnil -> all
              | _ -> failwith "unexpected type in record"
            in
            let fields = walk_fields [] t1 |> List.rev in
            TsRecord fields)
    | Tfield _ | Tnil -> failwith "unexpected field/nil outside record"
    (* Indirection *)
    | Tlink t | Tsubst t -> tygen t
    | Tvariant _ -> failwith "cannot translate polymorphic variants"
    | Tunivar _ -> failwith "cannot translate universal quantifiers"
    (* [Tpoly (ty,tyl)] ==> ['a1... 'an. ty] *)
    | Tpoly (t, []) -> tygen t
    | Tpoly (t, tps) -> TsTyApp (tygen t, List.map tygen tps)
    | Tpackage _ -> failwith "cannot translate packages"
  in
  let noop () = () in
  go ~set_readonly_prop:noop t

and tygen_tapp ~set_readonly_prop name typarams =
  let name = get_jsoo_name name in
  match (name, typarams) with
  | `JsType, [ t ] -> t
  | `Bool, [] -> TsBool
  | `Number, [] -> TsNumber
  | `String, [] -> TsString
  | `Array, [ t ] -> TsArray t
  | `ReadonlyProp, [ t ] ->
      set_readonly_prop ();
      t
  | `PossiblyNull, [ t ] -> TsUnion [ t; TsNull ]
  | `Name name, [] -> failwith ("cannot translate raw name " ^ name)
  | `Name name, _ -> failwith ("cannot translate raw polymorphic " ^ name)
  | _ -> failwith "malformed type"

let pp_ts_type f =
  let open Format in
  let rec go = function
    | TsNumber -> pp_print_string f "number"
    | TsString -> pp_print_string f "string"
    | TsBool -> pp_print_string f "boolean"
    | TsNull -> pp_print_string f "null"
    | TsUndefined -> pp_print_string f "undefined"
    | TsArrow { param; param_optional; param_ty; ret_ty } ->
        let param = Option.value param ~default:"_" in
        let opt = if param_optional then "?" else "" in
        fprintf f "@[<hov 2>(%s%s: " param opt;
        go param_ty;
        fprintf f ") =>@ ";
        go ret_ty;
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
