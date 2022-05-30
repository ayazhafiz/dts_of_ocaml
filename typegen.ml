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
  | TsFun of (string option * bool * ts_type) list * ts_type
  | TsArray of ts_type
  | TsTup of ts_type list
  | TsRecord of ts_record_field list
  | TsTyApp of ts_type * ts_type list
  | TsUnion of ts_type list

and ts_record_field = { name : string; readonly : bool; ty : ts_type }

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
  let rec go ~set_readonly_prop desc =
    let go' t = go ~set_readonly_prop (get_desc t) in
    let tygen_poly' = tygen_tapp ~set_readonly_prop in
    match desc with
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
    | Tconstr (t, tps, _) -> tygen_poly' (Path.name t) (List.map go' tps)
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
        | Some (t, tps) -> tygen_poly' (Path.name t) (List.map go' tps)
        | None ->
            let rec walk_fields all t =
              match get_desc t with
              (* [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)
              | Tfield (f, _, t, rst) ->
                  let readonly_prop = ref false in
                  let set_readonly_prop () = readonly_prop := true in
                  let ty = go ~set_readonly_prop (get_desc t) in
                  let f = { name = f; readonly = !readonly_prop; ty } in
                  walk_fields (f :: all) rst
              (* [Tnil] ==> [<...; >] *)
              | Tnil -> all
              | Tlink t | Tsubst (t, _) -> walk_fields all t
              | _ ->
                  failwith
                    (sprintf "unexpected type (%s) %s in record" (typkind t)
                       (printtyp t))
            in
            let fields = walk_fields [] t1 |> List.rev in
            TsRecord fields)
    | Tfield _ | Tnil -> failwith "unexpected field/nil outside record"
    (* Indirection *)
    | Tlink t | Tsubst (t, _) -> go' t
    | Tvariant _ -> failwith "cannot translate polymorphic variants"
    | Tunivar _ -> failwith "cannot translate universal quantifiers"
    (* [Tpoly (ty,tyl)] ==> ['a1... 'an. ty] *)
    | Tpoly (t, []) -> go' t
    | Tpoly (t, tps) -> TsTyApp (go' t, List.map go' tps)
    | Tpackage _ -> failwith "cannot translate packages"
  in
  let noop () = () in
  go ~set_readonly_prop:noop (get_desc t)

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

let rec linearize_arrows t =
  match t with
  | TsBool | TsNumber | TsString | TsNull | TsUndefined -> t
  | TsArrow _ ->
      let rec walk = function
        | TsArrow { param; param_optional; param_ty; ret_ty } ->
            let params, ret = walk ret_ty in
            ((param, param_optional, param_ty) :: params, ret)
        | fin -> ([], fin)
      in
      let params, ret = walk t in
      TsFun (params, ret)
  | TsFun (params, ret) ->
      TsFun
        ( List.map (fun (p, o, t) -> (p, o, linearize_arrows t)) params,
          linearize_arrows ret )
  | TsArray t -> TsArray (linearize_arrows t)
  | TsTup ts -> TsTup (List.map linearize_arrows ts)
  | TsRecord fs ->
      TsRecord (List.map (fun f -> { f with ty = linearize_arrows f.ty }) fs)
  | TsTyApp (t, ts) -> TsTyApp (linearize_arrows t, List.map linearize_arrows ts)
  | TsUnion ts -> TsUnion (List.map linearize_arrows ts)
