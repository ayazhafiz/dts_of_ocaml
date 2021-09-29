open Js_of_ocaml

let ok s =
  object%js
    val result = Js.(some @@ string s)

    val error = Js.null
  end

let err s =
  object%js
    val result = Js.null

    val error = Js.(some @@ string s)
  end

let _ =
  Js.export "substring"
    (fun
      [@jsdoc {|Like `String.substring`, but on the JSOO side|}] ~str
      ~start
      ~length
    ->
      let s = Js.to_string str in
      let start = Js.float_of_number start |> int_of_float in
      let length = Js.float_of_number length |> int_of_float in
      try String.sub s start length |> ok with Invalid_argument e -> err e)

let _ =
  Js.export "magicStrings"
    (Js.array
       (Array.map Js.string [| "zz-magic1"; "proofs-are-pudding" |])
     [@jsdoc "Magical strings"])

let a =
  object%js
    val bar = Js.string "abc"
  end

let _ = Js.export "foo" a
