# dts_of_ocaml

`dts_of_ocaml` generates TypeScript declarations of exports from OCaml sources
used with `js_of_ocaml`. For example, supposing `js.ml` consists of

```ocaml
open Js_of_ocaml

let ok s =
  object%js
    val result = Js.(some @@ string s)

    val error = Js.null
  end

let _ =
  Js.export "substring"
    (fun
      [@jsdoc {|Like `String.substring`, but on the JSOO side|}]
      ~str
      ~start
      ~length
    ->
      let s = Js.to_string str in
      let start = Js.float_of_number start |> int_of_float in
      let length = Js.float_of_number length |> int_of_float in
      String.sub s start length |> ok)
```

Generating `js.cmt` for `js.ml` and then running `dts_of_ocaml js.cmt` will
yield

```typescript
/** Like `String.substring`, but on the JSOO side */
export declare const substring:
  (str: string) =>
    (start: number) =>
      (length: number) => { result: string|null, error: string|null }
```

## Why not patch JSOO?

A natural question is why not just expose this as an additional option of JSOO.
It's a good question - the reason is that there is not a good place to do so.

- One option is to have TypeScript declarations be emitted as part of the JSOO
    library, but then one must hook into the OCaml compiler in order to get the
    type information of exported symbols.
- Having a ppx pass to emit declarations is not viable either because
    typechecking does not occur before ppx preprocessors are called. Calling the
    typechecker is duplicating work that happens anyway and cumbersome to
    configure correctly for the end user.
- Emitting declarations during JS compilation by `js_of_ocaml` is not viable
    because by the input to the compilation is bytecode, making it very
    difficult (if not impossible) to resolve types in a friendly way, and making
    it nearly impossible to discover exports. Not to say that `dts_of_ocaml`'s
    export discovery is perfect either, but it is better.
