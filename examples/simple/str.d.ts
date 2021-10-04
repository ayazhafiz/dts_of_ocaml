/** Like `String.substring`, but on the JSOO side */
export declare const substring:
  (str: string, start: number, length: number) =>
    { readonly result: string|null, readonly error: string|null }

/** Magical strings */
export declare const magicStrings: string[]

export declare const foo: { readonly bar: string }
