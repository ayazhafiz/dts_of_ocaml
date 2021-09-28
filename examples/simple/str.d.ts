/** Like `String.substring`, but on the JSOO side */
export declare const substring:
  (str: string) =>
    (start: number) =>
      (length: number) => { result: string|null, error: string|null }

/** Magical strings */
export declare const magicStrings: string[]
