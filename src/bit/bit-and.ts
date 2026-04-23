export type BitAnd<
  B extends string
, C extends string> =
  B extends `${infer BH}${infer BR}`
    ? C extends `${infer CH}${infer CR}`
      ? CH extends `1`
        ? BH extends `1`
          ? `1${BitAnd<BR, CR>}`
        : `0${BitAnd<BR, CR>}`
      : `0${BitAnd<BR, CR>}`
    : ``
  : ``
