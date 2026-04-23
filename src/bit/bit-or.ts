export type BitOr<
  B extends string
, C extends string> =
  B extends `${infer BH}${infer BR}`
    ? C extends `${infer CH}${infer CR}`
      ? CH extends `1`
        ? `1${BitOr<BR, CR>}`
      : BH extends `1`
        ? `1${BitOr<BR, CR>}`
      : `0${BitOr<BR, CR>}`
    : ``
  : ``
