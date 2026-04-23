export type BitXor<
  B extends string
, C extends string> =
  B extends `${infer BH extends '0' | '1'}${infer BR}`
    ? C extends `${infer CH extends '0' | '1'}${infer CR}`
      ? CH extends BH
        ? `0${BitXor<BR, CR>}`
      : `1${BitXor<BR, CR>}`
    : ''
  : ''
