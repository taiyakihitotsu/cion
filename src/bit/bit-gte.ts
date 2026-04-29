import type { BitSub } from './bit-sub.js'
import type { BitFill } from './bit-fill.js'

export type BitGTE<
  B extends string
, C extends string> =
  [BitFill<B>, BitFill<C>] extends [infer FilledB extends string, infer FilledC extends string]
    ? [FilledB, FilledC] extends [`${infer BS}${infer _}`, `${infer CS}${infer _}`]
      ? BS extends CS
	? BitSub<FilledB, FilledC> extends `${infer H}${infer _}`
	  ? H extends '1'
	    ? false
	  : true
	: never
      : BS extends '0'
	? true
      : false
    : never
  : never
