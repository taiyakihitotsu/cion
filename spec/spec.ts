import Cion, { BuiltinsUnion } from "../src/index.js";
import type { Equal } from "../src/util.js";
import { getter_specs } from "./getter.js"
import { setter_specs } from './setter.js'
import { collection_specs } from './collection.js'
import { predicate_specs } from './predicate.js'
import { math_specs } from './math.js'
import { logic_specs } from './logic.js'
import { string_specs } from './string.js'
import { fmap_specs } from './fmap.js'
import { macro_specs } from './macro.js'

type Docs<U extends string> = {
  readonly [K in U]: {
    readonly usage: readonly { readonly pre: string; readonly post: string }[];
    readonly doc: string;
  };
};

const specs = {
  ...getter_specs,
  ...setter_specs,
  ...collection_specs,
  ...predicate_specs,
  ...math_specs,
  ...logic_specs,
  ...string_specs,
  ...fmap_specs,
  ...macro_specs,
} as const;

const specs_check = <T>(
  specs: T extends (T extends Docs<BuiltinsUnion> ? T : never) ? T : never,
) => specs;

const _specs = specs_check(specs);

type Dec<N extends number> = [-1, 0, 1, 2, 3, 4, 5, 6][N];

type _SpecCheckForLocal<
  S extends Docs<BuiltinsUnion>,
  K extends BuiltinsUnion,
  N extends number,
> = K extends keyof S
  ? S[K]["usage"] extends infer Usage
    ? `${N}` extends keyof Usage
      ? Usage[`${N}`] extends infer S extends { pre: string; post: string }
        ? Equal<Cion.Lisp<S["pre"]>, Cion.Lisp<S["post"]>>
        : K
      : K
    : K
  : K;

type SpecCheckForKey<
  S extends Docs<BuiltinsUnion>,
  K extends keyof S & BuiltinsUnion,
  I extends number = Dec<S[K]["usage"]["length"]>,
  R extends boolean = false,
> = Equal<I, -1> extends true
  ? R
  : Equal<true, _SpecCheckForLocal<S, K, I>> extends true
    ? SpecCheckForKey<S, K, Dec<I>, true>
    : false;

type SpecCheck<
  S extends Docs<BuiltinsUnion>,
  K extends keyof S & BuiltinsUnion,
> = K extends any ? SpecCheckForKey<S, K> : never;

const scltest0: true = {} as _SpecCheckForLocal<typeof specs, "some->>", 0>
const scltest1: true = {} as _SpecCheckForLocal<typeof specs, "some->>", 1>

// [note]
// @ts-expect-error
const scktest0: true = {} as CCCCC<typeof specs, "if" | "let">

// [main]
// Type Check for spec.
const sctest0: true = {} as Equal<true, SpecCheck<typeof specs, BuiltinsUnion>>
