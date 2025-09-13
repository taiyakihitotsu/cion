import Cion, { BuiltinsUnion } from "../src/index";
import type { Equal } from "../src/util";

const general_spec = `List is not implemented. Vector is the integrated data for it. So some fns doesn't need such as nth, mapv, etc.

getter functions returns nil if the arg data doesn't have a corresponded element. This is a spec not following Clojure. Because of not to implement try-catch syntax.

Some fns is not able to take any args against the original Clojure's default behavior. all of fmap like map is so. Arithmetic and Logical operations can read any num of args.

if, fn, let doesn't arrow to have empty body.

R\Q isn't implemented. All number is expressed with int, n, or rational, n/m.

Regex doesn't need and cannot append #, not in the way of Clojure. And Regex type doesn't exist.

Deconstruct binding isn't implemented (currently).

There is no loop special syntax. Use let and fn as recursion.

Int range is -32767 to 32767. Rational is the same because it's written as Int/Int.

Division by zero is not allowed, to return nil.



### Following

Both nil and false are only falsy value.

Keyword and map are true by ifn?. You can use them as an accessor ... [todo] research again.

Fns are 1st order (Lisp-1).
`;

type Docs<U extends string> = {
  readonly [K in U]: {
    readonly usage: readonly { readonly pre: string; readonly post: string }[];
    readonly doc: string;
  };
};

const getter_doc = `
### getter
Getter functions for vector / map structure.
They return nil if it can be accessible.
This spec is not following Clojure Spec.
`;

const getter_specs = {
  first: {
    usage: [
      { pre: "(first [0 1 2])", post: "0" },
      { pre: "(first [])", post: "nil" },
    ],
    doc: "1st elem of vec. Nil if empty.",
  },
  second: {
    usage: [
      { pre: "(second [0 1 2])", post: "1" },
      { pre: "(second [])", post: "nil" },
    ],
    doc: "2nd elem of vec. Nil if empty.",
  },
  third: {
    usage: [
      { pre: "(third [0 1 2])", post: "2" },
      { pre: "(third [])", post: "nil" },
    ],
    doc: "3rd elem of vec. Nil if empty.",
  },
  last: {
    usage: [
      { pre: "(last [0 1 2])", post: "2" },
      { pre: "(last [])", post: "nil" },
    ],
    doc: "Last elem of vec. Nil if empty.",
  },
  rest: {
    usage: [
      { pre: "(rest [0 1 2])", post: "[1 2]" },
      { pre: "(rest [0])", post: "[]" },
      { pre: "(rest [])", post: "nil" },
    ],
    doc: "1st elem excluded. Empty if a count is <= 1. Nil if empty.",
  },
  butlast: {
    usage: [
      { pre: "(butlast [0 1 2])", post: "[0 1]" },
      { pre: "(butlast [0])", post: "[]" },
      { pre: "(butlast [])", post: "nil" },
    ],
    doc: "Last elem excluded. Empty if a count is <= 1. Nil if empty.",
  },
  get: {
    usage: [
      { pre: "(get [0 1 2] 0)", post: "0" },
      { pre: "(get {:a 1 :b 2} :a)", post: "1" },
      { pre: "(get [0 1 2] 9)", post: "nil" },
      { pre: "(get {:a 1 :b 2} :c)", post: "nil" },
    ],
    doc: "Elem at 2nd point. Nil if not exist.",
  },
  "get-in": {
    usage: [
      { pre: "(get-in [[0 1] 2] [0 1])", post: "1" },
      { pre: "(get-in {:a {:b 1}} [:a :b])", post: "1" },
      { pre: "(get-in [[0 1] 2] [9])", post: "nil" },
      { pre: "(get-in {:a {:b 1}} [:c])", post: "nil" },
    ],
    doc: "Elem at 2nd path. Nil if not exist.",
  },
} as const;

const setter_doc = `
### setter
Setter functions for vector / map structure.
But it's near to lens than Clojure.
They return nil if it can be accessible, as the same as getters.
Cion will be used as a type-check, so it's ideal to be strict as possible, so it rejects to set a value implicitly.
`;

const setter_specs = {
  assoc: {
    usage: [
      { pre: "(assoc [0 1] 0 100)", post: "[100 1]" },
      { pre: "(assoc {:a 1 :b 2} :a 100)", post: "{:a 100 :b 2}" },
      { pre: "(assoc {:a 1 :b 2} :c 100)", post: "nil" },
      { pre: "(assoc [0 1] 2 100)", post: "nil" },
    ],
    doc: `Map upd with 3rd val at 2nd point. Nil if not accessible.`,
  },
  "assoc-in": {
    usage: [
      { pre: "(assoc-in [0 [1 2]] [1 0] 100)", post: "[0 [100 2]]" },
      {
        pre: "(assoc-in {:a [0 1] :b 2} [:a 0] 100)",
        post: "{:a [100 1] :b 2}",
      },
      { pre: "(assoc-in {:a 1 :b 2} [:a :c] 100)", post: "nil" },
      { pre: "(assoc-in [0 1] [2 :a] 100)", post: "nil" },
    ],
    doc: `Map upd with 3rd val at 2nd path. Nil if not accessible.`,
  },
  update: {
    usage: [
      { pre: "(update [0 1] 0 inc)", post: "[1 1]" },
      { pre: "(update {:a 1 :b 2} :a inc)", post: "{:a 2 :b 2}" },
      { pre: "(update {:a 1 :b 2} :c inc)", post: "nil" },
      { pre: "(update [0 1] 2 inc)", post: "nil" },
    ],
    doc: `Map upd with 3rd fn at 2nd point. Nil if not accessible.`,
  },
  "update-in": {
    usage: [
      { pre: "(update-in [0 [1 2]] [1 0] inc)", post: "[0 [2 2]]" },
      {
        pre: "(update-in {:a [0 1] :b 2} [:a 0] inc)",
        post: "{:a [1 1] :b 2}",
      },
      { pre: "(update-in {:a 1 :b 2} [:a :c] inc)", post: "nil" },
      { pre: "(update-in [0 1] [2 :a] inc)", post: "nil" },
    ],
    doc: `Map upd with 3rd fn at 2nd path. Nil if not accessible.`,
  },
} as const;

const coll_docs = `
### collection
It follows Clojure Spec against getter / setter.
`;

const coll_specs = {
  conj: {
    usage: [{ pre: "(conj [0 1 2] 3)", post: "[0 1 2 3]" }],
    doc: "2nd added into 1st vec, at the last place.",
  },
  concat: {
    usage: [{ pre: "(concat [0 1 2] [3 4 5])", post: "[0 1 2 3 4 5]" }],
    doc: "Expanded 2nd added into 1st vec, at the last place.",
  },
  interleave: {
    usage: [
      { pre: "(interleave [:a :b] [0 1])", post: "[:a 0 :b 1]" },
      { pre: "(interleave [:a :b] [0 1 2])", post: "[:a 0 :b 1]" },
    ],
    doc: "[1st-0 2nd-0 ... 1st-n 2nd-n]. Cuted if 1st-n/m > 2nd-m/n.",
  },
  reverse: {
    usage: [
      { pre: "(reverse [0 1 2])", post: "[2 1 0]" },
      { pre: "(reverse [1])", post: "[1]" },
      { pre: "(reverse [])", post: "[]" },
    ],
    doc: "Reversed. Id if empty or 1 elem.",
  },
  range: {
    usage: [{ pre: "(range 0 10)", post: "[0 1 2 3 4 5 6 7 8 9]" }],
    doc: "1st to 2nd, without 2nd. Return [1st] if (= 1st 2nd).",
  },
  repeat: {
    usage: [{ pre: `(repeat 2 'x')`, post: `['x' 'x']` }],
    doc: "2nd repeated of 1st length.",
  },
  drop: {
    usage: [
      { pre: "(drop 2 [0 1 2 3])", post: "[2 3]" },
      { pre: "(drop 9 [0 1 2 3])", post: "[]" },
      { pre: "(drop 0 [0 1 2 3])", post: "[0 1 2 3]" },
    ],
    doc: "After 1st idx, with an elem at 1st. Id if 1nd <= 0. 1st < 0 is ok.",
  },
  take: {
    usage: [
      { pre: "(take 2 [0 1 2 3])", post: "[0 1]" },
      { pre: "(take 0 [0 1 2 3])", post: "[]" },
      { pre: "(take 9 [0 1 2 3])", post: "[0 1 2 3]" },
    ],
    doc: "Before 1st idx, without an elem at 1st. Id if 1st >= (count 1st). 1st < 0 is ok.",
  },
  keys: {
    usage: [
      { pre: "(keys {:a 1 :b 2})", post: "[:a :b]" },
      { pre: "(keys {})", post: "[]" },
    ],
    doc: "Keys of 1st map. Empty if empty.",
  },
  zipmap: {
    usage: [{ pre: "(zipmap [:a :b] [0 1])", post: "{:a 0 :b 1}" }],
    doc: "Get a map {1st-0 2nd-0 ... 1st-n 2nd-n}.",
  },
  count: {
    usage: [
      { pre: "(count [1 2 3])", post: "3" },
      { pre: "(count [])", post: "0" },
    ],
    doc: "Count of elems.",
  },
  vector: {
    usage: [{ pre: "(vector 0 1 2)", post: "[0 1 2]" }],
    doc: "Args vec.",
  },
  some: {
    usage: [{ pre: `(some (fn [x] (number? x)) [0 true])`, post: "true" }],
    doc: "True if pred returns true for elems in 2nd vec.",
  },
} as const;

const pred_doc = `
### predication
All are unary functions.
`;

const pred_specs = {
  "prim?": {
    usage: [
      { pre: "(prim? true)", post: "true" },
      { pre: `(prim? 'string')`, post: "true" },
      { pre: "(prim? 1)", post: "true" },
    ],
    doc: "True if boolean / string / number, false if else.",
  },
  "any?": {
    usage: [{ pre: "(any? 0)", post: "true" }],
    doc: "True always.",
  },
  "number?": {
    usage: [{ pre: "(number? 1/2)", post: "true" }],
    doc: "True if number, false if else.",
  },
  "string?": {
    usage: [{ pre: `(string? 'string')`, post: "true" }],
    doc: "True if string, false if else.",
  },
  "vector?": {
    usage: [{ pre: "(vector? [0 1])", post: "true" }],
    doc: "True if vector including empty, false if else.",
  },
  "map?": {
    usage: [{ pre: "(map? {:a 1})", post: "true" }],
    doc: "True if map including empty, false if else.",
  },
  "fn?": {
    usage: [{ pre: "(fn? inc)", post: "true" }],
    doc: "True if pure fn, false if else including keys / maps.",
  },
  "ifn?": {
    usage: [
      { pre: "(ifn? inc)", post: "true" },
      { pre: "(ifn? {:a 1})", post: "true" },
      { pre: "(ifn? :a)", post: "true" },
    ],
    doc: "True if fn/key/map which are callable in 1st sexpr, false if else.",
  },
  "int?": {
    usage: [
      { pre: "(int? 0)", post: "true" },
      { pre: "(int? -1)", post: "true" },
    ],
    doc: "True if integer, false if else.",
  },
  "nat?": {
    usage: [
      { pre: "(nat? 1)", post: "true" },
      { pre: "(nat? 0)", post: "true" },
      { pre: "(nat? -1)", post: "false" },
    ],
    doc: "True if natural numbers (including 0), false if else.",
  },
  "ratio?": {
    usage: [{ pre: "(ratio? 1/2)", post: "true" }],
    doc: "True if fraction (because of not implemented R\\Q), false if else.",
  },
  "pos?": {
    usage: [{ pre: "(pos? 2)", post: "true" }],
    doc: "True if >= 0, false if else.",
  },
  "neg?": {
    usage: [{ pre: "(neg? -2)", post: "true" }],
    doc: "True if <= 0, false if else.",
  },
  "pos-int?": {
    usage: [{ pre: "(pos-int? 2)", post: "true" }],
    doc: "True if int & >= 0, false if else.",
  },
  "neg-int?": {
    usage: [{ pre: "(neg-int? -2)", post: "true" }],
    doc: "True if int & <= 0, false if else.",
  },
  "odd?": {
    usage: [{ pre: "(odd? 1)", post: "true" }],
    doc: "True if odd number, false if else.",
  },
  "even?": {
    usage: [{ pre: "(even? 2)", post: "true" }],
    doc: "True if even number, false if else.",
  },
  "zero?": {
    usage: [{ pre: "(zero? 0)", post: "true" }],
    doc: "True if 0, false if else.",
  },
  "keyword?": {
    usage: [{ pre: "(keyword? :a)", post: "true" }],
    doc: "True if keyword, false if else.",
  },
  "empty?": {
    usage: [
      { pre: "(empty? [])", post: "true" },
      { pre: "(empty? {})", post: "true" },
    ],
    doc: "True if empty vec or map, false if else.",
  },
  "boolean?": {
    usage: [{ pre: "(boolean? false)", post: "true" }],
    doc: "True if boolean, false if else.",
  },
  type: {
    usage: [{ pre: `(type 'string')`, post: `'string'` }],
    doc: "String saying 1st type. Ignored elements and args for vec/map/fn.",
  },
  "every?": {
    usage: [{ pre: "(every? number? [0 1 2])", post: "true" }],
    doc: "True if all elems are true, false if not.",
  },
  "nil?": {
    usage: [{ pre: "(nil? nil)", post: "true" }],
    doc: "True only if 1st are pure nil, false if not.",
  },
  "some?": {
    usage: [{ pre: "(some? 1)", post: "true" }],
    doc: "True if any but nil, false if nil.",
  },
} as const;

const math_doc = `
Decimal point is not supported.

R\Q isn't implemented. All number is expressed with int, n, or rational, n/m.

Int range is -32767 to 32767. Rational is the same because it's written as Int/Int.

4/2, 3/-1, -3/-1 are rounded automatically to 2, -3, 3.

There is no diff for -0 / 0.
`;

const math_specs = {
  "+": {
    usage: [{ pre: "(+ 1 2 3)", post: "6" }],
    doc: "Sum of args. Accepts any number of numeric args.",
  },
  "-": {
    usage: [
      { pre: "(- 5 2)", post: "3" },
      { pre: "(- 5)", post: "-5" },
    ],
    doc: "Sub of args. With one arg, negates it. Accepts multiple args.",
  },
  "*": {
    usage: [{ pre: "(* 2 3 4)", post: "24" }],
    doc: "Product of all args. Accepts any number of numeric args.",
  },
  "/": {
    usage: [
      { pre: "(/ 8 2)", post: "4" },
      { pre: "(/ 8 3)", post: "8/3" },
      { pre: "(/ 8 0)", post: "nil" },
    ],
    doc: "Div of all args. Nil if division by zero. Accepts multiple args.",
  },
  mod: {
    usage: [
      { pre: "(mod 5 3)", post: "2" },
      { pre: "(mod -5 3)", post: "1" },
    ],
    doc: "Returns the modulo (non-negative remainder). Like Clojure’s mod.",
  },
  rem: {
    usage: [
      { pre: "(rem 5 3)", post: "2" },
      { pre: "(rem -5 3)", post: "-2" },
    ],
    doc: "Remainder (sign of numerator). Like Clojure’s rem.",
  },
  "%": {
    usage: [
      { pre: "(% 5 3)", post: "2" },
      { pre: "(% -5 3)", post: "-2" },
    ],
    doc: "Rem. Remainder (sign of numerator). Like Clojure’s rem.",
  },
  trunc: {
    usage: [
      { pre: "(trunc 3)", post: "3" },
      { pre: "(trunc -10/3)", post: "-3" },
    ],
    doc: "Truncates towards zero. Discards decimal part.",
  },
  floor: {
    usage: [
      { pre: "(floor 3)", post: "3" },
      { pre: "(floor -10/3)", post: "-4" },
    ],
    doc: "Rounds down to the nearest whole integer.",
  },
  inc: {
    usage: [{ pre: "(inc 1)", post: "2" }],
    doc: "Num + 1.",
  },
  dec: {
    usage: [{ pre: "(dec 1)", post: "0" }],
    doc: "Num - 1",
  },
  abs: {
    usage: [
      { pre: "(abs -3)", post: "3" },
      { pre: "(abs 3)", post: "3" },
    ],
    doc: "Absolute value.",
  },
  min: {
    usage: [{ pre: "(min 1 5 3)", post: "1" }],
    doc: "The smallest of the arguments.",
  },
  max: {
    usage: [{ pre: "(max 1 5 3)", post: "5" }],
    doc: "The largest of the arguments.",
  },
  ">=": {
    usage: [{ pre: "(>= 2 2 0)", post: "true" }],
    doc: "Grater than or equal. Not infix. Any count of args.",
  },
  "<=": {
    usage: [{ pre: "(<= 0 0 2)", post: "true" }],
    doc: "Less than or equal. Not infix. Any count of args.",
  },
  ">": {
    usage: [{ pre: "(> 2 1 0)", post: "true" }],
    doc: "Grater than. Not infix. Any count of args.",
  },
  "<": {
    usage: [{ pre: "(< 0 1 2)", post: "true" }],
    doc: "Less than. Not infix. Any count of args.",
  },
} as const;

const logical_doc = `
### logic
All are prefix, not infix.
They accepts any number of arguments like Clojure does.
`;

const logical_specs = {
  and: {
    usage: [{ pre: `(and (number? 1) (string? 'string'))`, post: "true" }],
    doc: "Logical operator &. Any count of args.",
  },
  or: {
    usage: [{ pre: "(or (number? 1) (string? 1))", post: "true" }],
    doc: "Logical operator |. Any count of args.",
  },
  not: {
    usage: [{ pre: "(not true)", post: "false" }],
    doc: "Logical operator !. One arg.",
  },
  eq: {
    usage: [{ pre: "(eq 0 0 0)", post: "true" }],
    doc: "=. Equal. Not infix. Any count of args.",
  },
  "=": {
    usage: [{ pre: "(= 0 0 0)", post: "true" }],
    doc: "Equal. Not infix. Any count of args.",
  },
} as const;

const string_doc = `
### string
Strings are expressed as 'string'.
Double-quote and back-quote leaves for TypeScript, and we don't use quote for a macro, so it's the only rest choice.
(This specs may be changed. But it doesn't need to distinct '' and/or "" on Cion futurely at least.)
`;

const string_specs = {
  str: {
    usage: [
      { pre: `(str '0' '1' 2 true)`, post: `'012true'` },
      { pre: `(str 0 [0 1] {:a 0} 2)`, post: "'0[0 1]{:a 0}2'" },
    ],
    doc: 'String them. [0 1] to "[0 1]" and {:a 1} to "{:a 1}". Any count of args.',
  },
  "re-find": {
    usage: [
      { pre: `(re-find 'ab\\d\\dc' 'ab12c')`, post: `'ab12c'` },
      { pre: `(re-find 'ab[\\dz]{1,2}c' 'abz2c')`, post: `'abz2c'` },
    ],
    doc: 'Matched string. Empty if not matched. Not needed #"" (Clojure way).',
  },
  split: {
    usage: [{ pre: `(split 'ababababa' 'b')`, post: `['a' 'a' 'a' 'a' 'a']` }],
    doc: "Splited by 2nd. Not included 2nd.",
  },
  subs: {
    usage: [
      { pre: `(subs '123456' 1 3)`, post: `'23'` },
      { pre: `(subs '123456' 3 8)`, post: `'456'` },
    ],
    doc: "String 2nd to 3rd. Not included 3rd idx char.",
  },
  "subs-all": {
    usage: [
      { pre: `(subs-all '123456' 1 3)`, post: `['1' '23' '456']` },
      { pre: `(subs-all '123456' 0 3)`, post: `['' '123' '456']` },
      { pre: `(subs-all '123456' -3 6)`, post: `['' '123456' '']` },
    ],
    doc: "String Vec: 0 <= n < 2nd, 2nd <= m < 3rd, 3rd <= Limit.",
  },
  replace: {
    usage: [
      { pre: `(replace 'ababababa' 'b' 'a')`, post: "'aaaaaaaaa'" },
      { pre: `(replace 'ab5468ba' '[\\d]' 'x')`, post: "'abxxxxba'" },
      {
        pre: `(replace 'ababababa' 'b' (fn [_ y _] (str y y)))`,
        post: "'abbabbabbabba'",
      },
    ],
    doc: "Replace by 2nd regex with 3rd str or fn. 3rd fn accepts a return of subs-all (see it.",
  },
  join: {
    usage: [{ pre: `(join ',' [0 1 'true' '2'])`, post: `'0,1,true,2'` }],
    doc: "",
  },
} as const;

const fmap_doc = `
### fmap + apply
Cion doesn't have a specific loop syntax.
reduce is a way of it.
(the other choice is using let and fn.)
`;

const fmap_specs = {
  map: {
    usage: [
      { pre: `(map inc [0 1 2])`, post: "[1 2 3]" },
      //      { pre: "(map inc [])", post: "[]" },
    ],
    doc: "All elems updated by 1st fn.",
  },
  filter: {
    usage: [
      { pre: `(filter number? [0 1 '2'])`, post: "[0 1]" },
      { pre: "(filter number? [true false])", post: "[]" },
    ],
    doc: "Elements only if not falsy.",
  },
  remove: {
    usage: [
      { pre: `(remove number? [0 1 '2'])`, post: `['2']` },
      { pre: `(remove number? [0 1])`, post: "[]" },
    ],
    doc: "Elements only if falsy.",
  },
  reduce: {
    usage: [{ pre: `(reduce (fn [r i] (+ r i)) 0 [1 2 3])`, post: "6" }],
    doc: `Folded 3rd by 1st with 2nd as an init. Use this as loop instead of the syntax which isn't implemented in Cion.`,
  },
  apply: {
    usage: [{ pre: `(apply + [0 1 2])`, post: `3` }],
    doc: `Run 1st fn with expanded 2nd as if nth elem is nth arg.`,
  },
} as const;

const macro_specs = {
  if: {
    usage: [
      { pre: "(if true 1 2)", post: "1" },
      { pre: "(if false 1 2)", post: "2" },
      { pre: "(if nil 1 2)", post: "2" },
    ],
    doc: "Conditional branching. If 1st is truthy, evaluates 2nd; else evaluates 3rd. Empty body is not allowed.",
  },
  let: {
    usage: [{ pre: "(let [x 1 y 2] (+ x y))", post: "3" }],
    doc: "Lexical bindings. Binds symbols to values in 1st vector, then evaluates body. No empty body allowed.",
  },
  fn: {
    usage: [{ pre: "((fn [x y] (+ x y 1)) 2 3)", post: "6" }],
    doc: "Anonymous function. Accepts a vector of args and a body. No empty body allowed.",
  },
  "->": {
    usage: [
      { pre: "(-> 1 inc inc)", post: "3" },
      { pre: "(-> {:a 1} :a inc)", post: "2" },
    ],
    doc: "Threading macro. Inserts previous result as the first argument of the next form.",
  },
  "->>": {
    usage: [{ pre: "(->> [1 2 3] (map inc) (filter even?))", post: "[2 4]" }],
    doc: "Threading macro. Inserts previous result as the last argument of the next form.",
  },
  "some->": {
    usage: [
      { pre: "(some-> {:a 1} :a inc)", post: "2" },
      { pre: "(some-> {:b 0} :a inc)", post: "nil" },
    ],
    doc: "Nil-safe threading (->). Skips remaining forms if any step returns nil.",
  },
  "some->>": {
    usage: [
      { pre: "(some->> [1 2 3] (map inc) (filter even?))", post: "[2 4]" },
      { pre: "(some->> nil (map inc))", post: "nil" },
    ],
    doc: "Nil-safe threading (->>). Skips remaining forms if any step returns nil.",
  },
} as const;

const specs = {
  ...getter_specs,
  ...setter_specs,
  ...coll_specs,
  ...pred_specs,
  ...math_specs,
  ...logical_specs,
  ...string_specs,
  ...fmap_specs,
  ...macro_specs,
} as const;

const specs_check = <T>(
  specs: T extends (T extends Docs<BuiltinsUnion> ? T : never) ? T : never,
) => specs;

const _specs = specs_check(specs);

type Dec<N extends number> = [null, 0, 1, 2, 3, 4, 5, 6][N];

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
> = Equal<I, null> extends true
  ? R
  : Equal<true, _SpecCheckForLocal<S, K, I>> extends true
    ? SpecCheckForKey<S, K, Dec<I>, true>
    : false;

type SpecCheck<
  S extends Docs<BuiltinsUnion>,
  K extends keyof S & BuiltinsUnion,
> = K extends any ? SpecCheckForKey<S, K> : never;

const scltest0: _SpecCheckForLocal<typeof specs, "some->>", 0> = true;
const scltest1: _SpecCheckForLocal<typeof specs, "some->>", 1> = true;

// [note]
// @ts-expect-error
const scktest0: CCCCC<typeof specs, "if" | "let"> = true;

// [main]
// Type Check for spec.
const sctest0: Equal<true, SpecCheck<typeof specs, BuiltinsUnion>> = true;
