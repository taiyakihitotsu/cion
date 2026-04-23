export const predicate_specs = {
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
} as const
