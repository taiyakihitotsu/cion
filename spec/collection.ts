export const collection_specs = {
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
} as const
