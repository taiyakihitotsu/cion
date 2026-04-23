export const fmap_specs = {
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
} as const
