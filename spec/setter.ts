export const setter_specs = {
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
} as const
