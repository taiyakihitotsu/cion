export const getter_specs = {
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
} as const
