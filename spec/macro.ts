export const macro_specs = {
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
} as const
