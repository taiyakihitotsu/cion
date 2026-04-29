export const math_specs = {
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
    doc: "Returns the modulo (non-negative remainder). Like Clojure's mod.",
  },
  rem: {
    usage: [
      { pre: "(rem 5 3)", post: "2" },
      { pre: "(rem -5 3)", post: "-2" },
    ],
    doc: "Remainder (sign of numerator). Like Clojure's rem.",
  },
  "%": {
    usage: [
      { pre: "(% 5 3)", post: "2" },
      { pre: "(% -5 3)", post: "-2" },
    ],
    doc: "Rem. Remainder (sign of numerator). Like Clojure's rem.",
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
} as const
