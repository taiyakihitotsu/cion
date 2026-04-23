export const logic_specs = {
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
} as const
