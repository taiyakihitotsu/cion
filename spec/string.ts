export const string_specs = {
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
} as const
