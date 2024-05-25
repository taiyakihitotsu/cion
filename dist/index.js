"use strict";
const Nil = [];
const NotMatch = "NotMatch";
// test
const getVarTest = "string";
const getVarTest2 = "stringer";
const EvalTest3 = NotMatch;
const letTest = [
    [
        { name: "ss", value: "stringer" },
        { name: "s", value: "string" },
        { name: "cc", value: [`prim`, `p/cc`] },
    ],
    [{ name: "sss", value: "str" }],
];
const readLetTest = "string";
const readLetTest2 = "str";
const readLetTest3 = NotMatch;
// test / primitive - case
const readLetTest4 = [`prim`, `p/sss`];
const readLetTest5 = [
    `prim`,
    `p/cc`,
];
// test readatom
const readatomtest = [`prim`, `p/sss`];
const readatomtest2 = [`prim`, `'sss'`];
// test reading
const readingtest = [
    ["prim", "a-str"],
    ["prim", "b-str"],
    ["prim", "c-str"],
];
const appendTest = [`prim`, "'+test'"];
// test str
const strtest1 = [
    `prim`,
    `test+tail`,
];
const strtest2 = [`prim`, `test`];
// test
const lispeqtest = [`prim`, "'a'"];
const lispeqtest1 = [`prim`, true];
const lispeqtest2 = [`prim`, false];
const lispeqtest3 = [`prim`, false];
const lispeqtest4 = [`prim`, true];
const lispeqtest5 = [`prim`, false];
// test let
const larttest = [[`sym`, `t`], `test`];
// -----------------
// memo
// type arrr = [number, ...number[]]
// const aaaaaa: arrr = [1,2,3,4]
// ------------------
// test fn
const dectest = [
    `fn`,
    [[`sym`, `a`]],
    [
        [`sym`, `a`],
        [`sym`, `b`],
    ],
];
const eqtest1 = true;
const eqtest2 = false;
const eqtest3 = false;
const eqtest4 = false;
const eqtest5 = true;
// biome-ignore lint/complexity/noBannedTypes:
const eqtest6 = false;
const eqtest7 = false;
const eqtest8 = false;
const eqtest9 = false;
const eqtest10 = true;
const eqtest11 = true;
const vectortest = [`vec`, [`prim`, `1`]];
const vectortest2 = [
    `vec`,
    [`prim`, `1`],
    [`prim`, `2`],
];
const vectortest3 = [
    `vec`,
    [`vec`, [`prim`, `2`]],
];
// error TS2322: Type '"prim"' is not assignable to type '"vec"'.
// const vectortest4: Vector = [`prim`, `1`]
const vectortest5 = [
    `vec`,
    [`vec`, [`vec`, [`prim`, true], [`prim`, `1`]], [`prim`, `1`]],
];
const testgetvec = [`prim`, 3];
const testgetmap = [`prim`, `val/a`];
const testgetvec1 = [`prim`, 3];
const testgetmap1 = [`prim`, `val/a`];
const testfirst = [`prim`, true];
const testrest = [`vec`, [`prim`, 0], [`prim`, 1]];
const testrest1 = [`vec`, [`prim`, 1]];
const testrest2 = [`vec`];
const testconj = [
    `vec`,
    [`prim`, true],
    [`prim`, 0],
    [`prim`, 1],
    [`prim`, false],
];
const testconj1 = [`vec`, [`prim`, false]];
const testconcat = [
    `vec`,
    [`prim`, true],
    [`prim`, 0],
    [`prim`, 1],
    [`prim`, true],
    [`prim`, 0],
    [`prim`, 1],
];
// if not directly input those sexpr, through args, this fmap eval returns any, because of ...infer T (in FMap) would be expanded unknown.
const testargv = [`vec`, [`prim`, `'1'`], [`prim`, `'2'`]];
const testfmap = [`vec`, [`prim`, `'+1'`], [`prim`, `'+2'`]];
const testfilter = [`vec`, [`prim`, 1], [`prim`, 1]];
const testfnlispeqa = [
    `prim`,
    false,
];
const testfnlispeqaa = [`prim`, false];
const testfnlispeq0 = [`prim`, false]; // todo : fix
const testfnlispeq1 = [`prim`, true]; // todo : fix
// interleave test
const testinterleave0 = [1, 4, 2, 5, 3, 6];
const testinterleave1 = [1, 4, 2, 5];
const testinterleave2 = [1, 2];
// multiarg fn test
const testmultiargfn0 = [`prim`, `'0''1'`];
// test raw
const evalTest = [`prim`, "'+test'"];
const evalTest2 = {
    error: ["AppendError", ["prim", "NotMatch"]],
};
const evalTest3 = [
    `prim`,
    "'+strval'",
];
const evalTest4 = [`prim`, "'+strval'"];
const evalTest5 = [`prim`, "'+strval'"];
// test fn
const evalfntest = [`prim`, "'+test'"];
const evalfntest2 = [`prim`, "'+test'"];
// test fn sym
const evalfnsymrawtest = [`prim`, `'+test'`];
const evalfnsymtest = [`prim`, `'+test'`];
// test atomic
const evalatomtest = [`prim`, `'test'`];
const evalatomtest2 = [
    `prim`,
    `'testval'`,
];
const evalatomtest3 = [`prim`, `'prim/test'`];
const evalatomtest4 = [`fn`, [[`sym`, `a`]], [`sym`, `a`]];
// test let
// const evallettest: Eval<[`let`, [[`sym`, `t`], `'test'`], [`sym`, `t`]]> = [`prim`, "'test'"] // deprecated.
const evallettest = {
    error: [
        "EvalError7/ 2nd in let form must be Atom. No wrapped value is deprecated.",
        0,
        ["let", [["sym", "t"], "'test'"], ["sym", "t"]],
    ],
};
const evalletwprimtest = [`prim`, "'test'"];
const evalrecfntest1 = [
    `prim`,
    `'+test'`,
];
const evalreclettest0 = [`prim`, `'+test'`];
const evalreclettest1 = [`prim`, `'++test'`];
const evalreclettest2 = [`prim`, `'++test'`];
const evalfltest0 = [`prim`, `'+test'`];
// ----------------------------
// todo : gross error msg.
// src/index.ts:218:7 - error TS2322: Type 'string[]' is not assignable to type '"AppendError"'.
//
// 218 const evallftest0: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
//
// const evallftesterr: Eval<[lfInnerTest, [`prim`, `test''`]]> = [`prim`, `'+test'`]
// ----------------------------
const evallftest0 = [
    `prim`,
    `'+test'`,
];
// test interleaved let form
const testiletform = [`prim`, `'1''2'`];
const evaliftest2 = [`prim`, true];
const evaliftest3 = [`prim`, false];
const evalifrectest0 = [`prim`, true];
const evalifrectest1 = [`prim`, false];
const evalifretfntest = [`prim`, `'+test'`];
// test str
const evalstrtest = [`prim`, `head/tail`];
const evalletmoretest1 = [`prim`, `text-a/text-b`];
const aaaaaaaaa = [
    `let`,
    [
        [`sym`, `a`],
        [`prim`, `text-a`],
        [`sym`, `b`],
        [`prim`, `text-b`],
    ],
    [
        [`sym`, `str`],
        [`sym`, `a`],
        [`sym`, `b`],
    ],
];
// test prim error pattern.
const evalprimerrortest = [`prim`, 0];
// test lispeq
const evallispeqtest0 = [
    `prim`,
    true,
];
const evallispeqtest1 = [
    `prim`,
    false,
];
const evallispeqtest2 = [`prim`, true];
//test (no env)
const rbiTest = [
    `prim`,
    "'+test'",
];
const rbiTest2 = [`prim`, "'++test'"];
const rbiTest3 = [`prim`, "'+++test'"];
// test (with env)
const rbiTest4 = [`prim`, "'+test'"];
const rbiTest5 = [`prim`, "'+test'"];
// --------------------
// def / defn a scribble.
// biome-ignore lint/suspicious/noExplicitAny:
const defined = null;
const readdef = [`sym`, `test`]; // null as any
// const readdef2: typeof defined = [`sym`, `tet`] // null as any
