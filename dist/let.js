"use strict";
// -------------------------
Object.defineProperty(exports, "__esModule", { value: true });
// test
const getVarTest = "string";
const getVarTest2 = "stringer";
const EvalTest3 = NotMatch;
//test
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
