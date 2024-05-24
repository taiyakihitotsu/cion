process.stdin.resume();
process.stdin.setEncoding("utf8");
// jlist
const cell = { type: "", elem: null };
const atom = (l) => {
  return typeof l === Object && l.type === "atom" ? true : false;
};
const cons = (el, l) => {
  r = l.concat();
  r.unshift(el);
  return r;
};
const car = (l) => {
  l[0];
};
const cdr = (l) => {
  r = l.concat();
  r.shift();
  return r;
};
const eq = (a, b) => {
  JSON.stringify(a) === JSON.stringify(b);
};
// parser
const reg_split = new RegExp(/(\)|\(|[\s\n]+)/);
const reg_space = new RegExp(/[\s\n]+/);
const separate = (s) =>
  s.split(reg_split).filter((x) => x !== "" && !reg_space.test(x));
const _sexpr = "(+ 1 2 (- 1 2))";
// util
const reduce = (f, s, r) => {
  let _r = null;
  const lim = r.length;
  for (let i = 0; i < lim; i++) {
    _r = f(_r, r[i]);
  }
  return _r;
};
const apply = (f, l) => f.apply(null, l); // 'apply' can't receive an arrow fn.
const apply_with = (mp, k, l) => mp[k].apply(null, l);
// eval
const eval_op_with = (curr) =>
  apply_with(curr.context, curr.opcode, curr.oprand);
const eval_atom_with = (curr, symname) => curr.context[symname];
// stack
// left
const curr_init = () => {
  return { opcode: "", oprand: [], argtypes: [], context: {}, depth: 0 };
};
const next = (stack, curr) => {
  const _curr = curr_init();
  _curr.context = curr.context;
  _curr.depth = curr.depth + 1;
  console.log(stack);
  console.log(curr);
  return [cons(curr, stack), _curr];
};
// right
const back = (stack, curr) => {
  const c_ = car(stack);
  if (c_.opcode === "") return false;
  const r_ = eval_op_with(c_);
  c_.oprand = cons(r_, c_.oprand);
  return [cdr(stack), c_];
};
// mid
const reg_int = new RegExp(/^[0-9]+$/);
const reg_sym = new RegExp(/[a-zA-z][a-zA-Z0-9\+\-\/\*]+/);
const toint = (s) => (reg_int.test(s) ? Number.parseInt(s) : null);
const resolve = (curr, s) => curr.context[s];
const which = (s) =>
  reg_int.test(s) ? "num" : reg_sym.test(s) ? "sym" : "else";
const match = {
  num: (x, s) => toint(s),
  sym: resolve,
  else: () => console.err("err"),
};
const toelem = (curr, s) => match[which(s)](curr, s);
// machine
// use reduce.
const oneset = ([stack, curr], s) => {
  if (s === "(") return next(stack, curr);
  if (s === ")") {
    //    if (stack.length === 0) {
    //      console.log("too right bracket err.");
    //      return false }
    return back(stack, curr);
  }
  const r_ = toelem(s);
  if (curr.opcode === "") {
    curr.opcode = r_;
    return [stack, curr];
  } else {
    curr.oprand = cons(r_, curr.oprand);
    return [stack, curr];
  }
};
// invoke
// let invoke = (curr) => fnlist[curr.oprand].apply(curr.argtypes)
// test
console.log(separate(_sexpr));
console.log(reduce((x, y) => x + y, 0, [1, 2, 3]));
console.log(toint("1"));
const a = { plus: (x, y) => x + y, empty: () => true };
console.log(apply(a["plus"], [1, 2]));
console.log(apply_with(a, "plus", [1, 2]));
console.log(apply(a["empty"], []));
console.log(toelem({}, "124"));
console.log(toelem({ context: a }, "plus"));
