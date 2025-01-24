// NOTE
// ---------------------------------
// type Equal<X, Y> = 
//   (<T>() => T extends X ? 1 : 2) extends
//   (<T>() => T extends Y ? 1 : 2) ? true : false
// 
// > https://github.com/microsoft/TypeScript/issues/27024
// ---------------------------------
// const eeeee0: Bit.BitLen<'3999'>  = [[[[null]]]]
// const eeeee1: Bit.BitLen<'11111'> = [[[[[null]]]]]
// const eeeee2: Equal<Bit.BitLen<'3999'>, Bit.BitLen<'11111'>> = true // => it should be false but the above definition returns 'true wrongly.
// ---------------------------------
// And they are able to be passed unintentionaly.
//
// const fffff0: typeof eeeee0 = eeeee0
// const fffff1: typeof eeeee1 = fffff0

namespace Util {

type _Eq<X, Y> =
  (<T>() => T extends X ? 1 : 2) extends
  (<T>() => T extends Y ? 1 : 2) ? true : false;

type _Equal<X,Y> =
  X extends [infer XH, ...infer XR]
    ? Y extends [infer YH, ...infer YR]
        ? Equal<XH, YH> | Equal<XR, YR>
        : false
    : _Eq<X,Y>

export type Equal<X,Y> = 
  _Equal<X,Y> extends true ? true : false
}

export default Util; 
