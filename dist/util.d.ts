declare namespace Util {
    type _Eq<X, Y> = (<T>() => T extends X ? 1 : 2) extends (<T>() => T extends Y ? 1 : 2) ? true : false;
    type _Equal<X, Y> = X extends [infer XH, ...infer XR] ? Y extends [infer YH, ...infer YR] ? Equal<XH, YH> | Equal<XR, YR> : false : _Eq<X, Y>;
    export type Equal<X, Y> = _Equal<X, Y> extends true ? true : false;
    export {};
}
export default Util;
