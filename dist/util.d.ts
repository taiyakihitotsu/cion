declare namespace Util {
    type Equal<X, Y> = (<T>() => T extends X & T | T ? 1 : 2) extends (<T>() => T extends Y & T | T ? 1 : 2) ? true : false;
}
export default Util;
