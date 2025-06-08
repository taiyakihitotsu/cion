export type Equal<X, Y> =
  (<T>() => T extends X & T | T ? 1 : 2) extends
  (<T>() => T extends Y & T | T ? 1 : 2) ? true : false;

export type * as Util from './util'
