// If sexpr spits error out, it saves the errors to pass it as a result.
export type ErrorMatch = {error: string, message: string, sexpr: unknown}
export type ErrorCase<
  Case extends string
, Msg extends string
, S
, Env = []> =
{error: Case, message: Msg, sexpr: S} & (Env extends [] ? {} : {env: Env})

// ------------------------
// -- Constants
// ------------------------

export type LetError0 = "LetError";

export type ReadLetError0 = "ReadLetError0"

export type ReadingError0 = "ReadingError0";
export type ReadingError1 = "ReadingError1";

// ------------------------
// -- Builtins
// ------------------------

export type LispSplitError0 = "LispSplitError0"

export type LiteralReplaceError0 = 'LiteralReplaceError0'
export type LiteralReplaceError1 = 'LiteralReplaceError1'
export type LiteralReplaceError2 = 'LiteralReplaceError2'
export type LiteralReplaceError3 = 'LiteralReplaceError3'

export type LispReplaceError0 = 'LispReplaceError0'

export type LispStrSubsAll0 = 'LispStrSubsAll0'
export type LispStrSubsAll1 = 'LispStrSubsAll1'
export type LispStrSubsAll2 = 'LispStrSubsAll2'
export type LispStrSubsAll3 = 'LispStrSubsAll3'

export type LispJoinError0 = 'LispJoinError0'
export type LispJoinError1 = 'LispJoinError1'
export type LispJoinError2 = 'LispJoinError2'
export type LispJoinError3 = 'LispJoinError3'
export type LispJoinError4 = 'LispJoinError4'

export type LispAndError0 = "LispAndError0"

export type LispIsCollError0 = 'LispIsCollError0'

export type LispEqError0 = "LispEqError0"

export type LispAddError0 = 'LispAddError0'
export type LispAddError1 = 'LispAddError1'
export type LispAddError2 = 'LispAddError2'

export type LispSubError0 = 'LispSubError0'
export type LispSubError1 = 'LispSubError1'

export type LispIncError0 = 'LispIncError0'

export type LispDecError0 = 'LispDecError0'

export type LispMulError0 = 'LispMulError0'
export type LispMulError1 = 'LispMulError1'

export type LispDivError0 = 'LispDivError0'
export type LispDivError1 = 'LispDivError1'
export type LispDivError2 = 'LispDivError2'

export type LispRemOrModError0 = 'LispRemOrModError0'
export type LispRemOrModError1 = 'LispRemOrModError1'
export type LispRemOrModError2 = 'LispRemOrModError2'

export type LispRelationError0 = 'LispRelationError0'
export type LispRelationError1 = 'LispRelationError1'

export type LispIsAnyError0 = 'LispIsAnyError0'

export type ConcatError0 = "ConcatError0"
export type ConcatError1 = "ConcatError1"
export type ConcatError2 = "ConcatError2"

export type VConsError0 = 'VConsError0'
export type VConsError1 = 'VConsError1'
export type VConsError2 = 'VConsError2'

export type GetMapError0 = "GetMapError0";
export type GetMapError1 = "GetMapError1";
export type GetMapError2 = "GetMapError2";

export type GetVecError0 = 'GetVecError0'
export type GetVecError1 = 'GetVecError1'
export type GetVecError2 = 'GetVecError2'

export type GetError0 = 'GetError0'
export type GetError1 = 'GetError1'
export type GetError2 = 'GetError2'

export type LispGetError0 = "LispGetError0"

export type LispGetInError0 = 'LispGetInError0'
export type LispGetInError1 = 'LispGetInError1'

export type LispSecondError0 = "LispSecondError0"

export type AssocError0 = 'AssocError0'
export type AssocError1 = 'AssocError1'
export type AssocError2 = 'AssocError2'
export type AssocError3 = 'AssocError3'

export type AssocErrorMsg0 = 'Args: 1st Map & 2nd not keyword.'
export type AssocErrorMsg1 = 'Args: 1st Vector & 2nd not number.'

export type AssocInError0 = 'AssocInError0'
export type AssocInError1 = 'AssocInError1'
export type AssocInError2 = 'AssocInError2'
export type AssocInError3 = 'AssocInError3'
export type AssocInError4 = 'AssocInError4'
export type AssocInError5 = 'AssocInError5'
export type AssocInError6 = 'AssocInError6'
export type AssocInError7 = 'AssocInError7'
export type AssocInError8 = 'AssocInError8'

export type AccessFailed = 'AccessFailed'

export type LispAssocError0 = 'LispAssocError0'
export type LispAUErrorMsg  = '1st or 2nd is not proper form.'

export type LispAssocInError0 = 'LispAssocInError0'

export type LispUpdateError0 = 'LispUpdateError0'

export type LispUpdateInError0 = 'LispUpdateInError0'
export type LispUpdateInError1 = 'LispUpdateInError1'
export type LispUpdateInError2 = 'LispUpdateInError2'
export type LispUpdateInError3 = 'LispUpdateInError3'

export type LispVectorError0 = "LispVectorError0"

export type CountError0 = "CountError0"
export type CountError1 = "CountError1"

export type LispZipmapError0 = "LispZipmapError0"

export type LispApplyError0 = 'LispApplyError0'

export type FirstError0 = "FirstError0";
export type FirstError1 = "FirstError1";

export type RestError0 = "RestError0";
export type RestError1 = "RestError1"

export type CommonArgVecErrMsg = 'arg should be vector as an inner expression.'
export type CommonArgVecErrMsgFn<S extends string> = `arg of ${S} should be a vector.`

export type ButlastError0 = "ButlastError0"
export type ButlastError1 = "ButlastError1"
export type ButlastError2 = "ButlastError2"
export type ButlastError3 = "ButlastError3"

export type ConjError0 = "ConjError0"
export type ConjError1 = "ConjError1"
export type ConjError2 = "ConjError2"
export type ConjError3 = "ConjError3"

export type TakeError0 = "TakeError0"
export type TakeError1 = "TakeError1"
export type TakeError2 = "TakeError2"
export type TakeError3 = "TakeError3"

export type DropError0 = "DropError0"
export type DropError1 = "DropError1"
export type DropError2 = "DropError2"
export type DropError3 = "DropError3"

export type LispMinError0 = 'LispMinError0'
export type LispMaxError0 = 'LispMaxError0'

export type FMapError = "MapError";
export type FilterError = "FilterError";
export type RemoveError = "RemoveError";
export type EveryError = "EveryError";
export type SomeError = "SomeError";

export type FilterError0 = "FilterError0"
export type FilterError1 = "FilterError1"

export type  RemoveError0 = "RemoveError0"

export type EveryError0 = 'EveryError0'
export type EveryError1 = 'EveryError1'

export type SomeError0 = 'SomeError0'
export type SomeError1 = 'SomeError1'

export type LispIsNilError0 = 'LispIsNilError0'

export type LispIsSomeError0 = 'LispIsSomeError0'

export type InterleaveError0 = "InterleaveError0"
export type InterleaveError1 = "InterleaveError1";

export type LispKeysError0 = 'LispKeysError0'
export type LispKeysError1 = 'LispKeysError1'
export type LispKeysError2 = 'LispKeysError2'
export type LispKeysError3 = 'LispKeysError3'
export type LispKeysError4 = 'LispKeysError4'
export type LispKeysError5 = 'LispKeysError5'
export type LispKeysError6 = 'LispKeysError6'
export type LispKeysError7 = 'LispKeysError7b'

export type LispThirdError0 = "LispThirdError0"

export type LispAbsError0 = 'LispAbsError0'

export type LispRepeatError0 = 'LispRepeatError0'
export type LispRepeatError1 = 'LispRepeatError1'

export type LispRangeError0 = 'LispRangeError0'
export type LispRangeError1 = 'LispRangeError1'

export type ReduceError0 = 'ReduceError0'
export type ReduceError1 = 'ReduceError1'
export type ReduceError2 = 'ReduceError2'

export type ReverseError0 = 'ReverseError0'
export type ReverseError1 = 'ReverseError1'
export type ReverseError2 = 'ReverseError2'
export type ReverseError3 = 'ReverseError3'

export type InsertSecondError0 = 'InsertSecondError0'
export type InsertSecondError1 = 'InsertSecondError1'

export type InsertLastError0 = 'InsertLastError0'

export type ThreadFirstError0 = 'ThreadFirstError0'
export type ThreadFirstError1 = 'ThreadFirstError1'
export type ThreadFirstError2 = 'ThreadFirstError2'
export type ThreadFirstError3 = 'ThreadFirstError3'

export type LispThreadFirstError0 = 'LispThreadFirstError0'

export type ThreadLastError0 = 'ThreadLastError0'
export type ThreadLastError1 = 'ThreadLastError1'
export type ThreadLastError2 = 'ThreadLastError2'
export type ThreadLastError3 = 'ThreadLastError3'

export type LispThreadLastError0 = 'LispThreadLastError0'

export type SomeThreadFirstError0 = 'SomeThreadFirstError0'
export type SomeThreadFirstError1 = 'SomeThreadFirstError1'
export type SomeThreadFirstError2 = 'SomeThreadFirstError2'
export type SomeThreadFirstError3 = 'SomeThreadFirstError3'

export type LispSomeThreadGeneralError0 = 'LispSomeThreadGeneralError0'
export type LispSomeThreadGeneralError1 = 'LispSomeThreadGeneralError1'

export type LispSomeThreadFirstError0 = 'LispSomeThreadFirstError0'
export type LispSomeThreadFirstError1 = 'LispSomeThreadFirstError1'
export type LispSomeThreadLastError0 = 'LispSomeThreadLastError0'
export type LispSomeThreadLastError1 = 'LispSomeThreadLastError1'

export type LastError0 = 'LastError0'
export type LastError1 = 'LastError1'

export type ConcatError = "ConcatError";

// ----------------
// -- Eval
// ----------------

export type EvalError1 = "EvalError1"
export type EvalError2 = "EvalError2"
export type EvalError3 = "EvalError3"
export type EvalError4 = "EvalError4"
export type EvalError5 = "EvalError5"
export type EvalError6 = "EvalError6"
export type EvalError7 = "EvalError7"
export type EvalError8 = "EvalError8"
export type EvalError9 = "EvalError9"
export type EvalError10 = "EvalError10"
export type EvalError11 = "EvalError11"
export type EvalError12 = "EvalError12"
export type EvalError13 = "EvalError13"
export type EvalError14 = "EvalError14"
export type EvalError15 = "EvalError15"
export type EvalError16 = "EvalError16"
export type EvalError17 = "EvalError17"
export type EvalError18 = "EvalError18"
export type EvalError19 = "EvalError19"
export type EvalError20 = "EvalError20"

export type * as error from './error.js'
