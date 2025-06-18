import type { regexCompiler as rc } from '../src/regex-compiler'
import type * as regexConst from '../src/regex-const'

// -----------------------------
// -- Compiler MinMax
// -----------------------------
const _1 = "0000000000000001" as const
const _2 = "0000000000000010" as const
const _3 = "0000000000000011" as const
const _15 = "0000000000001111" as const
const _16 = "0000000000010000" as const
// -- compile
const mmtest0: rc.CompMinMax<'{1,2}rest'> = [_1, _2, 'rest']
const mmtest1: rc.CompMinMax<'{1,}rest'> = [_1, '<', 'rest']
const mmtest2: rc.CompMinMax<'{1,15}rest'> = [_1, _15, 'rest']
const mmtest3: rc.CompMinMax<'{15,16}rest'> = [_15, _16, 'rest']
const mmtest4: rc.CompMinMax<'{15,}rest'> = [_15, '<', 'rest']
const mmtest5: rc.CompMinMax<'{2}rest'> = [_2, '=', 'rest']
const mmtest: rc.CompMinMax<'{15}rest'> = [_15, '=', 'rest']
// -- expand
const mmetest0: rc.ExpandMinMax<'rest', typeof _1, typeof _2> = ['rest', ['?', 'rest']]
const mmetest1: rc.ExpandMinMax<'rest', typeof _1, '<'> = ['rest', ['*', 'rest']]
const mmetest2: rc.ExpandMinMax<'rest', typeof _1, typeof _15> = ['rest', ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest']]
const mmetest3: rc.ExpandMinMax<'rest', typeof _15, '='> = ['rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest', 'rest']
// const mmetest4: rc.ExpandMinMax<'rest', typeof _1, typeof _2> = ['rest', ['?', 'rest']]
// const mmetest5: rc.ExpandMinMax<'rest', typeof _1, typeof _2> = ['rest', ['?', 'rest']]
// const mmetest: rc.ExpandMinMax<'rest', typeof _1, typeof _2> = ['rest', ['?', 'rest']]


// ------------------------
// -- General
// ------------------------

const NumList: regexConst.NumList = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
const WordLittleList: regexConst.WordLittleList = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
const WordLargeList: regexConst.WordLargeList = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
const WordList: regexConst.WordList = [...WordLittleList, ...WordLargeList]
const List: [...regexConst.WordLittleList, ...regexConst.WordLargeList, ...regexConst.NumList]  = [...WordList, ...NumList]

const test_doc0: rc.Comp<'.'>['tapes'] = ['.']
const test_doc1: rc.Comp<'\\.'>['tapes'] = ['\\.']
const test_doc2: rc.Comp<'[.]'>['tapes'] = [['chara-class', ['.']]]
const test_doc: rc.Comp<'[\\.]'>['tapes'] = [['chara-class', ['.']]]

const test_ncmpajk: rc.Comp<'ab(c|gd|(12)34|(xy)|a(ef)|de)fgf'>['tapes'] = ['a', 'b', ['or-group', ['c'], ['g', 'd'], [['group', ['1', '2']], '3', '4'], [['group', ['x', 'y']]], ['a', ['group', ['e', 'f']]], ['d', 'e']], 'f', 'g', 'f']
const test_ajkg: rc.Comp<'ab(cc(12)34)fgf'>['tapes'] = ['a', 'b', ['group', ['c', 'c', ['group', ['1', '2']], '3', '4']], 'f', 'g', 'f']
const test_hajkg: rc.Comp<'ab(cc(12(0)34(6)7(8)9))?fgf'>['tapes'] = ['a', 'b', ['?', ['group', ['c', 'c', ['group', ['1', '2', ['group', ['0']], '3', '4', ['group', ['6']], '7', ['group', ['8']], '9']]]]], 'f', 'g', 'f']
const tete: rc.Comp<'abc[dke]q{2,5}?'>['tapes'] = ['a', 'b', 'c', ['chara-class', ['d', 'k', 'e']], ['?', ['q', 'q', ['?', 'q'], ['?', 'q'], ['?', 'q']]]]
const tetete: rc.Comp<'abc[dke]q{2,}?'>['tapes'] = ['a', 'b', 'c', ['chara-class', ['d', 'k', 'e']], ['?', ['q', 'q', ['*', 'q']]]]
const tefte: rc.Comp<'[abd1-8]?a?b+d'>['tapes'] = [['?', ['chara-class', ['a', 'b', 'd', '1', '2', '3', '4', '5', '6', '7', '8']]], ['?', 'a'], 'b', ['*', 'b'], 'd']
const teftde: rc.CompCharaClass<'[abd\\d]', '['> = [['chara-class', ['a', 'b', 'd', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], '']
const teftdec: rc.Comp<'[abd\\d]?a?b+d'>['tapes'] = [['?', ['chara-class', ['a', 'b', 'd', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', 'a'], 'b', ['*', 'b'], 'd']

// ---------------
// -- email
// ---------------
// ^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$
//
// vue.js : https://v2.vuejs.org/v2/cookbook/form-validation#Using-Custom-Validation
// ----------------------------------------------------------------------------------
// -- first half part
const _email_regtest0:  ['<', '>', '(', ')', '[', ']', '.', ',', ';', ':', ' ', '@', '"'] = ['<', '>', '(', ')', '[', ']', '.', ',', ';', ':', ' ', '@', '"']

const first_mailtest0: rc.Comp<'[^<>()[\\].,;: @"]', {negExpand: false}>['tapes'] = [['chara-class', _email_regtest0]]

const first_mailtest1: rc.Comp<'(\\.[^<>()[\\].,;: @"]+)', {negExpand: false}>['tapes'] = 
  [[ 'group'
   , '\\.'
   , ['chara-class', _email_regtest0]
   , ['*', ['chara-class', _email_regtest0]]]]

const first_end_mailtest: rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))', {negExpand: false}>['tapes'] =
   [[ 'or-group'
   , [['group', ['chara-class', _email_regtest0]
              , ['*', ['chara-class', _email_regtest0]]
              , [ '*'
                , [ 'group'
                  , '\\.'
                  , ['chara-class', _email_regtest0]
                  , [ '*'
                    , ['chara-class', _email_regtest0]]]]]]
   , [['group', '\"', '.', ['*', '.'], ['\"']]]]]

// -- second half part
const _numclass_regtest0: [['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]] = 
  [ [ 'chara-class'
    , [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]
  , [ '?'
    , [ 'chara-class'
      , ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]
  , [ '?'
    , ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]]

const second_mailtest3: rc.Comp<'(\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])'>['tapes'] = [['group', '\\[', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]]

const second_mailtest4a: rc.Comp<'[a-zA-Z-0-9]'>['tapes'] = [['chara-class', [...WordList, '-', ...NumList]]]

const second_mailtest4b: rc.Comp<'([a-zA-Z0-9-]+\\.)+'>['tapes'] = [['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]]]

const second_mailtest4: rc.Comp<'(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,})'>['tapes'] = [['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]]

const second_end_mailtest: rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes'] =  [['or-group',[['group', '\\[', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],[['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]]]]
// -- all mail regex
const all_end_mailtest: rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))', {negExpand: false}>['tapes'] = [...first_end_mailtest, '@', ...second_end_mailtest]

export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>

export type * as testrc from './regex-compiler'
