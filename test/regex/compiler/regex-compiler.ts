import type { regexCompiler as rc } from '../../../src/regex/index.js'
import type { WordList, NumList } from '../../../src/regex/index.js'
import type {Equal} from '../../../src/util.js'

// ------------------------
// -- General
// ------------------------

// const NumList: regexConst.NumList = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
// const WordLittleList: regexConst.WordLittleList = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']
// const WordLargeList: regexConst.WordLargeList = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']
// const WordList: regexConst.WordList = [...WordLittleList, ...WordLargeList]
// const List: [...regexConst.WordLittleList, ...regexConst.WordLargeList, ...regexConst.NumList]  = [...WordList, ...NumList]

const test_dot_0: true = {} as Equal<['.'], rc.Comp<'.'>['tapes']>
const test_dot_1: true = {} as Equal<['\\.'], rc.Comp<'\\.'>['tapes']>
const test_dot_2: true = {} as Equal<[['chara-class', ['.']]], rc.Comp<'[.]'>['tapes']>
const test_dot_3: true = {} as Equal<[['chara-class', ['.']]], rc.Comp<'[\\.]'>['tapes']>

// Complex nested or-groups and groups
const test_complex_nest_0: true = {} as Equal<
  ['a', 'b', ['or-group', ['c'], ['g', 'd'], [['group', ['1', '2']], '3', '4'], [['group', ['x', 'y']]], ['a', ['group', ['e', 'f']]], ['d', 'e']], 'f', 'g', 'f'],
  rc.Comp<'ab(c|gd|(12)34|(xy)|a(ef)|de)fgf'>['tapes']
>

// Deeply nested groups
const test_complex_nest_1: true = {} as Equal<
  ['a', 'b', ['group', ['c', 'c', ['group', ['1', '2']], '3', '4']], 'f', 'g', 'f'],
  rc.Comp<'ab(cc(12)34)fgf'>['tapes']
>

const test_complex_nest_2: true = {} as Equal<
  ['a', 'b', ['?', ['group', ['c', 'c', ['group', ['1', '2', ['group', ['0']], '3', '4', ['group', ['6']], '7', ['group', ['8']], '9']]]]], 'f', 'g', 'f'],
  rc.Comp<'ab(cc(12(0)34(6)7(8)9))?fgf'>['tapes']
>

// Quantifiers and chara-classes
const test_quantifier_0: true = {} as Equal<
  ['a', 'b', 'c', ['chara-class', ['d', 'k', 'e']], ['?', ['q', 'q', ['?', 'q'], ['?', 'q'], ['?', 'q']]]],
  rc.Comp<'abc[dke]q{2,5}?'>['tapes']
>

const test_quantifier_1: true = {} as Equal<
  ['a', 'b', 'c', ['chara-class', ['d', 'k', 'e']], ['?', ['q', 'q', ['*', 'q']]]],
  rc.Comp<'abc[dke]q{2,}?'>['tapes']
>

// Character class ranges and digit escapes
const test_class_range_0: true = {} as Equal<
  [['?', ['chara-class', ['a', 'b', 'd', '1', '2', '3', '4', '5', '6', '7', '8']]], ['?', 'a'], 'b', ['*', 'b'], 'd'],
  rc.Comp<'[abd1-8]?a?b+d'>['tapes']
>

const test_class_digit_0: true = {} as Equal<
  [['chara-class', ['a', 'b', 'd', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ''],
  rc.CompCharaClass<'[abd\\d]', '['>
>

const test_class_digit_1: true = {} as Equal<
  [['?', ['chara-class', ['a', 'b', 'd', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', 'a'], 'b', ['*', 'b'], 'd'],
  rc.Comp<'[abd\\d]?a?b+d'>['tapes']
>

// ---------------
// -- email
// ---------------
// ^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$
//
// vue.js : https://v2.vuejs.org/v2/cookbook/form-validation#Using-Custom-Validation
// ----------------------------------------------------------------------------------
// -- first half part

type Email_regtest = ['<', '>', '(', ')', '[', ']', '.', ',', ';', ':', ' ', '@', '"']

const test_neg_chara_0: true = {} as Equal<
  [['chara-class', Email_regtest]],
  rc.Comp<'[^<>()[\\].,;: @"]', { negExpand: false }>['tapes']
>

const test_neg_chara_group_0: true = {} as Equal<
  [[ 'group'
    , '\\.'
    , ['chara-class', Email_regtest]
    , ['*', ['chara-class', Email_regtest]]]],
  rc.Comp<'(\\.[^<>()[\\].,;: @"]+)', { negExpand: false }>['tapes']
>

const test_email_fst_half_complex: true = {} as Equal<
  [[ 'or-group'
    , [['group', ['chara-class', Email_regtest]
               , ['*', ['chara-class', Email_regtest]]
               , [ '*'
                 , [ 'group'
                   , '\\.'
                   , ['chara-class', Email_regtest]
                   , [ '*'
                     , ['chara-class', Email_regtest]]]]]]
    , [['group', '\"', '.', ['*', '.'], ['\"']]]]],
  rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))', { negExpand: false }>['tapes']
>

// -- second half part
type Numclass_AST = [['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]]

const test_ip_address_group: true = {} as Equal<
  [['group', '\\[', Numclass_AST, '\\.', Numclass_AST, '\\.', Numclass_AST, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],
  rc.Comp<'(\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])'>['tapes']
>

const test_word_num_hyphen_class: true = {} as Equal<
  [['chara-class', [...WordList, '-', ...NumList]]],
  rc.Comp<'[a-zA-Z-0-9]'>['tapes']
>

const test_subdomain_plus: true = {} as Equal<
  [['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]]],
  rc.Comp<'([a-zA-Z0-9-]+\\.)+'>['tapes']
>

const test_domain_full_times: true = {} as Equal<
  [['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]],
  rc.Comp<'(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,})'>['tapes']
>

const test_email_snd_half_or_group: true = {} as Equal<
  [['or-group',[['group', '\\[', Numclass_AST, '\\.', Numclass_AST, '\\.', Numclass_AST, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],[['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]]]],
  rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
>

// -- all mail regex
type First_end_mailtest = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))', {negExpand: false}>['tapes']
const first_end_mailtest: true = {} as Equal<First_end_mailtest,
   [[ 'or-group'
   , [['group', ['chara-class', Email_regtest]
              , ['*', ['chara-class', Email_regtest]]
              , [ '*'
                , [ 'group'
                  , '\\.'
                  , ['chara-class', Email_regtest]
                  , [ '*'
                    , ['chara-class', Email_regtest]]]]]]
   , [['group', '\"', '.', ['*', '.'], ['\"']]]]]>

type Second_end_mailtest = rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
const second_end_mailtest: true = {} as Equal<Second_end_mailtest, [['or-group',[['group', '\\[', Numclass_AST, '\\.', Numclass_AST, '\\.', Numclass_AST, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],[['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]]]]>

const test_full_email_neg_expand: true = {} as Equal<
  [...First_end_mailtest, '@', ...Second_end_mailtest],
  rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))', { negExpand: false }>['tapes']
>

// [todo]
export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>

export type * as testrc from './regex-compiler.js'
