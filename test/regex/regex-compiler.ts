import type { regexCompiler as rc } from '../../src/regex-compiler'
import type * as regexConst from '../../src/regex-const'
import type {Equal} from '../../src/util'

// -----------------------------
// -- Compiler MinMax
// -----------------------------
const _1 = "0000000000000001" as const
const _2 = "0000000000000010" as const
const _3 = "0000000000000011" as const
const _15 = "0000000000001111" as const
const _16 = "0000000000010000" as const

// -- compile
const test_minmax_1_2: true = {} as Equal<[typeof _1, typeof _2, 'rest'], rc.CompMinMax<'{1,2}rest'>>
const test_minmax_1_inf: true = {} as Equal<[typeof _1, '<', 'rest'], rc.CompMinMax<'{1,}rest'>>
const test_minmax_1_15: true = {} as Equal<[typeof _1, typeof _15, 'rest'], rc.CompMinMax<'{1,15}rest'>>
const test_minmax_15_16: true = {} as Equal<[typeof _15, typeof _16, 'rest'], rc.CompMinMax<'{15,16}rest'>>
const test_minmax_15_inf: true = {} as Equal<[typeof _15, '<', 'rest'], rc.CompMinMax<'{15,}rest'>>
const test_minmax_2_eq: true = {} as Equal<[typeof _2, '=', 'rest'], rc.CompMinMax<'{2}rest'>>
const test_minmax_15_eq: true = {} as Equal<[typeof _15, '=', 'rest'], rc.CompMinMax<'{15}rest'>>

const test_expand_1_2: true = {} as Equal<['rest', ['?', 'rest']], rc.ExpandMinMax<'rest', typeof _1, typeof _2>>
const test_expand_1_inf: true = {} as Equal<['rest', ['*', 'rest']], rc.ExpandMinMax<'rest', typeof _1, '<'>>
const test_expand_1_15: true = {} as Equal<[
  'rest', 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], 
  ['?', 'rest'], ['?', 'rest'], ['?', 'rest'], ['?', 'rest']
], rc.ExpandMinMax<'rest', typeof _1, typeof _15>>
const test_expand_15_eq: true = {} as Equal<[
  'rest', 'rest', 'rest', 'rest', 'rest', 
  'rest', 'rest', 'rest', 'rest', 'rest', 
  'rest', 'rest', 'rest', 'rest', 'rest'
], rc.ExpandMinMax<'rest', typeof _15, '='>>

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

const _email_regtest0:  ['<', '>', '(', ')', '[', ']', '.', ',', ';', ':', ' ', '@', '"'] = ['<', '>', '(', ')', '[', ']', '.', ',', ';', ':', ' ', '@', '"']

const test_neg_chara_0: true = {} as Equal<
  [['chara-class', typeof _email_regtest0]],
  rc.Comp<'[^<>()[\\].,;: @"]', { negExpand: false }>['tapes']
>

const test_neg_chara_group_0: true = {} as Equal<
  [[ 'group'
    , '\\.'
    , ['chara-class', typeof _email_regtest0]
    , ['*', ['chara-class', typeof _email_regtest0]]]],
  rc.Comp<'(\\.[^<>()[\\].,;: @"]+)', { negExpand: false }>['tapes']
>

const test_email_fst_half_complex: true = {} as Equal<
  [[ 'or-group'
    , [['group', ['chara-class', typeof _email_regtest0]
               , ['*', ['chara-class', typeof _email_regtest0]]
               , [ '*'
                 , [ 'group'
                   , '\\.'
                   , ['chara-class', typeof _email_regtest0]
                   , [ '*'
                     , ['chara-class', typeof _email_regtest0]]]]]]
    , [['group', '\"', '.', ['*', '.'], ['\"']]]]],
  rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))', { negExpand: false }>['tapes']
>

// -- second half part
const _numclass_regtest0: [['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]] = 
  [ [ 'chara-class'
    , [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]
  , [ '?'
    , [ 'chara-class'
      , ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]
  , [ '?'
    , ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]]

const test_ip_address_group: true = {} as Equal<
  [['group', '\\[', typeof _numclass_regtest0, '\\.', typeof _numclass_regtest0, '\\.', typeof _numclass_regtest0, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],
  rc.Comp<'(\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])'>['tapes']
>

const test_word_num_hyphen_class: true = {} as Equal<
  [['chara-class', [...typeof WordList, '-', ...typeof NumList]]],
  rc.Comp<'[a-zA-Z-0-9]'>['tapes']
>

const test_subdomain_plus: true = {} as Equal<
  [['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']]]],
  rc.Comp<'([a-zA-Z0-9-]+\\.)+'>['tapes']
>

const test_domain_full_times: true = {} as Equal<
  [['group', ['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']]], [[['chara-class', typeof WordList], ['chara-class', typeof WordList], ['*', ['chara-class', typeof WordList]]]]]],
  rc.Comp<'(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,})'>['tapes']
>

const test_email_snd_half_or_group: true = {} as Equal<
  [['or-group',[['group', '\\[', typeof _numclass_regtest0, '\\.', typeof _numclass_regtest0, '\\.', typeof _numclass_regtest0, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],[['group', ['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...typeof WordList, ...typeof NumList, '-']], ['*', ['chara-class', [...typeof WordList, ...typeof NumList, '-']]], ['\\.']]], [[['chara-class', typeof WordList], ['chara-class', typeof WordList], ['*', ['chara-class', typeof WordList]]]]]]]],
  rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes']
>


// -- all mail regex
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
const second_end_mailtest: rc.Comp<'((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>['tapes'] =  [['or-group',[['group', '\\[', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', _numclass_regtest0, '\\.', [[['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]], ['?', ['chara-class', ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']]]], '\\]']]],[['group', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']], ['*', ['group', ['chara-class', [...WordList, ...NumList, '-']], ['*', ['chara-class', [...WordList, ...NumList, '-']]], ['\\.']]], [[['chara-class', WordList], ['chara-class', WordList], ['*', ['chara-class', WordList]]]]]]]]

const test_full_email_neg_expand: true = {} as Equal<
  [...typeof first_end_mailtest, '@', ...typeof second_end_mailtest],
  rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))', { negExpand: false }>['tapes']
>

// [todo]
export type mailRegex = rc.Comp<'(([^<>()[\\].,;: @"]+(\\.[^<>()[\\].,;: @"]+)*)|(".+"))@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z0-9-]+\\.)+[a-zA-Z]{2,}))'>

export type * as testrc from './regex-compiler'
