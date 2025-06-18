import type * as strutil from '../src/strutil'


const test_regcut: strutil.RegCut<'{1,}))rest'> = ['{', '1,}))rest']

const test_strlen0: strutil.StrLen<'111'> = '0000000000000011'
const test_strlen1: strutil.StrLen<''>    = '0000000000000000'

const test_charat0: strutil.CharAt<'123', '0000000000000001'> = '2'
const test_charat1: strutil.CharAt<'123', '0000000000000111'> = ''
const test_charat2: strutil.CharAt<'', '0000000000000001'> = ''

const test_matchchar0: strutil.MatchChar<'s', 's'> = true
const test_matchchar1: strutil.MatchChar<'', 's'> = false
const test_matchchar2: strutil.MatchChar<'s', ''> = false
const test_matchchar3: strutil.MatchChar<'s', 'ss'> = false
const test_matchchar0x: strutil.MatchChar<'s', '.'> = false
const test_matchchar3x: strutil.MatchChar<'s', '\\.'> = false

const test_somelen0: strutil.SomeLen<'sss', 'xxa'> = true
const test_somelen1: strutil.SomeLen<'sss', 'sxxa'> = false
const test_somelen2: strutil.SomeLen<'sss', ''> = false
const test_somelen3: strutil.SomeLen<'', 'xxa'> = false
const test_somelen4: strutil.SomeLen<'', ''> = true

const test_strtake0: strutil.StrTake<'012345', '0000000000000011'> = '012'
const test_strtake1: strutil.StrTake<'012345', '0000000000000000'> = ''
const test_strtake2: strutil.StrTake<'012345', '0000000000001111'> = '012345'

const test_strDrop0: strutil.StrDrop<'012345', '0000000000000011'> = '345'
const test_strDrop1: strutil.StrDrop<'012345', '0000000000000000'> = '012345'
const test_strDrop2: strutil.StrDrop<'012345', '0000000000001111'> = ''

const test_charsone0: strutil.StrSearchHead<'sssss', 's'> = ['s', 'ssss']
const test_charsone1: strutil.StrSearchHead<'sssxx', 'xx'> = []
const test_charsone2: strutil.StrSearchHead<'xxsss', 'xx'> = ['xx', 'sss']
const test_charsone3: strutil.StrSearchHead<'xxsss', 'd'> = []
const test_charsone2x: strutil.StrSearchHead<'xxsss', '.'> = ['x', 'xsss']
const test_charsone3x: strutil.StrSearchHead<'xxsss', '\\.'> = []

const test_charsoned0: strutil.MatchChar<'1', '\\d'> = true
const test_charsoned1: strutil.MatchChar<'0', '\\d'> = true
const test_charsoned2: strutil.MatchChar<'s', '\\d'> = false
const test_charsoned3: strutil.MatchChar<'1', '\\w'> = true
const test_charsoned4: strutil.MatchChar<'a', '\\w'> = true
const test_charsoned5: strutil.MatchChar<'!', '\\w'> = false
const test_charsoned0a: strutil.MatchChar<'1', '\\u'> = false
const test_charsoned1a: strutil.MatchChar<'d', '\\u'> = false
const test_charsoned2a: strutil.MatchChar<'D', '\\u'> = true
const test_charsoned3a: strutil.MatchChar<'1', '\\l'> = false
const test_charsoned4a: strutil.MatchChar<'d', '\\l'> = true
const test_charsoned5a: strutil.MatchChar<'D', '\\l'> = false
const test_charsonedx0: strutil.MatchChar<'1', '\\D'> = false
const test_charsonedx1: strutil.MatchChar<'0', '\\D'> = false
const test_charsonedx2: strutil.MatchChar<'s', '\\D'> = true
const test_charsonedx3: strutil.MatchChar<'1', '\\W'> = false
const test_charsonedx4: strutil.MatchChar<'a', '\\W'> = false
const test_charsonedx5: strutil.MatchChar<'!', '\\W'> = true
const test_charsonedx0a: strutil.MatchChar<'1', '\\U'> = true
const test_charsonedx1a: strutil.MatchChar<'d', '\\U'> = true
const test_charsonedx2a: strutil.MatchChar<'D', '\\U'> = false
const test_charsonedx3a: strutil.MatchChar<'1', '\\L'> = true
const test_charsonedx4a: strutil.MatchChar<'d', '\\L'> = true
const test_charsonedx5a: strutil.MatchChar<'D', '\\L'> = false

const test_charsone4: strutil.StrSearchHead<'xxsss', 'xs'> = []
const test_charsone: strutil.StrSearchHead<'xsss', 'xs'> = ['xs', 'ss']
const dot_test_charsone0: strutil.StrSearchHead<'sssss', '.s'> = ['ss', 'sss']
const dot_test_charsone1: strutil.StrSearchHead<'sssxx', '..xx'> = []
const dot_test_charsone1b: strutil.StrSearchHead<'sssxx', '...xx'> = ['sssxx', '']
const dot_test_charsone1c: strutil.StrSearchHead<'sssxx', '.xx'> = []
const dot_test_charsone2: strutil.StrSearchHead<'xxsss', 'xx.'> = ['xxs', 'ss']
const dot_test_charsone3: strutil.StrSearchHead<'xxsss', '.d'> = []
const dot_test_charsone4: strutil.StrSearchHead<'xxsss', 'x.s'> = ['xxs', 'ss']
const dot_test_charsone4b: strutil.StrSearchHead<'xxsss', '.xs'> = ['xxs', 'ss']
const dot_test_charsone5: strutil.StrSearchHead<'xsss', 'xs..'> = ['xsss', '']
const dot_test_charsone: strutil.StrSearchHead<'x🦊sss', 'x🦊s..'> = ['x🦊sss', ''] // 🦊🦊
// test
const test_allsone0: strutil.StrSearchAll<'sssss', 's'> =  ['s', 'ssss']
const test_allsone1: strutil.StrSearchAll<'sssxx', 'xx'> = ['sssxx', '']
const test_allsone2: strutil.StrSearchAll<'xxsss', 'xx'> = ['xx', 'sss']
const test_allsone2a: strutil.StrSearchAll<'xxxsss', 'xx'> = ['xx', 'xsss']
const test_allsone3: strutil.StrSearchAll<'ssxxsss', 'xx'> = ['ssxx', 'sss']
const test_allsone: strutil.StrSearchAll<'xxsss', 'd'> = []
const test_tallsone0: strutil.StrSearchAll<'sssss', 's', '$'> = ['sssss', '']
const test_tallsone1: strutil.StrSearchAll<'sssxx', 'xx', '$'> = ['sssxx', '']
const test_tallsone2: strutil.StrSearchAll<'xxsss', 'xx', '$'> = []
const test_tallsone3: strutil.StrSearchAll<'ssxxsss', 'xx', '$'> = []
const test_tallsone: strutil.StrSearchAll<'xxsss', 'd', '$'> = []
const test_hallsone0: strutil.StrSearchAll<'sssss', 's', '^'> = ['s', 'ssss']
const test_hallsone1: strutil.StrSearchAll<'sssxx', 'xx', '^'> = []
const test_hallsone2: strutil.StrSearchAll<'xxsss', 'xx', '^'> = ['xx', 'sss']
const test_hallsone: strutil.StrSearchAll<'xxsss', 'd', '^'> = []

const test_strinter0: strutil.StrInter<'0123456789', '0000000000000010', '0000000000000101'> = '2345'
const test_strinter1: strutil.StrInter<'0123456789', '0000000000000000', '0000000000000101'> = '012345'
const test_strinter2: strutil.StrInter<'0123456789', '0000000000000010', '0000000000001111'> = '23456789'
