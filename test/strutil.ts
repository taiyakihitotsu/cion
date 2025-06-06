import type strutil from '../src/strutil'
// test
const test_strlen0: strutil.StrLen<'111'> = '0000000000000011'
const test_strlen1: strutil.StrLen<''>    = '0000000000000000'

const test_charat0: strutil.CharAt<'123', '0000000000000001'> = '2'
const test_charat1: strutil.CharAt<'123', '0000000000000111'> = ''
const test_charat2: strutil.CharAt<'', '0000000000000001'> = ''

const test_matchchar0: strutil.MatchChar<'s', 's'> = true
const test_matchchar1: strutil.MatchChar<'', 's'> = false
const test_matchchar2: strutil.MatchChar<'s', ''> = false
const test_matchchar3: strutil.MatchChar<'s', 'ss'> = false

const test_somelen0: strutil.SomeLen<'sss', 'xxa'> = true
const test_somelen1: strutil.SomeLen<'sss', 'sxxa'> = false
const test_somelen2: strutil.SomeLen<'sss', ''> = false
const test_somelen3: strutil.SomeLen<'', 'xxa'> = false
const test_somelen4: strutil.SomeLen<'', ''> = true

const test_charsone0: strutil.StrSearchHead<'sssss', 's'> = ['s', 'ssss']
const test_charsone1: strutil.StrSearchHead<'sssxx', 'xx'> = []
const test_charsone2: strutil.StrSearchHead<'xxsss', 'xx'> = ['xx', 'sss']
const test_charsone3: strutil.StrSearchHead<'xxsss', 'd'> = []
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
const test_allsone1: strutil.StrSearchAll<'sssxx', 'xx'> = ['xx', '']
const test_allsone2: strutil.StrSearchAll<'xxsss', 'xx'> = ['xx', 'sss']
const test_allsone3: strutil.StrSearchAll<'ssxxsss', 'xx'> = ['xx', 'sss']
const test_allsone: strutil.StrSearchAll<'xxsss', 'd'> = []
const test_tallsone0: strutil.StrSearchAll<'sssss', 's', '$'> = ['s', '']
const test_tallsone1: strutil.StrSearchAll<'sssxx', 'xx', '$'> = ['xx', '']
const test_tallsone2: strutil.StrSearchAll<'xxsss', 'xx', '$'> = []
const test_tallsone3: strutil.StrSearchAll<'ssxxsss', 'xx', '$'> = []
const test_tallsone: strutil.StrSearchAll<'xxsss', 'd', '$'> = []
const test_hallsone0: strutil.StrSearchAll<'sssss', 's', '^'> = ['s', 'ssss']
const test_hallsone1: strutil.StrSearchAll<'sssxx', 'xx', '^'> = []
const test_hallsone2: strutil.StrSearchAll<'xxsss', 'xx', '^'> = ['xx', 'sss']
const test_hallsone: strutil.StrSearchAll<'xxsss', 'd', '^'> = []
