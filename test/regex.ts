import type { regex } from '../src/regex'
import type * as regexConst from '../src/regex-const'

// regex loop
const evaltesttms_comp0vbzf0: regex.RegexFind<'szds', 's(z|d){0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzf2: regex.RegexFind<'xszds', 's(z|d){0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzf2a: regex.RegexFind<'xsds', 's(z|d){0,2}s'> = ['sds', '']
const evaltesttms_comp0vbzf4: regex.RegexFind<'xszds', 's(z|d){0,2}s$'> = ['szds', '']
const evaltettms_comp0vbzf4b: regex.RegexFind<'xszds', '^s(z|d){0,2}s'> = []
const evaltesttms_comp0vbzf6: regex.RegexFind<'xszds', 's(z|d){0,2}s'> = ['szds', '']

const evaltesttms_compdke0: regex.RegexFind<'xk?2', 'xk[d?]2'> = ['xk?2', '']
const evaltesttms_compdke1: regex.RegexFind<'xk?2', 'x[k?]{0,2}2'> = ['xk?2', '']
const evaltesttms_compdke2: regex.RegexFind<'xk?2', 'x(k\\?)2'> = ['xk?2', '']
const evaltesttms_compdke3: regex.RegexFind<'ddd12d1d', 'ddd\\d\\dd\\dd'> = ['ddd12d1d', '']
const evaltesttms_compdke4: regex.RegexFind<'xkd32', 'xk(d\\d){1,2}2'> = ['xkd32', '']
const evaltesttms_compdke5: regex.RegexFind<'xkd32', 'xk(d|\\d)2'> = []
const evaltesttms_compdke6: regex.RegexFind<'xkd345?2', 'xk(d|\\d){1,4}\\?2'> = ['xkd345?2', '']

const evaltesttms_regg3x: regex.RegexFind<'sz1s', 'sz\\d'> = ['sz1', 's']
const evaltesttms_comp0vbzfx0:  regex.RegexFind<'szds', 's(z|\\d){0,2}s'>  = []
const evaltesttms_comp0vbzfx0a: regex.RegexFind<'szds', 's[z\\d]{0,2}s'>   = []
const evaltesttms_comp0vbzfx2:  regex.RegexFind<'xszds', 's[zd\\d]{0,2}s'> = ['szds', '']
const evaltesttms_comp0vbzfx2a: regex.RegexFind<'s12s', 's[zd\\d]{0,2}s'>  = ['s12s', '']
const evaltesttms_comp0vbzfx2b: regex.RegexFind<'s12s', 's[zd12]{0,2}s'>   = ['s12s', '']
const evaltesttms_comp0vbzfx4:  regex.RegexFind<'xszds', 's(z|d){0,2}s$'>  = ['szds', '']
const evaltesttms_comp0vbzfx4b: regex.RegexFind<'xszds', '^s(z|d){0,2}s'>  = []
const evaltesttms_comp0vbzfx6:  regex.RegexFind<'xszds', 's(z|d){0,2}s'>   = ['szds', '']

const evaltesttms_comp0vbzfx61:  regex.RegexFind<'xs', 's[a-c]s'> = []
const evaltesttms_comp0vbzfx61a:  regex.RegexFind<'scs', 's[a-c]s'> = ['scs', '']
const evaltesttms_comp0vbzfx61b:  regex.RegexFind<'sbs', 's[a-c]s'> = ['sbs', '']
const evaltesttms_comp0vbzfx61c:  regex.RegexFind<'sas', 's[a-c]s'> = ['sas', '']
const evaltesttms_comp0vbzfx61d:  regex.RegexFind<'sbacs', 's[abc]{0,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61e:  regex.RegexFind<'sbacs', 's[abc]{1,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61f:  regex.RegexFind<'sbacs', 's[abc]{2,3}s'> = ['sbacs', '']
const evaltesttms_comp0vbzfx61fb:  regex.RegexFind<'sabcs', 's[abc]{3,3}s'> = ['sabcs', '']
const evaltesttms_comp0vbzfx61g:  regex.RegexFind<'sbbacs', 's[abc]{2,4}s'> = ['sbbacs', '']
const evaltesttms_comp0vbzfx61h:  regex.RegexFind<'sbcs', 's[abc]{2,4}s'> = ['sbcs', '']
const evaltesttms_comp0vbzfx61f1:  regex.RegexFind<'sbas', 's[abc]+s'> = ['sbas', '']
const evaltesttms_comp0vbzfx61i:  regex.RegexFind<'sbbs', 's[abc]{2,4}s'> = ['sbbs', '']
const evaltesttms_comp0vbzfx61i1:  regex.RegexFind<'sbbbs', 's[abc]{2,4}s'> = ['sbbbs', '']
const evaltesttms_comp0vbzfx61i2:  regex.RegexFind<'sbbbbs', 's[abc]{2,4}s'> = ['sbbbbs', '']
const evaltesttms_comp0vbzfx61i3:  regex.RegexFind<'sbbbbbs', 's[abc]{2,4}s'> = []

// ------------
// -- mail
// ------------
const test_email_regex0: regex.RegexFind<'dajlaed@exampletest.com', regexConst.EmailRegex> = ['dajlaed@exampletest.com', '']
const test_email_regex1: regex.RegexFind<'user+mailbox/adjke=ddddafmadiod@example.co.uk', regexConst.EmailRegex> = ['user+mailbox/adjke=ddddafmadiod@example.co.uk', '']















