import type { regex } from '../../src/regex'
import type * as regexConst from '../../src/regex-const'
import type { Equal } from '../../src/util'

// --- regex loop ---
const evaltesttms_comp0vbzf0: true = {} as Equal<['', 'szds', ''], regex.RegexFind<'szds', 's(z|d){0,2}s'>>
const evaltesttms_comp0vbzf2: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's(z|d){0,2}s'>>
const evaltesttms_comp0vbzf2a: true = {} as Equal<['x', 'sds', ''], regex.RegexFind<'xsds', 's(z|d){0,2}s'>>
const evaltesttms_comp0vbzf4: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's(z|d){0,2}s$'>>
const evaltettms_comp0vbzf4b: true = {} as Equal<[], regex.RegexFind<'xszds', '^s(z|d){0,2}s'>>
const evaltesttms_comp0vbzf6: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's(z|d){0,2}s'>>

const evaltesttms_f_comp0vbzf0: true = {} as Equal<['', 'szds', ''], regex.RegexFind<'szds', '^s(z|d){0,2}s'>>
const evaltesttms_ff_comp0vbzf0: true = {} as Equal<['', 'szds', ''], regex.RegexFind<'szds', '^s(z|d){0,2}s$'>>
const evaltesttms_fr_comp0vbzf0: true = {} as Equal<['', 'szds', 'ye9r'], regex.RegexFind<'szdsye9r', '^s(z|d){0,2}s'>>
const evaltesttms_frb_comp0vbzf0: true = {} as Equal<[], regex.RegexFind<'szdsye9r', '^s(z|d){0,2}s$'>>
const evaltesttms_f_comp0vbzf2: true = {} as Equal<[], regex.RegexFind<'xszds', '^s(z|d){0,2}s'>>
const evaltesttms_f_comp0vbzf2a: true = {} as Equal<[], regex.RegexFind<'xsds', '^s(z|d){0,2}s'>>
const evaltesttms_f_comp0vbzf4: true = {} as Equal<[], regex.RegexFind<'xszds', '^s(z|d){0,2}s$'>>
const evaltesttms_f_comp0vbzf6: true = {} as Equal<[], regex.RegexFind<'xszds', '^s(z|d){0,2}s'>>

const evaltesttms_compdke0: true = {} as Equal<['', 'xk?2', ''], regex.RegexFind<'xk?2', 'xk[d?]2'>>
const evaltesttms_compdke1: true = {} as Equal<['', 'xk?2', ''], regex.RegexFind<'xk?2', 'x[k?]{0,2}2'>>
const evaltesttms_compdke2: true = {} as Equal<['', 'xk?2', ''], regex.RegexFind<'xk?2', 'x(k\\?)2'>>
const evaltesttms_compdke3: true = {} as Equal<['', 'ddd12d1d', ''], regex.RegexFind<'ddd12d1d', 'ddd\\d\\dd\\dd'>>
const evaltesttms_compdke4: true = {} as Equal<['', 'xkd32', ''], regex.RegexFind<'xkd32', 'xk(d\\d){1,2}2'>>
const evaltesttms_compdke5: true = {} as Equal<[], regex.RegexFind<'xkd32', 'xk(d|\\d)2'>>
const evaltesttms_compdke6: true = {} as Equal<['', 'xkd345?2', ''], regex.RegexFind<'xkd345?2', 'xk(d|\\d){1,4}\\?2'>>

const evaltesttms_regg3x: true = {} as Equal<['', 'sz1', 's'], regex.RegexFind<'sz1s', 'sz\\d'>>
const evaltesttms_comp0vbzfx0: true = {} as Equal<[], regex.RegexFind<'szds', 's(z|\\d){0,2}s'>>
const evaltesttms_comp0vbzfx0a: true = {} as Equal<[], regex.RegexFind<'szds', 's[z\\d]{0,2}s'>>
const evaltesttms_comp0vbzfx2: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's[zd\\d]{0,2}s'>>
const evaltesttms_comp0vbzfx2a: true = {} as Equal<['', 's12s', ''], regex.RegexFind<'s12s', 's[zd\\d]{0,2}s'>>
const evaltesttms_comp0vbzfx2b: true = {} as Equal<['', 's12s', ''], regex.RegexFind<'s12s', 's[zd12]{0,2}s'>>
const evaltesttms_comp0vbzfx4: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's(z|d){0,2}s$'>>
const evaltesttms_comp0vbzfx4b: true = {} as Equal<[], regex.RegexFind<'xszds', '^s(z|d){0,2}s'>>
const evaltesttms_comp0vbzfx6: true = {} as Equal<['x', 'szds', ''], regex.RegexFind<'xszds', 's(z|d){0,2}s'>>

const evaltesttms_comp0vbzfx61: true = {} as Equal<[], regex.RegexFind<'xs', 's[a-c]s'>>
const evaltesttms_comp0vbzfx61a: true = {} as Equal<['', 'scs', ''], regex.RegexFind<'scs', 's[a-c]s'>>
const evaltesttms_comp0vbzfx61b: true = {} as Equal<['', 'sbs', ''], regex.RegexFind<'sbs', 's[a-c]s'>>
const evaltesttms_comp0vbzfx61c: true = {} as Equal<['', 'sas', ''], regex.RegexFind<'sas', 's[a-c]s'>>
const evaltesttms_comp0vbzfx61d: true = {} as Equal<['', 'sbacs', ''], regex.RegexFind<'sbacs', 's[abc]{0,3}s'>>
const evaltesttms_comp0vbzfx61e: true = {} as Equal<['', 'sbacs', ''], regex.RegexFind<'sbacs', 's[abc]{1,3}s'>>
const evaltesttms_comp0vbzfx61f: true = {} as Equal<['', 'sbacs', ''], regex.RegexFind<'sbacs', 's[abc]{2,3}s'>>
const evaltesttms_comp0vbzfx61fb: true = {} as Equal<['', 'sabcs', ''], regex.RegexFind<'sabcs', 's[abc]{3,3}s'>>
const evaltesttms_comp0vbzfx61g: true = {} as Equal<['', 'sbbacs', ''], regex.RegexFind<'sbbacs', 's[abc]{2,4}s'>>
const evaltesttms_comp0vbzfx61h: true = {} as Equal<['', 'sbcs', ''], regex.RegexFind<'sbcs', 's[abc]{2,4}s'>>
const evaltesttms_comp0vbzfx61f1: true = {} as Equal<['', 'sbas', ''], regex.RegexFind<'sbas', 's[abc]+s'>>
const evaltesttms_comp0vbzfx61i: true = {} as Equal<['', 'sbbs', ''], regex.RegexFind<'sbbs', 's[abc]{2,4}s'>>
const evaltesttms_comp0vbzfx61i1: true = {} as Equal<['', 'sbbbs', ''], regex.RegexFind<'sbbbs', 's[abc]{2,4}s'>>
const evaltesttms_comp0vbzfx61i2: true = {} as Equal<['', 'sbbbbs', ''], regex.RegexFind<'sbbbbs', 's[abc]{2,4}s'>>
const evaltesttms_comp0vbzfx61i3: true = {} as Equal<[], regex.RegexFind<'sbbbbbs', 's[abc]{2,4}s'>>

// --- mail ---
const test_email_regex0: true = {} as Equal<
  ['', 'dajlaed@exampletest.com', ''], 
  regex.RegexFind<'dajlaed@exampletest.com', regexConst.EmailRegex>
>
const test_email_regex1: true = {} as Equal<
  ['', 'user+mailbox/adjke=ddddafmadiod@example.co.uk', ''], 
  regex.RegexFind<'user+mailbox/adjke=ddddafmadiod@example.co.uk', regexConst.EmailRegex>
>
