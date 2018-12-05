import pyparsing as pp
pc = pp.pyparsing_common

import numpy as np

line = (
	pp.Char('#').suppress() + pc.number('id') +
	pp.Char('@').suppress() + pc.number('l') + pp.Char(',').suppress() + pc.number('t') +
	pp.Char(':').suppress() + pc.number('w') + pp.Char('x').suppress() + pc.number('h')
)

with open('3.in', 'r') as f:
	d = [line.parseString(i) for i in f.readlines()]

m = np.zeros((max(i.w + i.l for i in d), max(i.h + i.t for i in d)))
for i in d: m[i.l:i.w+i.l, i.t:i.t+i.h] += 1
print(np.sum(m >= 2))

