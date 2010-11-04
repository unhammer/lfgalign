#!/usr/bin/python
# coding=utf-8
# -*- encoding: utf-8 -*-

## Script to turn the Ding en-de dictionary into an LPT table, in
## Prolog format. Just outputs the cartesian product of all possible
## variants; the LPT representation is rather stupid at the moment.
## Ding is available from http://www-user.tu-chemnitz.de/~fri/ding/
## under the GNU GPL.

## Usage:
# $ cat /usr/share/dict/de-en.txt | python2 dev/ding-to-LPT.py > de-en-LPT.pl


import sys, codecs, copy, commands, string, re

sys.stdin  = codecs.getreader('utf-8')(sys.stdin);
sys.stdout = codecs.getwriter('utf-8')(sys.stdout);
sys.stderr = codecs.getwriter('utf-8')(sys.stderr);

def xproduct(left, right):
	prod = []
	for i in left:
		for j in right:
			prod += [ ( i, j ) ]
	return prod

def outprod(xprod):
	# simplest to write them out in Prolog form for now, since we already read ria-alignments etc. in that form anyway
	for pair in xprod:
		sys.stdout.write("LPT( '")
		sys.stdout.write(string.strip(string.replace(pair[0],"'", "\\'")))
		sys.stdout.write("', '")
		sys.stdout.write(string.strip(string.replace(pair[1],"'", "\\'")))
		sys.stdout.write("').\n")

def splitsenses(lsenses, rsenses, nline):
	# For each line we may have several "senses" (but often these are just different forms, like sg/pl)
	# For each "sense", we may have different variants, separated by ;
	if len(lsenses) != len(rsenses):
		sys.stderr.write("Warning: number of senses did not match on line " + str(nline) + "\n")
		return
	i = 0;
	while i < len(lsenses):
		lvariant = string.split(lsenses[i],";")
		rvariant = string.split(rsenses[i],";")
		outprod(xproduct(lvariant, rvariant))
		i += 1

nline=0
while True:
	nline += 1
	line = sys.stdin.readline()[:-1] # remove trailing newline
	if not line: break
	if re.match("^ *#", line): continue
	
	# Remove comments, grammatical taggings, etc.
	line = re.sub("{[^}]*}", "",
		      re.sub("\([^\)]*\)", "",
			     re.sub("\[[^\]]*\]", "", line)))

	# Languages are on each side of ::
	both = string.split(line,"::")
	if len(both) != 2:
		sys.stderr.write("Warning: did not find exactly two languages on line " + str(nline) + "\n")
		continue
	lhs = both[0]; rhs = both[1]
	lsenses = string.split(lhs,"|")
	rsenses = string.split(rhs,"|")

	splitsenses(lsenses, rsenses, nline)

