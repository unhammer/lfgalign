fstructure('Wir meinen ebenfalls, dass Minderjährige nicht in die gemeinsame Regelung einbezogen werden sollen.',
	% Properties:
	[
	'file'('Ger-train-sents/S65344.txt'),
	'first_byte_pos'('1'),
	'last_byte_pos'('100'),
	'xle_version'('XLE release of Aug 01, 2007 15:00.'),
	'grammar'('/home/ygraham/Grammars/mt-FROZEN-23jun04/german.lfg'),
	'grammar_date'('Aug 31, 2007 11:04'),
	'word_count'('13'),
	'statistics'('1+39 solutions, 0.30 CPU seconds, 624 subtrees unified'),
	'rootcategory'('ROOT')
	],
	% Choices:
	[
	
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('meinen',6,[var(38),var(6)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(38))),
	cf(1,eq(attr(var(0),'COMP'),var(6))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(3))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(40))),
	cf(1,eq(attr(var(0),'TOPIC'),var(38))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'PASSIVE'),'-')),
	cf(1,eq(attr(var(0),'STMT-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VTYPE'),'main')),
	cf(1,eq(attr(var(38),'PRED'),semform('pro',1,[],[]))),
	cf(1,eq(attr(var(38),'NTYPE'),var(39))),
	cf(1,eq(attr(var(38),'CASE'),'nom')),
	cf(1,eq(attr(var(38),'NUM'),'pl')),
	cf(1,eq(attr(var(38),'PERS'),'1')),
	cf(1,eq(attr(var(38),'PRON-FORM'),'sie')),
	cf(1,eq(attr(var(38),'PRON-TYPE'),'pers')),
	cf(1,eq(attr(var(39),'NSYN'),'pronoun')),
	cf(1,eq(attr(var(6),'PRED'),semform('sollen',46,[var(20)],[var(11)]))),
	cf(1,eq(attr(var(6),'SUBJ'),var(11))),
	cf(1,eq(attr(var(6),'XCOMP'),var(20))),
	cf(1,eq(attr(var(6),'ADJUNCT'),var(7))),
	cf(1,eq(attr(var(6),'CHECK'),var(9))),
	cf(1,eq(attr(var(6),'TNS-ASP'),var(19))),
	cf(1,eq(attr(var(6),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(6),'COMP-FORM'),'dass')),
	cf(1,eq(attr(var(6),'PASSIVE'),'-')),
	cf(1,eq(attr(var(6),'VTYPE'),'modal')),
	cf(1,eq(attr(var(11),'PRED'),semform('pro',13,[],[]))),
	cf(1,eq(attr(var(11),'PRED-RESTR'),var(15))),
	cf(1,eq(attr(var(11),'CHECK'),var(12))),
	cf(1,eq(attr(var(11),'NTYPE'),var(13))),
	cf(1,eq(attr(var(11),'CASE'),'nom')),
	cf(1,eq(attr(var(11),'NUM'),'pl')),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(1,eq(attr(var(11),'PRON-TYPE'),'null')),
	cf(1,eq(attr(var(15),'PRED'),semform('minderjährig',11,[var(11)],[]))),
	cf(1,eq(attr(var(15),'SUBJ'),var(11))),
	cf(1,eq(attr(var(15),'CHECK'),var(16))),
	cf(1,eq(attr(var(15),'ATYPE'),'attributive')),
	cf(1,eq(attr(var(15),'DEG-DIM'),'pos')),
	cf(1,eq(attr(var(15),'DEGREE'),'positive_')),
	cf(1,eq(attr(var(16),'_MORPH'),var(17))),
	cf(1,eq(attr(var(17),'_DERIV'),var(18))),
	cf(1,eq(attr(var(17),'_CAPITAL'),'+')),
	cf(1,eq(attr(var(18),'_ADJ'),'+')),
	cf(1,eq(attr(var(12),'_INFL'),'strong-adj')),
	cf(1,eq(attr(var(13),'NSEM'),var(14))),
	cf(1,eq(attr(var(13),'NSYN'),'pronoun')),
	cf(1,eq(attr(var(14),'COMMON'),'count')),
	cf(1,eq(attr(var(20),'PRED'),semform('ein#beziehen',40,['NULL',var(11),var(26)],[]))),
	cf(1,eq(attr(var(20),'SUBJ'),var(11))),
	cf(1,eq(attr(var(20),'OBL'),var(25))),
	cf(1,eq(attr(var(20),'CHECK'),var(21))),
	cf(1,eq(attr(var(20),'TNS-ASP'),var(37))),
	cf(1,eq(attr(var(20),'PASSIVE'),'+')),
	cf(1,eq(attr(var(20),'VTYPE'),'main')),
	cf(1,eq(attr(var(25),'PRED'),semform('in',23,[var(26)],[]))),
	cf(1,eq(attr(var(25),'OBJ'),var(26))),
	cf(1,eq(attr(var(25),'PSEM'),'dir')),
	cf(1,eq(attr(var(25),'PTYPE'),'nosem')),
	cf(1,eq(attr(var(26),'PRED'),semform('Regelung',35,[],[]))),
	cf(1,eq(attr(var(26),'ADJUNCT'),var(27))),
	cf(1,eq(attr(var(26),'CHECK'),var(31))),
	cf(1,eq(attr(var(26),'NTYPE'),var(34))),
	cf(1,eq(attr(var(26),'SPEC'),var(35))),
	cf(1,eq(attr(var(26),'CASE'),'acc')),
	cf(1,eq(attr(var(26),'GEND'),'fem')),
	cf(1,eq(attr(var(26),'NUM'),'sg')),
	cf(1,eq(attr(var(26),'PERS'),'3')),
	cf(1,in_set(var(28),var(27))),
	cf(1,eq(attr(var(28),'PRED'),semform('gemeinsam',31,[var(26)],[]))),
	cf(1,eq(attr(var(28),'SUBJ'),var(26))),
	cf(1,eq(attr(var(28),'CHECK'),var(29))),
	cf(1,eq(attr(var(28),'ATYPE'),'attributive')),
	cf(1,eq(attr(var(28),'DEG-DIM'),'pos')),
	cf(1,eq(attr(var(28),'DEGREE'),'positive_')),
	cf(1,eq(attr(var(29),'_MORPH'),var(30))),
	cf(1,eq(attr(var(30),'_CAPITAL'),'-')),
	cf(1,eq(attr(var(31),'_NCONSTR'),var(32))),
	cf(1,eq(attr(var(31),'_SPEC-TYPE'),var(33))),
	cf(1,eq(attr(var(31),'_INFL'),'strong-det')),
	cf(1,eq(attr(var(32),'_ADJ-ATTR'),'+')),
	cf(1,eq(attr(var(33),'_COUNT'),'+')),
	cf(1,eq(attr(var(33),'_DEF'),'+')),
	cf(1,eq(attr(var(33),'_DET'),'attr')),
	cf(1,eq(attr(var(34),'NSYN'),'common')),
	cf(1,eq(attr(var(35),'DET'),var(36))),
	cf(1,eq(attr(var(36),'PRED'),semform('die',26,[],[]))),
	cf(1,eq(attr(var(36),'DET-TYPE'),'def')),
	cf(1,eq(attr(var(21),'_AUX-FORM'),var(22))),
	cf(1,eq(attr(var(21),'_VLEX'),var(23))),
	cf(1,eq(attr(var(21),'_VMORPH'),var(24))),
	cf(1,in_set('werden-pass_',var(22))),
	cf(1,eq(attr(var(23),'_AUX-SELECT'),'sein')),
	cf(1,eq(attr(var(24),'_INF'),'+')),
	cf(1,eq(attr(var(24),'_INF-FORM'),'bare')),
	cf(1,eq(attr(var(24),'_PART-VERB'),'+')),
	cf(1,eq(attr(var(24),'_PARTICIPLE'),'perfect')),
	cf(1,eq(attr(var(24),'_PARTICLE'),'attached')),
	cf(1,eq(attr(var(37),'PASS-SEM'),'dynamic_')),
	cf(1,in_set(var(8),var(7))),
	cf(1,eq(attr(var(8),'PRED'),semform('nicht',20,[],[]))),
	cf(1,eq(attr(var(8),'ADJUNCT-TYPE'),'negation')),
	cf(1,eq(attr(var(9),'_VLEX'),var(10))),
	cf(1,eq(attr(var(10),'_AUX-FLIP'),'+')),
	cf(1,eq(attr(var(10),'_AUX-SELECT'),'haben')),
	cf(1,eq(attr(var(10),'_COHERENT'),'+')),
	cf(1,eq(attr(var(19),'TENSE'),'pres')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('ebenfalls',8,[],[]))),
	cf(1,eq(attr(var(2),'ADV-TYPE'),'unspec')),
	cf(1,eq(attr(var(3),'_VLEX'),var(4))),
	cf(1,eq(attr(var(3),'_VMORPH'),var(5))),
	cf(1,eq(attr(var(4),'_AUX-SELECT'),'haben')),
	cf(1,eq(attr(var(4),'_COHERENT'),'-')),
	cf(1,eq(attr(var(5),'_PART-VERB'),'-')),
	cf(1,eq(attr(var(40),'TENSE'),'pres')),
	cf(1,eq(attr(var(45),'RIGHT_SISTER'),var(46))),
	cf(1,eq(proj(var(45),'o::'),var(69))),
	cf(1,eq(attr(var(46),'LEFT_SISTER'),var(45))),
	cf(1,eq(attr(var(46),'RIGHT_DAUGHTER'),var(47))),
	cf(1,eq(attr(var(47),'LEFT_SISTER'),var(48))),
	cf(1,eq(attr(var(48),'LEFT_SISTER'),var(49))),
	cf(1,eq(attr(var(48),'RIGHT_DAUGHTER'),var(50))),
	cf(1,eq(attr(var(48),'RIGHT_SISTER'),var(47))),
	cf(1,eq(proj(var(48),'o::'),var(68))),
	cf(1,eq(attr(var(49),'RIGHT_SISTER'),var(48))),
	cf(1,eq(attr(var(50),'RIGHT_DAUGHTER'),var(51))),
	cf(1,eq(attr(var(51),'LEFT_SISTER'),var(52))),
	cf(1,eq(attr(var(51),'RIGHT_DAUGHTER'),var(53))),
	cf(1,eq(attr(var(52),'RIGHT_SISTER'),var(51))),
	cf(1,eq(attr(var(53),'RIGHT_DAUGHTER'),var(54))),
	cf(1,eq(attr(var(54),'LEFT_SISTER'),var(55))),
	cf(1,eq(attr(var(54),'RIGHT_DAUGHTER'),var(56))),
	cf(1,eq(attr(var(55),'RIGHT_SISTER'),var(54))),
	cf(1,eq(attr(var(56),'LEFT_SISTER'),var(57))),
	cf(1,eq(attr(var(56),'RIGHT_DAUGHTER'),var(59))),
	cf(1,eq(attr(var(57),'RIGHT_SISTER'),var(56))),
	cf(1,eq(proj(var(57),'o::'),var(58))),
	cf(1,in_set('AdvPAdjunct',var(58))),
	cf(1,eq(attr(var(59),'LEFT_SISTER'),var(60))),
	cf(1,eq(attr(var(60),'RIGHT_DAUGHTER'),var(61))),
	cf(1,eq(attr(var(60),'RIGHT_SISTER'),var(59))),
	cf(1,eq(attr(var(61),'RIGHT_DAUGHTER'),var(62))),
	cf(1,eq(attr(var(62),'LEFT_SISTER'),var(63))),
	cf(1,eq(attr(var(62),'RIGHT_DAUGHTER'),var(64))),
	cf(1,eq(attr(var(63),'RIGHT_SISTER'),var(62))),
	cf(1,eq(attr(var(64),'LEFT_SISTER'),var(65))),
	cf(1,eq(attr(var(64),'RIGHT_DAUGHTER'),var(66))),
	cf(1,eq(attr(var(66),'LEFT_SISTER'),var(67))),
	cf(1,eq(attr(var(67),'RIGHT_SISTER'),var(66))),
	cf(1,in_set('ClauseExtraposed',var(68))),
	cf(1,in_set('AdvPAdjunct',var(69))),
	cf(1,eq(attr(var(71),'RIGHT_DAUGHTER'),var(72))),
	cf(1,eq(attr(var(71),'RIGHT_SISTER'),var(70))),
	cf(1,eq(attr(var(72),'RIGHT_DAUGHTER'),var(46))),
	cf(1,eq(attr(var(70),'LEFT_SISTER'),var(71))),
	cf(1,eq(proj(var(41),'o::'),var(42))),
	cf(1,in_set('DieAsDet',var(42))),
	cf(1,eq(proj(var(43),'o::'),var(44))),
	cf(1,in_set('WerdenAsAux',var(44)))
	],
	% C-Structure:
	[
	cf(1,subtree(5493,'ROOT',6498,668)),
	cf(1,phi(5493,var(0))),
	cf(1,subtree(6498,'ROOT',-,6493)),
	cf(1,phi(6498,var(0))),
	cf(1,subtree(6493,'CProot[std]',6432,6491)),
	cf(1,phi(6493,var(0))),
	cf(1,cproj(6493,var(71))),
	cf(1,subtree(6491,'Cbar',6465,6485)),
	cf(1,phi(6491,var(0))),
	cf(1,cproj(6491,var(72))),
	cf(1,subtree(6485,'NACHFELD',6484,648)),
	cf(1,phi(6485,var(0))),
	cf(1,cproj(6485,var(46))),
	cf(1,subtree(6484,'NACHFELD',6471,5435)),
	cf(1,phi(6484,var(0))),
	cf(1,cproj(6484,var(46))),
	cf(1,subtree(6471,'NACHFELD',-,182)),
	cf(1,phi(6471,var(0))),
	cf(1,subtree(6465,'Cbar',6454,1121)),
	cf(1,phi(6465,var(0))),
	cf(1,subtree(6454,'Cbar',-,6452)),
	cf(1,phi(6454,var(0))),
	cf(1,subtree(6452,'V[v,fin]',-,6451)),
	cf(1,phi(6452,var(0))),
	cf(1,subtree(6451,'Vx[v,fin]',6450,138)),
	cf(1,phi(6451,var(0))),
	cf(1,subtree(6450,'Vx[v,fin]',6449,142)),
	cf(1,phi(6450,var(0))),
	cf(1,subtree(6449,'Vx[v,fin]',6448,143)),
	cf(1,phi(6449,var(0))),
	cf(1,subtree(6448,'Vx[v,fin]',6447,145)),
	cf(1,phi(6448,var(0))),
	cf(1,subtree(6447,'Vx[v,fin]',6445,147)),
	cf(1,phi(6447,var(0))),
	cf(1,subtree(6445,'Vx[v,fin]',-,117)),
	cf(1,phi(6445,var(0))),
	cf(1,subtree(6432,'CProot[std]',-,1853)),
	cf(1,phi(6432,var(0))),
	cf(1,subtree(5890,'VPx[coh,fin]',5876,5363)),
	cf(1,phi(5890,var(6))),
	cf(1,cproj(5890,var(56))),
	cf(1,subtree(5876,'VPx[coh,fin]',-,3971)),
	cf(1,phi(5876,var(6))),
	cf(1,cproj(5876,var(56))),
	cf(1,subtree(5857,'VPx[coh,fin]',5825,5890)),
	cf(1,phi(5857,var(6))),
	cf(1,cproj(5857,var(54))),
	cf(1,subtree(5825,'VPx[coh,fin]',-,2341)),
	cf(1,phi(5825,var(6))),
	cf(1,subtree(5816,'VPx[coh,fin]',-,5706)),
	cf(1,phi(5816,var(6))),
	cf(1,subtree(5706,'DP[std]',-,5704)),
	cf(1,phi(5706,var(11))),
	cf(1,cproj(5706,var(55))),
	cf(1,subtree(5704,'DPx[std]',-,5243)),
	cf(1,phi(5704,var(11))),
	cf(1,subtree(5435,'CPdep[std]',-,5434)),
	cf(1,phi(5435,var(6))),
	cf(1,cproj(5435,var(48))),
	cf(1,subtree(5434,'Cbar-comp[std]',2050,5430)),
	cf(1,phi(5434,var(6))),
	cf(1,cproj(5434,var(50))),
	cf(1,subtree(5430,'VP[coh,fin]',-,5429)),
	cf(1,phi(5430,var(6))),
	cf(1,cproj(5430,var(51))),
	cf(1,subtree(5429,'VPx[coh,fin]',5816,5857)),
	cf(1,phi(5429,var(6))),
	cf(1,cproj(5429,var(53))),
	cf(1,subtree(5363,'VPx[coh,fin]',-,5360)),
	cf(1,phi(5363,var(6))),
	cf(1,subtree(5360,'VC[coh,fin]',5348,5358)),
	cf(1,phi(5360,var(6))),
	cf(1,subtree(5358,'V[coh,fin]',-,5357)),
	cf(1,phi(5358,var(6))),
	cf(1,subtree(5357,'Vx[coh,fin]',5356,629)),
	cf(1,phi(5357,var(6))),
	cf(1,subtree(5356,'Vx[coh,fin]',5355,633)),
	cf(1,phi(5356,var(6))),
	cf(1,subtree(5355,'Vx[coh,fin]',5354,634)),
	cf(1,phi(5355,var(6))),
	cf(1,subtree(5354,'Vx[coh,fin]',5353,636)),
	cf(1,phi(5354,var(6))),
	cf(1,subtree(5353,'Vx[coh,fin]',5351,638)),
	cf(1,phi(5353,var(6))),
	cf(1,subtree(5351,'Vx[coh,fin]',-,607)),
	cf(1,phi(5351,var(6))),
	cf(1,subtree(5348,'VC[coh,fin]',-,2834)),
	cf(1,phi(5348,var(6))),
	cf(1,subtree(5274,'NAdj',5273,249)),
	cf(1,phi(5274,var(15))),
	cf(1,subtree(5273,'NAdj',5261,251)),
	cf(1,phi(5273,var(15))),
	cf(1,subtree(5267,'N[comm]',-,5266)),
	cf(1,phi(5267,var(11))),
	cf(1,subtree(5266,'NAdj',5274,247)),
	cf(1,phi(5266,var(15))),
	cf(1,subtree(5261,'NAdj',5259,253)),
	cf(1,phi(5261,var(15))),
	cf(1,subtree(5259,'NAdj',5258,219)),
	cf(1,phi(5259,var(15))),
	cf(1,subtree(5258,'NAdj',5257,221)),
	cf(1,phi(5258,var(15))),
	cf(1,subtree(5257,'NAdj',5255,225)),
	cf(1,phi(5257,var(15))),
	cf(1,subtree(5255,'NAdj',-,243)),
	cf(1,phi(5255,var(15))),
	cf(1,subtree(5243,'NP',-,5267)),
	cf(1,phi(5243,var(11))),
	cf(1,subtree(4536,'D[std]',4529,342)),
	cf(1,phi(4536,var(26))),
	cf(1,subtree(4535,'DPx[std]',-,4533)),
	cf(1,phi(4535,var(26))),
	cf(1,cproj(4535,var(64))),
	cf(1,subtree(4533,'D[std]',4532,339)),
	cf(1,phi(4533,var(26))),
	cf(1,cproj(4533,var(67))),
	cf(1,subtree(4532,'D[std]',4536,340)),
	cf(1,phi(4532,var(26))),
	cf(1,subtree(4529,'D[std]',4528,344)),
	cf(1,phi(4529,var(26))),
	cf(1,subtree(4528,'D[std]',4527,346)),
	cf(1,phi(4528,var(26))),
	cf(1,subtree(4527,'D[std]',4523,348)),
	cf(1,phi(4527,var(26))),
	cf(1,subtree(4523,'D[std]',-,350)),
	cf(1,phi(4523,var(26))),
	cf(1,subtree(4448,'A[+infl]',4447,421)),
	cf(1,phi(4448,var(28))),
	cf(1,subtree(4447,'A[+infl]',2590,423)),
	cf(1,phi(4447,var(28))),
	cf(1,subtree(4440,'DPx[std]',4535,4436)),
	cf(1,phi(4440,var(26))),
	cf(1,cproj(4440,var(64))),
	cf(1,subtree(4436,'NP',2603,2730)),
	cf(1,phi(4436,var(26))),
	cf(1,subtree(3971,'PP[std]',-,3968)),
	cf(1,phi(3971,var(25))),
	cf(1,cproj(3971,var(60))),
	cf(1,subtree(3968,'PPx[std]',2446,3964)),
	cf(1,phi(3968,var(25))),
	cf(1,cproj(3968,var(61))),
	cf(1,subtree(3964,'DP[std]',-,4440)),
	cf(1,phi(3964,var(26))),
	cf(1,cproj(3964,var(62))),
	cf(1,subtree(2834,'VC[v,inf]',2825,2832)),
	cf(1,phi(2834,var(20))),
	cf(1,subtree(2832,'Vaux[pass,inf]',2830,579)),
	cf(1,phi(2832,var(20))),
	cf(1,subtree(2830,'Vaux[pass,inf]',2829,580)),
	cf(1,phi(2830,var(20))),
	cf(1,subtree(2829,'Vaux[pass,inf]',-,584)),
	cf(1,phi(2829,var(20))),
	cf(1,subtree(2825,'VC[v,inf]',-,2821)),
	cf(1,phi(2825,var(20))),
	cf(1,subtree(2821,'VC[v,part]',-,2819)),
	cf(1,phi(2821,var(20))),
	cf(1,subtree(2819,'V[v,part]',-,2818)),
	cf(1,phi(2819,var(20))),
	cf(1,subtree(2818,'Vx[v,part]',2816,530)),
	cf(1,phi(2818,var(20))),
	cf(1,subtree(2816,'Vx[v,part]',2815,532)),
	cf(1,phi(2816,var(20))),
	cf(1,subtree(2815,'Vx[v,part]',-,511)),
	cf(1,phi(2815,var(20))),
	cf(1,subtree(2730,'N[comm]',2729,475)),
	cf(1,phi(2730,var(26))),
	cf(1,subtree(2729,'N[comm]',2728,477)),
	cf(1,phi(2729,var(26))),
	cf(1,subtree(2728,'N[comm]',2727,479)),
	cf(1,phi(2728,var(26))),
	cf(1,subtree(2727,'N[comm]',2726,481)),
	cf(1,phi(2727,var(26))),
	cf(1,subtree(2726,'N[comm]',-,483)),
	cf(1,phi(2726,var(26))),
	cf(1,subtree(2603,'NP',-,2600)),
	cf(1,phi(2603,var(26))),
	cf(1,subtree(2600,'AP[std,+infl]',-,2599)),
	cf(1,phi(2600,var(28))),
	cf(1,subtree(2599,'APx[std,+infl]',-,2596)),
	cf(1,phi(2599,var(28))),
	cf(1,subtree(2596,'A[+infl]',4448,419)),
	cf(1,phi(2596,var(28))),
	cf(1,subtree(2590,'A[+infl]',2589,425)),
	cf(1,phi(2590,var(28))),
	cf(1,subtree(2589,'A[+infl]',2588,427)),
	cf(1,phi(2589,var(28))),
	cf(1,subtree(2588,'A[+infl]',2586,429)),
	cf(1,phi(2588,var(28))),
	cf(1,subtree(2586,'A[+infl]',-,449)),
	cf(1,phi(2586,var(28))),
	cf(1,subtree(2446,'PPx[std]',-,2444)),
	cf(1,phi(2446,var(25))),
	cf(1,subtree(2444,'P[pre]',2443,312)),
	cf(1,phi(2444,var(25))),
	cf(1,subtree(2443,'P[pre]',2441,314)),
	cf(1,phi(2443,var(25))),
	cf(1,subtree(2441,'P[pre]',-,316)),
	cf(1,phi(2441,var(25))),
	cf(1,subtree(2341,'ADVP[std]',-,310)),
	cf(1,phi(2341,var(8))),
	cf(1,cproj(2341,var(57))),
	cf(1,subtree(2050,'Cbar-comp[std]',-,201)),
	cf(1,phi(2050,var(6))),
	cf(1,subtree(1853,'DP[std]',-,895)),
	cf(1,phi(1853,var(38))),
	cf(1,subtree(1121,'ADVP[std]',-,1120)),
	cf(1,phi(1121,var(2))),
	cf(1,cproj(1121,var(45))),
	cf(1,subtree(1120,'ADV[std]',1118,150)),
	cf(1,phi(1120,var(2))),
	cf(1,subtree(1118,'ADV[std]',-,166)),
	cf(1,phi(1118,var(2))),
	cf(1,subtree(895,'DPx[std]',-,894)),
	cf(1,phi(895,var(38))),
	cf(1,subtree(894,'PRON[std]',893,21)),
	cf(1,phi(894,var(38))),
	cf(1,subtree(893,'PRON[std]',892,24)),
	cf(1,phi(893,var(38))),
	cf(1,subtree(892,'PRON[std]',891,26)),
	cf(1,phi(892,var(38))),
	cf(1,subtree(891,'PRON[std]',890,28)),
	cf(1,phi(891,var(38))),
	cf(1,subtree(890,'PRON[std]',889,30)),
	cf(1,phi(890,var(38))),
	cf(1,subtree(889,'PRON[std]',888,32)),
	cf(1,phi(889,var(38))),
	cf(1,subtree(888,'PRON[std]',883,34)),
	cf(1,phi(888,var(38))),
	cf(1,subtree(883,'PRON[std]',-,40)),
	cf(1,phi(883,var(38))),
	cf(1,subtree(668,'PERIOD',-,665)),
	cf(1,phi(668,var(0))),
	cf(1,terminal(665,'.',[665])),
	cf(1,phi(665,var(0))),
	cf(1,subtree(648,'COMMA',-,641)),
	cf(1,phi(648,var(0))),
	cf(1,terminal(641,',',[641])),
	cf(1,phi(641,var(0))),
	cf(1,terminal(639,'+V',[601])),
	cf(1,phi(639,var(6))),
	cf(1,subtree(638,'V-T_BASE',-,639)),
	cf(1,phi(638,var(6))),
	cf(1,terminal(637,'.13',[601])),
	cf(1,phi(637,var(11))),
	cf(1,subtree(636,'PERS-F_BASE',-,637)),
	cf(1,phi(636,var(11))),
	cf(1,terminal(635,'.Pl',[601])),
	cf(1,phi(635,var(11))),
	cf(1,subtree(634,'NUM-F_BASE',-,635)),
	cf(1,phi(634,var(11))),
	cf(1,subtree(633,'TENSE-F_BASE',-,631)),
	cf(1,phi(633,var(6))),
	cf(1,terminal(631,'.Pres',[601])),
	cf(1,phi(631,var(6))),
	cf(1,subtree(629,'MOOD-F_BASE',-,628)),
	cf(1,phi(629,var(6))),
	cf(1,terminal(628,'.IS',[601])),
	cf(1,phi(628,var(6))),
	cf(1,terminal(608,'sollen',[601])),
	cf(1,phi(608,var(6))),
	cf(1,subtree(607,'Vmod-S_BASE',-,608)),
	cf(1,phi(607,var(6))),
	cf(1,subtree(584,'Vaux-S_BASE[pass]',-,583)),
	cf(1,phi(584,var(20))),
	cf(1,terminal(583,'werden',[576])),
	cf(1,phi(583,var(20))),
	cf(1,cproj(583,var(43))),
	cf(1,terminal(581,'+V',[576])),
	cf(1,phi(581,var(20))),
	cf(1,subtree(580,'V-T_BASE',-,581)),
	cf(1,phi(580,var(20))),
	cf(1,subtree(579,'INF-F_BASE',-,578)),
	cf(1,phi(579,var(20))),
	cf(1,terminal(578,'.Inf',[576])),
	cf(1,phi(578,var(20))),
	cf(1,terminal(533,'+V',[500])),
	cf(1,phi(533,var(20))),
	cf(1,subtree(532,'V-T_BASE',-,533)),
	cf(1,phi(532,var(20))),
	cf(1,terminal(531,'.PPast',[500])),
	cf(1,phi(531,var(20))),
	cf(1,subtree(530,'PPAST-F_BASE',-,531)),
	cf(1,phi(530,var(20))),
	cf(1,terminal(512,'ein#beziehen',[500])),
	cf(1,phi(512,var(20))),
	cf(1,subtree(511,'V-S_BASE',-,512)),
	cf(1,phi(511,var(20))),
	cf(1,terminal(484,'Regelung',[473])),
	cf(1,phi(484,var(26))),
	cf(1,subtree(483,'N-S_BASE',-,484)),
	cf(1,phi(483,var(26))),
	cf(1,terminal(482,'+NN',[473])),
	cf(1,phi(482,var(26))),
	cf(1,subtree(481,'N-T_BASE',-,482)),
	cf(1,phi(481,var(26))),
	cf(1,terminal(480,'.Fem',[473])),
	cf(1,phi(480,var(26))),
	cf(1,subtree(479,'GEND-F_BASE',-,480)),
	cf(1,phi(479,var(26))),
	cf(1,terminal(478,'.NGDA',[473])),
	cf(1,phi(478,var(26))),
	cf(1,subtree(477,'CASE-F_BASE',-,478)),
	cf(1,phi(477,var(26))),
	cf(1,terminal(476,'.Sg',[473])),
	cf(1,phi(476,var(26))),
	cf(1,subtree(475,'NUM-F_BASE',-,476)),
	cf(1,phi(475,var(26))),
	cf(1,terminal(450,'.Wk',[415])),
	cf(1,phi(450,var(26))),
	cf(1,subtree(449,'A-CANON-S_BASE',-,432)),
	cf(1,phi(449,var(28))),
	cf(1,terminal(432,'gemeinsam',[415])),
	cf(1,phi(432,var(28))),
	cf(1,terminal(430,'+ADJ',[415])),
	cf(1,phi(430,var(28))),
	cf(1,subtree(429,'A-T_BASE',-,430)),
	cf(1,phi(429,var(28))),
	cf(1,terminal(428,'.Pos',[415])),
	cf(1,phi(428,var(28))),
	cf(1,subtree(427,'DEG-F_BASE',-,428)),
	cf(1,phi(427,var(28))),
	cf(1,terminal(426,'.Fem',[415])),
	cf(1,phi(426,var(26))),
	cf(1,subtree(425,'GEND-F_BASE',-,426)),
	cf(1,phi(425,var(26))),
	cf(1,terminal(424,'.NA',[415])),
	cf(1,phi(424,var(26))),
	cf(1,subtree(423,'CASE-F_BASE',-,424)),
	cf(1,phi(423,var(26))),
	cf(1,terminal(422,'.Sg',[415])),
	cf(1,phi(422,var(26))),
	cf(1,subtree(421,'NUM-F_BASE',-,422)),
	cf(1,phi(421,var(26))),
	cf(1,subtree(419,'INFL-F_BASE[adj]',-,450)),
	cf(1,phi(419,var(26))),
	cf(1,terminal(351,'die_art',[335])),
	cf(1,phi(351,var(26))),
	cf(1,cproj(351,var(41))),
	cf(1,subtree(350,'ART-S_BASE',-,351)),
	cf(1,phi(350,var(26))),
	cf(1,terminal(349,'+ART',[335])),
	cf(1,phi(349,var(26))),
	cf(1,subtree(348,'ART-T_BASE',-,349)),
	cf(1,phi(348,var(26))),
	cf(1,terminal(347,'.Def',[335])),
	cf(1,phi(347,var(26))),
	cf(1,subtree(346,'DISTR-ATTR_BASE',-,347)),
	cf(1,phi(346,var(26))),
	cf(1,terminal(345,'.Fem',[335])),
	cf(1,phi(345,var(26))),
	cf(1,subtree(344,'GEND-F_BASE',-,345)),
	cf(1,phi(344,var(26))),
	cf(1,terminal(343,'.NA',[335])),
	cf(1,phi(343,var(26))),
	cf(1,subtree(342,'CASE-F_BASE',-,343)),
	cf(1,phi(342,var(26))),
	cf(1,terminal(341,'.Sg',[335])),
	cf(1,phi(341,var(26))),
	cf(1,subtree(340,'NUM-F_BASE',-,341)),
	cf(1,phi(340,var(26))),
	cf(1,subtree(339,'INFL-F_BASE[det]',-,337)),
	cf(1,phi(339,var(26))),
	cf(1,terminal(337,'.St',[335])),
	cf(1,phi(337,var(26))),
	cf(1,terminal(317,'in',[311])),
	cf(1,phi(317,var(25))),
	cf(1,subtree(316,'ADPOS-S_BASE',-,317)),
	cf(1,phi(316,var(25))),
	cf(1,terminal(315,'+PREP',[311])),
	cf(1,phi(315,var(25))),
	cf(1,subtree(314,'ADPOS-T_BASE[pre]',-,315)),
	cf(1,phi(314,var(25))),
	cf(1,terminal(313,'.DA',[311])),
	cf(1,phi(313,var(26))),
	cf(1,subtree(312,'CASE-F_BASE',-,313)),
	cf(1,phi(312,var(26))),
	cf(1,subtree(310,'ADV[std]',-,284)),
	cf(1,phi(310,var(8))),
	cf(1,terminal(284,'nicht',[284])),
	cf(1,phi(284,var(8))),
	cf(1,terminal(254,'.MFN',[207])),
	cf(1,phi(254,var(11))),
	cf(1,subtree(253,'GEND-F_BASE',-,254)),
	cf(1,phi(253,var(11))),
	cf(1,terminal(252,'.NA',[207])),
	cf(1,phi(252,var(11))),
	cf(1,subtree(251,'CASE-F_BASE',-,252)),
	cf(1,phi(251,var(11))),
	cf(1,terminal(250,'.Pl',[207])),
	cf(1,phi(250,var(11))),
	cf(1,subtree(249,'NUM-F_BASE',-,250)),
	cf(1,phi(249,var(11))),
	cf(1,subtree(247,'INFL-F_BASE[adj]',-,246)),
	cf(1,phi(247,var(11))),
	cf(1,terminal(246,'.St',[207])),
	cf(1,phi(246,var(11))),
	cf(1,subtree(243,'A-CANON-S_BASE',-,227)),
	cf(1,phi(243,var(15))),
	cf(1,terminal(227,'minderjährig',[207])),
	cf(1,phi(227,var(15))),
	cf(1,subtree(225,'ADJ-DERIV-F_BASE',-,224)),
	cf(1,phi(225,var(15))),
	cf(1,terminal(224,'^ADJ',[207])),
	cf(1,phi(224,var(15))),
	cf(1,terminal(222,'.Pos',[207])),
	cf(1,phi(222,var(15))),
	cf(1,subtree(221,'DEG-F_BASE',-,222)),
	cf(1,phi(221,var(15))),
	cf(1,terminal(220,'+NN',[207])),
	cf(1,phi(220,var(15))),
	cf(1,subtree(219,'N-T_BASE',-,220)),
	cf(1,phi(219,var(15))),
	cf(1,subtree(201,'C[std]',-,198)),
	cf(1,phi(201,var(6))),
	cf(1,terminal(198,'dass',[198])),
	cf(1,phi(198,var(6))),
	cf(1,subtree(182,'COMMA',-,172)),
	cf(1,phi(182,var(0))),
	cf(1,terminal(172,',',[172])),
	cf(1,phi(172,var(0))),
	cf(1,subtree(166,'ADV-S_BASE',-,153)),
	cf(1,phi(166,var(2))),
	cf(1,terminal(153,'ebenfalls',[149])),
	cf(1,phi(153,var(2))),
	cf(1,terminal(151,'+ADV',[149])),
	cf(1,phi(151,var(2))),
	cf(1,subtree(150,'ADV-T_BASE[std]',-,151)),
	cf(1,phi(150,var(2))),
	cf(1,terminal(148,'+V',[55])),
	cf(1,phi(148,var(0))),
	cf(1,subtree(147,'V-T_BASE',-,148)),
	cf(1,phi(147,var(0))),
	cf(1,terminal(146,'.13',[55])),
	cf(1,phi(146,var(38))),
	cf(1,subtree(145,'PERS-F_BASE',-,146)),
	cf(1,phi(145,var(38))),
	cf(1,terminal(144,'.Pl',[55])),
	cf(1,phi(144,var(38))),
	cf(1,subtree(143,'NUM-F_BASE',-,144)),
	cf(1,phi(143,var(38))),
	cf(1,subtree(142,'TENSE-F_BASE',-,140)),
	cf(1,phi(142,var(0))),
	cf(1,terminal(140,'.Pres',[55])),
	cf(1,phi(140,var(0))),
	cf(1,subtree(138,'MOOD-F_BASE',-,137)),
	cf(1,phi(138,var(0))),
	cf(1,terminal(137,'.IS',[55])),
	cf(1,phi(137,var(0))),
	cf(1,terminal(118,'meinen',[55])),
	cf(1,phi(118,var(0))),
	cf(1,subtree(117,'V-S_BASE',-,118)),
	cf(1,phi(117,var(0))),
	cf(1,subtree(40,'PRON-S_BASE',-,37)),
	cf(1,phi(40,var(38))),
	cf(1,terminal(37,'sie',[20])),
	cf(1,phi(37,var(38))),
	cf(1,terminal(35,'+PPRO',[20])),
	cf(1,phi(35,var(38))),
	cf(1,subtree(34,'PRON-PERS-T_BASE',-,35)),
	cf(1,phi(34,var(38))),
	cf(1,terminal(33,'.Pers',[20])),
	cf(1,phi(33,var(38))),
	cf(1,subtree(32,'PRON-PERS-F_BASE',-,33)),
	cf(1,phi(32,var(38))),
	cf(1,terminal(31,'.1',[20])),
	cf(1,phi(31,var(38))),
	cf(1,subtree(30,'PERS-F_BASE',-,31)),
	cf(1,phi(30,var(38))),
	cf(1,terminal(29,'.Pl',[20])),
	cf(1,phi(29,var(38))),
	cf(1,subtree(28,'NUM-F_BASE',-,29)),
	cf(1,phi(28,var(38))),
	cf(1,terminal(27,'.MFN',[20])),
	cf(1,phi(27,var(38))),
	cf(1,subtree(26,'GEND-F_BASE',-,27)),
	cf(1,phi(26,var(38))),
	cf(1,terminal(25,'.Nom',[20])),
	cf(1,phi(25,var(38))),
	cf(1,subtree(24,'CASE-F_BASE',-,25)),
	cf(1,phi(24,var(38))),
	cf(1,terminal(22,'.None',[20])),
	cf(1,phi(22,var(38))),
	cf(1,subtree(21,'INFL-F_BASE',-,22)),
	cf(1,phi(21,var(38))),
	cf(1,semform_data(1,40,1,1)),
	cf(1,semform_data(6,117,5,9)),
	cf(1,semform_data(8,166,12,20)),
	cf(1,semform_data(11,243,28,40)),
	cf(1,semform_data(13,5267,28,42)),
	cf(1,semform_data(20,310,43,48)),
	cf(1,semform_data(23,316,49,50)),
	cf(1,semform_data(26,350,52,53)),
	cf(1,semform_data(31,449,56,64)),
	cf(1,semform_data(35,483,67,74)),
	cf(1,semform_data(40,511,76,84)),
	cf(1,semform_data(46,607,94,99)),
	cf(1,surfaceform(665,'.',100,100)),
	cf(1,surfaceform(641,',',100,100)),
	cf(1,surfaceform(601,'sollen',94,100)),
	cf(1,surfaceform(576,'werden',87,93)),
	cf(1,surfaceform(500,'einbezogen',76,86)),
	cf(1,surfaceform(473,'Regelung',67,75)),
	cf(1,surfaceform(415,'gemeinsame',56,66)),
	cf(1,surfaceform(335,'die',52,55)),
	cf(1,surfaceform(311,'in',49,51)),
	cf(1,surfaceform(284,'nicht',43,48)),
	cf(1,surfaceform(207,'Minderjährige',28,42)),
	cf(1,surfaceform(198,'dass',23,27)),
	cf(1,surfaceform(172,',',21,22)),
	cf(1,surfaceform(149,'ebenfalls',12,21)),
	cf(1,surfaceform(55,'meinen',5,11)),
	cf(1,surfaceform(20,'wir',1,4))
	]).
