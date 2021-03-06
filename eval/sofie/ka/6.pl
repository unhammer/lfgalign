% -*- coding: iso-8859-1 -*-

fstructure('soPim ar iCoda, daTanHmeboda Tu ara.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('6'),
	'statistics'('1+1 solutions, 0.07 CPU seconds, 74 subtrees unified'),
	'rootcategory'('ROOT'),
	'max_medial_constituent_weight'('35'),
	'max_medial2_constituent_weight'('30'),
	'max_raw_subtrees'('50000'),
	'hostname'('maximos')
	],
	% Choices:
	[
	
	],
	% Equivalences:
	[
	
	],
	% Constraints:
	[
	cf(1,eq(attr(var(0),'PRED'),semform('Codna',3,[var(6),var(12)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(6))),
	cf(1,eq(attr(var(0),'OBJ'),var(12))),
	cf(1,eq(attr(var(0),'ADJUNCT'),var(1))),
	cf(1,eq(attr(var(0),'CHECK'),var(4))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(24))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'NEG'),'+')),
	cf(1,eq(attr(var(0),'POLARITY'),'neg')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(6),'PRED'),semform('soPi',0,[],[]))),
	cf(1,eq(attr(var(6),'CHECK'),var(7))),
	cf(1,eq(attr(var(6),'NTYPE'),var(8))),
	cf(1,eq(attr(var(6),'ANIM'),'+')),
	cf(1,eq(attr(var(6),'CASE'),'erg')),
	cf(1,eq(attr(var(6),'GEND'),'fem')),
	cf(1,eq(attr(var(6),'NUM'),'sg')),
	cf(1,eq(attr(var(6),'PERS'),'3')),
	cf(1,eq(attr(var(7),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(7),'_POLARITY'),'neg')),
	cf(1,eq(attr(var(8),'NSYN'),'proper')),
	cf(1,eq(attr(var(12),'PRED'),semform('da-TanHmeba',6,[var(21),var(19)],[]))),
	cf(1,eq(attr(var(12),'SUBJ'),var(21))),
	cf(1,eq(attr(var(12),'OBJth'),var(19))),
	cf(1,eq(attr(var(12),'ADJUNCT'),var(13))),
	cf(1,eq(attr(var(12),'CHECK'),var(16))),
	cf(1,eq(attr(var(12),'TNS-ASP'),var(23))),
	cf(1,eq(attr(var(12),'CLAUSE-TYPE'),'sub')),
	cf(1,eq(attr(var(12),'COMP-FORM'),'Tu')),
	cf(1,eq(attr(var(12),'NUM'),'sg')),
	cf(1,eq(attr(var(12),'PERS'),'3')),
	cf(1,eq(attr(var(12),'VFORM'),'fin')),
	cf(1,eq(attr(var(21),'PRED'),semform('pro',7,[],[]))),
	cf(1,eq(attr(var(21),'CHECK'),var(22))),
	cf(1,eq(attr(var(21),'CASE'),'nom')),
	cf(1,eq(attr(var(21),'PERS'),'3')),
	cf(1,eq(attr(var(22),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(22),'_PRODROP'),'+')),
	cf(1,eq(attr(var(19),'PRED'),semform('pro',8,[],[]))),
	cf(1,eq(attr(var(19),'CHECK'),var(20))),
	cf(1,eq(attr(var(19),'CASE'),'dat')),
	cf(1,eq(attr(var(19),'PERS'),'3')),
	cf(1,eq(attr(var(20),'_PRODROP'),'+')),
	cf(1,in_set(var(14),var(13))),
	cf(1,eq(attr(var(14),'PRED'),semform('ara',9,[],[]))),
	cf(1,eq(attr(var(14),'CHECK'),var(15))),
	cf(1,eq(attr(var(14),'ADV-TYPE'),'neg')),
	cf(1,eq(attr(var(15),'_ARTIC'),'+')),
	cf(1,eq(attr(var(15),'_PURE-NEG'),'+')),
	cf(1,eq(attr(var(16),'_MORPH-SYNT'),var(17))),
	cf(1,eq(attr(var(16),'_AGR'),'both')),
	cf(1,eq(attr(var(16),'_MAIN-CL'),'-')),
	cf(1,eq(attr(var(16),'_TENSE'),'plu-perf')),
	cf(1,eq(attr(var(16),'_TENSEGROUP'),'perf')),
	cf(1,eq(attr(var(16),'_TOP'),'+')),
	cf(1,eq(attr(var(17),'_AGR'),var(18))),
	cf(1,eq(attr(var(17),'_CLASS'),'RP1')),
	cf(1,eq(attr(var(17),'_LEXID'),'V723-10')),
	cf(1,eq(attr(var(17),'_PERF-PV'),'da')),
	cf(1,eq(attr(var(17),'_SYNTAX'),'unacc')),
	cf(1,eq(attr(var(18),'_OBJ'),var(19))),
	cf(1,eq(attr(var(23),'ASPECT'),'perf')),
	cf(1,eq(attr(var(23),'MOOD'),'indicative')),
	cf(1,eq(attr(var(23),'TENSE'),'past')),
	cf(1,in_set(var(2),var(1))),
	cf(1,eq(attr(var(2),'PRED'),semform('ar',2,[],[]))),
	cf(1,eq(attr(var(2),'CHECK'),var(3))),
	cf(1,eq(attr(var(2),'ADV-TYPE'),'neg')),
	cf(1,eq(attr(var(3),'_PURE-NEG'),'+')),
	cf(1,eq(attr(var(4),'_IN-SITU'),var(5))),
	cf(1,eq(attr(var(4),'_MORPH-SYNT'),var(9))),
	cf(1,eq(attr(var(4),'_AGR'),'both')),
	cf(1,eq(attr(var(4),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(4),'_PERIOD'),'+')),
	cf(1,eq(attr(var(4),'_TENSE'),'impf')),
	cf(1,eq(attr(var(4),'_TENSEGROUP'),'pres')),
	cf(1,in_set(var(6),var(5))),
	cf(1,eq(attr(var(9),'_AGR'),var(10))),
	cf(1,eq(attr(var(9),'_CLASS'),'IV1')),
	cf(1,eq(attr(var(9),'_LEXID'),'V3117-1')),
	cf(1,eq(attr(var(9),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(9),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(10),'_OBJ'),var(11))),
	cf(1,eq(attr(var(11),'PERS'),'3')),
	cf(1,eq(attr(var(24),'ASPECT'),'imperf')),
	cf(1,eq(attr(var(24),'MOOD'),'indicative')),
	cf(1,eq(attr(var(24),'TENSE'),'past')),
	cf(1,eq(proj(var(45),'o::'),var(46))),
	cf(1,in_set('PV',var(46)))
	],
	% C-Structure:
	[
	cf(1,subtree(1361,'ROOT',1358,74)),
	cf(1,phi(1361,var(0))),
	cf(1,subtree(1358,'ROOT',-,1343)),
	cf(1,phi(1358,var(0))),
	cf(1,subtree(1343,'IPfoc[main,-]',187,1266)),
	cf(1,phi(1343,var(0))),
	cf(1,subtree(1266,'Ibar[main,-]',1169,1242)),
	cf(1,phi(1266,var(0))),
	cf(1,subtree(1242,'S',488,1084)),
	cf(1,phi(1242,var(0))),
	cf(1,subtree(1169,'Ibar[main,-]',-,1159)),
	cf(1,phi(1169,var(0))),
	cf(1,subtree(1159,'I[main,-]',-,458)),
	cf(1,phi(1159,var(0))),
	cf(1,subtree(1084,'CPsub',761,67)),
	cf(1,phi(1084,var(12))),
	cf(1,subtree(761,'CPsub',759,59)),
	cf(1,phi(761,var(12))),
	cf(1,subtree(759,'CPsub',-,758)),
	cf(1,phi(759,var(12))),
	cf(1,subtree(758,'IP[sub,-]',-,756)),
	cf(1,phi(758,var(12))),
	cf(1,subtree(756,'Ibar[sub,-]',-,744)),
	cf(1,phi(756,var(12))),
	cf(1,subtree(744,'I[sub,-]',-,739)),
	cf(1,phi(744,var(12))),
	cf(1,subtree(739,'V',738,35)),
	cf(1,phi(739,var(12))),
	cf(1,subtree(738,'V',737,37)),
	cf(1,phi(738,var(12))),
	cf(1,subtree(737,'V',736,39)),
	cf(1,phi(737,var(12))),
	cf(1,subtree(736,'V',735,41)),
	cf(1,phi(736,var(12))),
	cf(1,subtree(735,'V',734,43)),
	cf(1,phi(735,var(12))),
	cf(1,subtree(734,'V',732,45)),
	cf(1,phi(734,var(12))),
	cf(1,subtree(732,'V',-,49)),
	cf(1,phi(732,var(12))),
	cf(1,subtree(488,'S',-,33)),
	cf(1,phi(488,var(0))),
	cf(1,subtree(458,'Vneg',271,456)),
	cf(1,phi(458,var(0))),
	cf(1,subtree(456,'V',455,11)),
	cf(1,phi(456,var(0))),
	cf(1,subtree(455,'V',454,13)),
	cf(1,phi(455,var(0))),
	cf(1,subtree(454,'V',453,15)),
	cf(1,phi(454,var(0))),
	cf(1,subtree(453,'V',452,17)),
	cf(1,phi(453,var(0))),
	cf(1,subtree(452,'V',451,19)),
	cf(1,phi(452,var(0))),
	cf(1,subtree(451,'V',449,21)),
	cf(1,phi(451,var(0))),
	cf(1,subtree(449,'V',-,25)),
	cf(1,phi(449,var(0))),
	cf(1,subtree(271,'Vneg',-,8)),
	cf(1,phi(271,var(0))),
	cf(1,subtree(187,'IPfoc[main,-]',-,164)),
	cf(1,phi(187,var(0))),
	cf(1,subtree(164,'PROPP',-,2)),
	cf(1,phi(164,var(6))),
	cf(1,subtree(74,'PERIOD',-,68)),
	cf(1,phi(74,var(0))),
	cf(1,terminal(68,'.',[68])),
	cf(1,phi(68,var(0))),
	cf(1,subtree(67,'ADVneg',-,60)),
	cf(1,phi(67,var(14))),
	cf(1,terminal(60,'ara',[60])),
	cf(1,phi(60,var(14))),
	cf(1,subtree(59,'Csub',-,53)),
	cf(1,phi(59,var(12))),
	cf(1,terminal(53,'Tu',[53])),
	cf(1,phi(53,var(12))),
	cf(1,subtree(49,'V_BASE',-,48)),
	cf(1,phi(49,var(12))),
	cf(1,terminal(48,'da-TanHmeba-723-10',[34])),
	cf(1,phi(48,var(47))),
	cf(1,cproj(48,var(45))),
	cf(1,terminal(46,'+V',[34])),
	cf(1,phi(46,var(12))),
	cf(1,subtree(45,'V_SUFF_BASE',-,46)),
	cf(1,phi(45,var(12))),
	cf(1,terminal(44,'+Unacc',[34])),
	cf(1,phi(44,var(12))),
	cf(1,subtree(43,'V_SUFF_BASE',-,44)),
	cf(1,phi(43,var(12))),
	cf(1,terminal(42,'+Base',[34])),
	cf(1,phi(42,var(12))),
	cf(1,subtree(41,'V_SUFF_BASE',-,42)),
	cf(1,phi(41,var(12))),
	cf(1,terminal(40,'+PluPerf',[34])),
	cf(1,phi(40,var(12))),
	cf(1,subtree(39,'V_SUFF_BASE',-,40)),
	cf(1,phi(39,var(12))),
	cf(1,terminal(38,'+Subj3Sg',[34])),
	cf(1,phi(38,var(12))),
	cf(1,subtree(37,'V_SUFF_BASE',-,38)),
	cf(1,phi(37,var(12))),
	cf(1,terminal(36,'+Obj3',[34])),
	cf(1,phi(36,var(12))),
	cf(1,subtree(35,'V_SUFF_BASE',-,36)),
	cf(1,phi(35,var(12))),
	cf(1,subtree(33,'COMMA',-,29)),
	cf(1,phi(33,var(0))),
	cf(1,terminal(29,',',[29])),
	cf(1,phi(29,var(0))),
	cf(1,subtree(25,'V_BASE',-,24)),
	cf(1,phi(25,var(0))),
	cf(1,terminal(24,'Codna-3117-1',[10])),
	cf(1,phi(24,var(48))),
	cf(1,terminal(22,'+V',[10])),
	cf(1,phi(22,var(0))),
	cf(1,subtree(21,'V_SUFF_BASE',-,22)),
	cf(1,phi(21,var(0))),
	cf(1,terminal(20,'+Unerg',[10])),
	cf(1,phi(20,var(0))),
	cf(1,subtree(19,'V_SUFF_BASE',-,20)),
	cf(1,phi(19,var(0))),
	cf(1,terminal(18,'+Base',[10])),
	cf(1,phi(18,var(0))),
	cf(1,subtree(17,'V_SUFF_BASE',-,18)),
	cf(1,phi(17,var(0))),
	cf(1,terminal(16,'+Impf',[10])),
	cf(1,phi(16,var(0))),
	cf(1,subtree(15,'V_SUFF_BASE',-,16)),
	cf(1,phi(15,var(0))),
	cf(1,terminal(14,'+Subj3Sg',[10])),
	cf(1,phi(14,var(0))),
	cf(1,subtree(13,'V_SUFF_BASE',-,14)),
	cf(1,phi(13,var(0))),
	cf(1,terminal(12,'+Obj3',[10])),
	cf(1,phi(12,var(0))),
	cf(1,subtree(11,'V_SUFF_BASE',-,12)),
	cf(1,phi(11,var(0))),
	cf(1,subtree(8,'ADVneg',-,3)),
	cf(1,phi(8,var(2))),
	cf(1,terminal(3,'ar',[3])),
	cf(1,phi(3,var(2))),
	cf(1,subtree(2,'PROP',-,1)),
	cf(1,phi(2,var(6))),
	cf(1,terminal(1,'soPim',[1])),
	cf(1,phi(1,var(6))),
	cf(1,semform_data(0,2,1,6)),
	cf(1,semform_data(2,8,7,9)),
	cf(1,semform_data(3,25,10,14)),
	cf(1,semform_data(6,49,17,28)),
	cf(1,semform_data(7,49,17,28)),
	cf(1,semform_data(8,49,17,28)),
	cf(1,semform_data(9,67,33,36)),
	cf(1,fspan(var(0),1,37)),
	cf(1,fspan(var(6),1,6)),
	cf(1,fspan(var(2),7,9)),
	cf(1,fspan(var(12),17,36)),
	cf(1,fspan(var(14),33,36)),
	cf(1,surfaceform(1,'soPim',1,6)),
	cf(1,surfaceform(3,'ar',7,9)),
	cf(1,surfaceform(10,'iCoda',10,15)),
	cf(1,surfaceform(29,',',16,16)),
	cf(1,surfaceform(34,'daTanHmeboda',17,29)),
	cf(1,surfaceform(53,'Tu',30,32)),
	cf(1,surfaceform(60,'ara',33,36)),
	cf(1,surfaceform(68,'.',36,37))
	]).

