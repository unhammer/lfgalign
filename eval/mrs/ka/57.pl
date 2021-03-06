% -*- coding: iso-8859-1 -*-

fstructure('samma jaGlTaganma iqePa.',
	% Properties:
	[
	'xle_version'('XLE release of Jan 21, 2009 13:04.'),
	'grammar'('/usr/local/xledir/pargram/kartuli/kartuli.lfg'),
	'grammar_date'('Mar 24, 2009 10:39'),
	'word_count'('3'),
	'statistics'('1+1 solutions, 0.02 CPU seconds, 136 subtrees unified'),
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
	cf(1,eq(attr(var(0),'PRED'),semform('qePa',12,[var(3)],[]))),
	cf(1,eq(attr(var(0),'SUBJ'),var(3))),
	cf(1,eq(attr(var(0),'CHECK'),var(1))),
	cf(1,eq(attr(var(0),'POLARITY'),var(5))),
	cf(1,eq(attr(var(0),'TNS-ASP'),var(19))),
	cf(1,eq(attr(var(0),'CLAUSE-TYPE'),'decl')),
	cf(1,eq(attr(var(0),'VFORM'),'fin')),
	cf(1,eq(attr(var(3),'PRED'),semform('pro',7,[],[]))),
	cf(1,eq(attr(var(3),'CHECK'),var(4))),
	cf(1,eq(attr(var(3),'PART'),var(6))),
	cf(1,eq(attr(var(3),'SPEC'),var(13))),
	cf(1,eq(attr(var(3),'CASE'),'erg')),
	cf(1,eq(attr(var(3),'NUM'),'sg')),
	cf(1,eq(attr(var(3),'PERS'),'3')),
	cf(1,eq(attr(var(3),'SEMNUM'),'pl')),
	cf(1,eq(attr(var(4),'_POLARITY'),var(5))),
	cf(1,eq(attr(var(4),'_AGR-POS'),'left')),
	cf(1,eq(attr(var(6),'PRED'),semform('-gan',6,[var(9)],[]))),
	cf(1,eq(attr(var(6),'OBJ'),var(9))),
	cf(1,eq(attr(var(6),'CHECK'),var(7))),
	cf(1,eq(attr(var(6),'PSEM'),'part')),
	cf(1,eq(attr(var(6),'PTYPE'),'sem')),
	cf(1,eq(attr(var(9),'PRED'),semform('jaGli',5,[],[]))),
	cf(1,eq(attr(var(9),'CHECK'),var(10))),
	cf(1,eq(attr(var(9),'NTYPE'),var(12))),
	cf(1,eq(attr(var(9),'CASE'),'gen')),
	cf(1,eq(attr(var(9),'NUM'),'pl')),
	cf(1,eq(attr(var(9),'PERS'),'3')),
	cf(1,eq(attr(var(10),'_DOUBLE'),var(8))),
	cf(1,eq(attr(var(10),'_MORPH-SYNT'),var(11))),
	cf(1,eq(attr(var(10),'_CASE-TYPE'),'full')),
	cf(1,eq(attr(var(10),'_POSTP'),'-gan')),
	cf(1,eq(attr(var(8),'CASE'),'erg')),
	cf(1,eq(attr(var(8),'NUM'),'sg')),
	cf(1,eq(attr(var(8),'_CASE'),'+')),
	cf(1,eq(attr(var(11),'OLD-PL'),'+')),
	cf(1,eq(attr(var(12),'NSYN'),'common')),
	cf(1,eq(attr(var(7),'_DOUBLE'),var(8))),
	cf(1,eq(attr(var(7),'_POSTP'),'-gan')),
	cf(1,eq(attr(var(13),'NUMBER'),var(14))),
	cf(1,eq(attr(var(14),'PRED'),semform('3',0,[],[]))),
	cf(1,eq(attr(var(14),'CHECK'),var(15))),
	cf(1,eq(attr(var(14),'CASE'),'erg')),
	cf(1,eq(attr(var(14),'NUMBER-TYPE'),'card')),
	cf(1,eq(attr(var(15),'_CASE-TYPE'),'reduced')),
	cf(1,eq(attr(var(15),'_DIGVAL'),'3')),
	cf(1,eq(attr(var(1),'_IN-SITU'),var(2))),
	cf(1,eq(attr(var(1),'_MORPH-SYNT'),var(16))),
	cf(1,eq(attr(var(1),'_AGR'),'both')),
	cf(1,eq(attr(var(1),'_MAIN-CL'),'+')),
	cf(1,eq(attr(var(1),'_PERIOD'),'+')),
	cf(1,eq(attr(var(1),'_TENSE'),'aor')),
	cf(1,eq(attr(var(1),'_TENSEGROUP'),'aor')),
	cf(1,in_set(var(3),var(2))),
	cf(1,eq(attr(var(16),'_AGR'),var(17))),
	cf(1,eq(attr(var(16),'_CLASS'),'MV')),
	cf(1,eq(attr(var(16),'_LEXID'),'V2746-3')),
	cf(1,eq(attr(var(16),'_PERF-PV'),'-')),
	cf(1,eq(attr(var(16),'_SYNTAX'),'unerg')),
	cf(1,eq(attr(var(17),'_OBJ'),var(18))),
	cf(1,eq(attr(var(18),'NUM'),'sg')),
	cf(1,eq(attr(var(18),'PERS'),'3')),
	cf(1,eq(attr(var(19),'ASPECT'),'perf')),
	cf(1,eq(attr(var(19),'MOOD'),'indicative')),
	cf(1,eq(attr(var(19),'TENSE'),'past')),
	cf(1,eq(proj(var(30),'o::'),var(31))),
	cf(1,in_set('NO-PV',var(31)))
	],
	% C-Structure:
	[
	cf(1,subtree(904,'ROOT',901,86)),
	cf(1,phi(904,var(0))),
	cf(1,subtree(916,'POSSP',-,651)),
	cf(1,phi(916,var(3))),
	cf(1,subtree(901,'ROOT',-,878)),
	cf(1,phi(901,var(0))),
	cf(1,subtree(878,'IPfoc[main,-]',858,791)),
	cf(1,phi(878,var(0))),
	cf(1,subtree(858,'IPfoc[main,-]',-,644)),
	cf(1,phi(858,var(0))),
	cf(1,subtree(791,'Ibar[main,-]',-,688)),
	cf(1,phi(791,var(0))),
	cf(1,subtree(688,'I[main,-]',-,685)),
	cf(1,phi(688,var(0))),
	cf(1,subtree(685,'V',684,62)),
	cf(1,phi(685,var(0))),
	cf(1,subtree(684,'V',683,64)),
	cf(1,phi(684,var(0))),
	cf(1,subtree(683,'V',682,66)),
	cf(1,phi(683,var(0))),
	cf(1,subtree(682,'V',681,68)),
	cf(1,phi(682,var(0))),
	cf(1,subtree(681,'V',680,70)),
	cf(1,phi(681,var(0))),
	cf(1,subtree(680,'V',678,72)),
	cf(1,phi(680,var(0))),
	cf(1,subtree(678,'V',-,76)),
	cf(1,phi(678,var(0))),
	cf(1,subtree(651,'PPpart',-,522)),
	cf(1,phi(651,var(6))),
	cf(1,subtree(644,'QuantP',490,916)),
	cf(1,phi(644,var(3))),
	cf(1,subtree(522,'NP',-,517)),
	cf(1,phi(522,var(9))),
	cf(1,subtree(517,'N',516,34)),
	cf(1,phi(517,var(9))),
	cf(1,subtree(516,'N',515,38)),
	cf(1,phi(516,var(9))),
	cf(1,subtree(515,'N',514,41)),
	cf(1,phi(515,var(9))),
	cf(1,subtree(514,'N',513,45)),
	cf(1,phi(514,var(9))),
	cf(1,subtree(513,'N',512,49)),
	cf(1,phi(513,var(9))),
	cf(1,subtree(512,'N',511,50)),
	cf(1,phi(512,var(9))),
	cf(1,subtree(511,'N',510,52)),
	cf(1,phi(511,var(9))),
	cf(1,subtree(510,'N',508,54)),
	cf(1,phi(510,var(9))),
	cf(1,subtree(508,'N',-,56)),
	cf(1,phi(508,var(9))),
	cf(1,subtree(490,'QuantP',-,488)),
	cf(1,phi(490,var(3))),
	cf(1,subtree(488,'APcard',-,485)),
	cf(1,phi(488,var(14))),
	cf(1,subtree(485,'Acard',462,30)),
	cf(1,phi(485,var(14))),
	cf(1,subtree(462,'Acard',460,14)),
	cf(1,phi(462,var(14))),
	cf(1,subtree(460,'Acard',459,15)),
	cf(1,phi(460,var(14))),
	cf(1,subtree(459,'Acard',458,19)),
	cf(1,phi(459,var(14))),
	cf(1,subtree(458,'Acard',457,22)),
	cf(1,phi(458,var(14))),
	cf(1,subtree(457,'Acard',-,26)),
	cf(1,phi(457,var(14))),
	cf(1,subtree(86,'PERIOD',-,80)),
	cf(1,phi(86,var(0))),
	cf(1,terminal(80,'.',[80])),
	cf(1,phi(80,var(0))),
	cf(1,subtree(76,'V_BASE',-,75)),
	cf(1,phi(76,var(0))),
	cf(1,terminal(75,'qePa-2746-3',[61])),
	cf(1,phi(75,var(32))),
	cf(1,cproj(75,var(30))),
	cf(1,terminal(73,'+V',[61])),
	cf(1,phi(73,var(0))),
	cf(1,subtree(72,'V_SUFF_BASE',-,73)),
	cf(1,phi(72,var(0))),
	cf(1,terminal(71,'+Unerg',[61])),
	cf(1,phi(71,var(0))),
	cf(1,subtree(70,'V_SUFF_BASE',-,71)),
	cf(1,phi(70,var(0))),
	cf(1,terminal(69,'+Base',[61])),
	cf(1,phi(69,var(0))),
	cf(1,subtree(68,'V_SUFF_BASE',-,69)),
	cf(1,phi(68,var(0))),
	cf(1,terminal(67,'+Aor',[61])),
	cf(1,phi(67,var(0))),
	cf(1,subtree(66,'V_SUFF_BASE',-,67)),
	cf(1,phi(66,var(0))),
	cf(1,terminal(65,'+Subj3Sg',[61])),
	cf(1,phi(65,var(0))),
	cf(1,subtree(64,'V_SUFF_BASE',-,65)),
	cf(1,phi(64,var(0))),
	cf(1,terminal(63,'+Obj3',[61])),
	cf(1,phi(63,var(0))),
	cf(1,subtree(62,'V_SUFF_BASE',-,63)),
	cf(1,phi(62,var(0))),
	cf(1,terminal(57,'jaGli',[31])),
	cf(1,phi(57,var(9))),
	cf(1,subtree(56,'N_BASE',-,57)),
	cf(1,phi(56,var(9))),
	cf(1,terminal(55,'+N',[31])),
	cf(1,phi(55,var(9))),
	cf(1,subtree(54,'N_SUFF_BASE',-,55)),
	cf(1,phi(54,var(9))),
	cf(1,terminal(53,'+DErg',[31])),
	cf(1,phi(53,var(9))),
	cf(1,subtree(52,'N_SUFF_BASE',-,53)),
	cf(1,phi(52,var(9))),
	cf(1,terminal(51,'+DSg',[31])),
	cf(1,phi(51,var(9))),
	cf(1,subtree(50,'N_SUFF_BASE',-,51)),
	cf(1,phi(50,var(9))),
	cf(1,subtree(49,'N_SUFF_BASE',-,47)),
	cf(1,phi(49,var(9))),
	cf(1,terminal(47,'+Gen',[31])),
	cf(1,phi(47,var(9))),
	cf(1,subtree(45,'N_SUFF_BASE',-,43)),
	cf(1,phi(45,var(9))),
	cf(1,terminal(43,'+Pl',[31])),
	cf(1,phi(43,var(9))),
	cf(1,subtree(41,'N_SUFF_BASE',-,40)),
	cf(1,phi(41,var(9))),
	cf(1,terminal(40,'+OldPl',[31])),
	cf(1,phi(40,var(9))),
	cf(1,subtree(38,'N_SUFF_BASE',-,36)),
	cf(1,phi(38,var(9))),
	cf(1,terminal(36,'+Full',[31])),
	cf(1,phi(36,var(9))),
	cf(1,subtree(34,'N_SUFF_BASE',-,33)),
	cf(1,phi(34,var(9))),
	cf(1,terminal(33,'+Gan',[31])),
	cf(1,phi(33,var(9))),
	cf(1,subtree(30,'N_SUFF_BASE',-,28)),
	cf(1,phi(30,var(14))),
	cf(1,terminal(28,'+Reduced',[1])),
	cf(1,phi(28,var(14))),
	cf(1,subtree(26,'Acard_BASE',-,24)),
	cf(1,phi(26,var(14))),
	cf(1,terminal(24,'3',[1])),
	cf(1,phi(24,var(14))),
	cf(1,subtree(22,'CARD_SUFF_BASE',-,21)),
	cf(1,phi(22,var(14))),
	cf(1,terminal(21,'+Card',[1])),
	cf(1,phi(21,var(14))),
	cf(1,subtree(19,'CARD_SUFF_BASE',-,18)),
	cf(1,phi(19,var(14))),
	cf(1,terminal(18,'+Alphabetic',[1])),
	cf(1,phi(18,var(14))),
	cf(1,terminal(16,'+A',[1])),
	cf(1,phi(16,var(14))),
	cf(1,subtree(15,'A_SUFF_BASE',-,16)),
	cf(1,phi(15,var(14))),
	cf(1,subtree(14,'N_SUFF_BASE',-,12)),
	cf(1,phi(14,var(14))),
	cf(1,terminal(12,'+Erg',[1])),
	cf(1,phi(12,var(14))),
	cf(1,semform_data(0,26,1,3)),
	cf(1,semform_data(5,56,7,17)),
	cf(1,semform_data(6,651,7,18)),
	cf(1,semform_data(9,916,7,18)),
	cf(1,semform_data(12,76,19,23)),
	cf(1,fspan(var(0),1,25)),
	cf(1,fspan(var(3),1,18)),
	cf(1,fspan(var(3),7,18)),
	cf(1,fspan(var(14),1,6)),
	cf(1,fspan(var(6),7,18)),
	cf(1,fspan(var(9),7,18)),
	cf(1,surfaceform(1,'samma',1,6)),
	cf(1,surfaceform(31,'jaGlTaganma',7,18)),
	cf(1,surfaceform(61,'iqePa',19,24)),
	cf(1,surfaceform(80,'.',24,25))
	]).

