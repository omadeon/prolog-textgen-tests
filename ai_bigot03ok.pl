:- dynamic f/3, n/3, g/5.

/* "ARTIFICIAL BIGOT" (a PROLOG "rants generator") */
%
% DESCRIPTION:
% This text-generator consists of just a few lines of PROLOG
% code, but it produces remarkably life-like, hilarious results. 
% You can also produce your own rants, by adding or changing a few
% simple statements in a knowledge-base of 'facts'.
%
% The first version (1997) was Written and run in AMZI-PROLOG.
% It was then (re-)published in various programming sites, e.g.
% http://computer-programming-forum.com/55-prolog/71dc872939b57bb4.htm
%
% An On-line demo was uploaded in 2010, but today that site is down.
% Re-uploaded on SWISH (SWI-Prolog on-line demo-site) in May 2016.
% 
% Author: George A. Stathis 

%%%%%%%%%%%%% ARTIFICIAL BIGOT CODE: %%%%%%%%%%%%%%%%%%

%%% KNOWLEDGE-BASE (modifiable at will):
%
%  Structure/domains:   f(OBJECTindex1,[DEFINITION/LIST],OBJECTindex2).
%                       n(OBJECTindex, [OBJECT-TEXT-LIST], VERB-suitable).


f(2, [a, trait, of], 1).
f(3, [the, only, way, to, deal, with], 1).
f(2, [the, fear, of], 3).
f(4, [the, language, of], 1).
f(8, [a, 'Bastardly'], 6).
f(9, [what, 'Bastardonians', call], 8).
f(9, [the, 'Freakish', name, for], 8).
f(8, [what, people, call], 9).
f(10, [a, different, language, than], 4).
f(10, [a, 'Bastardly'], 5).
f(11, [speakers, of], 10).
f(7, [the, ancestors, of], 11).
f(10, [the, language, of], 11).
f(13, [our, glorious, land, of], 11).
f(1, [the, enemies, of], 13).
f(1, [the, scumbags, who, occupy], 13).
f(1, [the, enemies, of], 13).
f(9, [the, capital, of], 13).
f(11, [the, inhabitants, of], 13).
f(14, [the, nation, of], 1).
f(15, [the, 'State', of], 13).
f(12, [the, people, of], 15).
f(16, [the, region, of, 'Bastardonia', occupied, by], 14).
f(1, [the, oppressors, of], 16).
f(3, [the, only, way, to, liberate], 16).
f(9, [the, capital, of], 16).
f(13, [the, territory, of, 'CD-R.O.M.', as, well, as], 16).
f(17, [the, leader, of], 7).
f(17, [the, ancestor, of], 11).
f(3, [the, military, genious, of], 17).

/* Additional Lexikon for K.B.[2]: */

n(1, ['Freaks'], are).
n(2, ['Cowardice'], is).
n(3, ['War'], is).
n(4, ['Freakish'], is).
n(5, ['language'], is).
n(6, ['city'], is).
n(7, [ancient, 'Bastardonians'], are).
n(8, ['Psolun'], is).
n(9, ['Psalmonika'], is).
n(10, ['Bastardonian'], is).
n(11, ['Bastardonians'], are).
n(12, [the, 'Bastardonian', people], are).
n(13, ['Bastardonia'], is).
n(14, ['Freakland'], is).
n(15, ['CD-R.O.M.'], is).
n(16, ['Southern', 'Bastardonia'], is).
n(17, ['Ox-killer', the, 'Great'], was).

verbs(['is','are','was','were']).

g(1, 4, ['Freaks'], are, [[the, enemies, of, 13], [the, scumbags, who, occupy, 13], [the, enemies, of, 13], [the, oppressors, of, 16]]).
g(2, 2, ['Cowardice'], is, [[a, trait, of, 1], [the, fear, of, 3]]).
g(3, 3, ['War'], is, [[the, only, way, to, deal, with, 1], [the, only, way, to, liberate, 16], [the, military, genious, of, 17]]).
g(4, 1, ['Freakish'], is, [[the, language, of, 1]]).
g(5, 0, [language], is, []).
g(6, 0, [city], is, []).
g(7, 1, [ancient, 'Bastardonians'], are, [[the, ancestors, of, 11]]).
g(8, 2, ['Psolun'], is, [[a, 'Bastardly', 6], [what, people, call, 9]]).
g(9, 4, ['Psalmonika'], is, [[what, 'Bastardonians', call, 8], [the, 'Freakish', name, for, 8], [the, capital, of, 13], [the, capital, of, 16]]).
g(10, 3, ['Bastardonian'], is, [[a, different, language, than, 4], [a, 'Bastardly', 5], [the, language, of, 11]]).
g(11, 2, ['Bastardonians'], are, [[speakers, of, 10], [the, inhabitants, of, 13]]).
g(12, 1, [the, 'Bastardonian', people], are, [[the, people, of, 15]]).
g(13, 2, ['Bastardonia'], is, [[our, glorious, land, of, 11], [the, territory, of, 'CD-R.O.M.', as, well, as, 16]]).
g(14, 1, ['Freakland'], is, [[the, nation, of, 1]]).
g(15, 1, ['CD-R.O.M.'], is, [[the, 'State', of, 13]]).
g(16, 1, ['Southern', 'Bastardonia'], is, [[the, region, of, 'Bastardonia', occupied, by, 14]]).
g(17, 2, ['Ox-killer', the, 'Great'], was, [[the, leader, of, 7], [the, ancestor, of, 11]]).

rearrange_f:- n(N,PL,VRB), findall(SLx, ( f(N,SL1,Nx), append(SL1,[Nx],SLx) ), SLLx), 
	length(SLLx,Numx),
	assert(g(N,Numx,PL,VRB,SLLx)), fail.
rearrange_f.


exhaust_nglx([]):- !.
exhaust_nglx(NL):- random_select(Nx,NL,NL2),
%		nb_setarg(1,Count,N2),
		g(Nx,N2x,SBJ,VRB,SL2),
		TRMx = g(Nx,N2x,SBJ,VRB,SL2),
		write(TRMx), nl, !,
		exhaust_nglx(NL2).

exhaust_repl([],[]):- !.
exhaust_repl([A|L],[A|Lx]):- atom(A), !, exhaust_repl(L,Lx).
exhaust_repl([N|L],[SL|Lx]):- integer(N), g(N,N2,SL,_IS,[]), N2 < 2,
		exhaust_repl(L,Lx).
exhaust_repl([N|L],[SL2x|Lx]):- integer(N), g(N,1,_SL,_IS,[SL2]),
		exhaust_repl(SL2,SL2x),
		exhaust_repl(L,Lx).
exhaust_repl([N|L],[SL2xz|Lx]):- integer(N), g(N,N2,_SL,_IS,SLL2), N2 > 1,
		random_select(SL2xx,SLL2,_),
		exhaust_repl(SL2xx,SL2xz),
		exhaust_repl(L,Lx).

		exp_g(Ndx,1):- g(Ndx,1,SUBJ,VERB,[OBJL]),
		exhaust_repl(OBJL,OBJLokX),
		append(SUBJ,[VERB],SBJ2),
		append(SBJ2,OBJLokX,OUTLxx),
		write(OUTLxx), nl, fail.
exp_g(_,0).
	
		
xp_glx(NL):- Count = counter(NL),
	repeat, 
		arg(1,Count,Lx),
		%Lx =[FIRST|REST],
		random_select(Ndx,Lx,REST),
		g(Ndx,_Numx,SBJ,VRB,OBJLL),
		append(SBJ,[VRB],SBJ2),
		random_select(OBJx,OBJLL,_),
		append(SBJ2,OBJx,OUTx),
		write(OUTx), nl,
		nb_setarg(1,Count,REST),
		REST = [], !.
        
rnd_g:- findall(Ng,( g(Ng,Numx,_SUBJ,_VRB,_SLx), Numx > 0 ),NgLx),
	write(NgLx), nl, nl,
		random_permutation(NgLx,NgLxx),
		xp_glx(NgLxx).

xlist(L0,[X],Lx):- atom_length(X,L), Lx is L0+L+1, Lx < 64, write(X), !.
xlist(_,[X],Lx):- atom_length(X,L), nl, write(X), Lx is L+1, !.
xlist(L0,[X|XL],Lx):- atom_length(X,L), L2 is L0+L+1,
        L2<64, write(X), write(' '), !, xlist(L2,XL,Lx).
xlist(_,[X|XL],Lx):- atom_length(X,L), L2 is L+1, nl,
        write(X), write(' '), !, xlist(L2,XL,Lx).

llen(N,[],N):- !.
llen(N,[_|T],X):- N2 is N+5, !, llen(N2,T,X).

gen(N,INDEX,IDEA,OLDndxs,N,[IDEA,[VERB],RANT,LASTidea]):-
        n(INDEX,IDEA,VERB), not(member(INDEX,OLDndxs)),
        f(INDEX,RANT,TARGETindex), n(TARGETindex,LASTidea,_).

gen(N,NDX,IDEA,OLD,Nx,[IDEA,[IS],RANT|REST]):- n(NDX,IDEA,IS),
        not(member(NDX,OLD)), f(NDX,RANT,X2), not(member(X2,OLD)),
        n(X2,LAST,_), verbs(VERBS), member(IS2,VERBS), N2 is N+1,
        gen(N2,_,LAST,[NDX|OLD],Nx,[LAST,[IS2]|REST]).

writesll(PREV,[SL],XX):- xlist(PREV,SL,XX), write('.'), !.
writesll(PREV,[SL|SLL],XX):- xlist(PREV,SL,PREV2),
        write(' '), !, writesll(PREV2,SLL,XX).


rant2(Count):- gen(1,_,_,[],LEVEL,DEFx), R is LEVEL*100, llen(R,DEFx,Rate),
        atom_number(Rs,Rate),
        atomics_to_string(['                                    - Bigotry-Rating: ',Rs],Z),
		arg(1,Count,N0),
		N2 is N0 + 1,
		nb_setarg(1,Count,N2),
        nl, write(N2), write(') '),
		writesll(0,DEFx,_), nl, write(Z), nl, fail.
rant2(_).

rant:- Count=counter(0), rant2(Count).

%% new code May 2016 (not used yet)
/*
succeeds_n_times(Goal,Times):-
	Counter = counter(0),
	(	Goal,
		arg(1, Counter, N0),
		N is N0 + 1,
		nb_setarg(1, Counter, N),
		fail
		;
		arg(1, Counter, Times)
	).


rndx(Outx):- repeat, get_time(X), Y is float_fractional_part(X), Y > 0.0001, 
	Outx is floor(10000 * Y), !.
	
set_seed:- rndx(X), set_random(seed(X)), !.
*/


dorant('s'):- Count = counter(0),
		repeat, gen(1,_,_,[],LEVEL,DEFx), R is LEVEL*100, llen(R,DEFx,Rate),
        atom_number(Rs,Rate),
        atomics_to_string(['                                    - Bigotry-Rating: ',Rs],Z),
		arg(1,Count,N0),
		N2 is N0 + 1,
		nb_setarg(1,Count,N2),
        nl, write(N2), write(') '),
		writesll(0,DEFx,_), nl, write(Z),
		write('\n\n(Press ENTER for more bigotry, or any other key+ENTER to stop): '),
		get_char(Ch), write(Ch), nl, Ch  \= '\n', !.

dorant('a'):- Count=counter(0), rant2(Count).

dorant('f'):- nl, write('please wait till OK appears on the screen...'), nl,
 write('and then examine the (output-)file rant1.txt.'), nl,
 tell('rant1.txt'),
 nl,
 write('---------------------------------------------------------------'),
 nl, write('Computer-Generated Text follows:'), nl, nl,
 Count=counter(0), rant2(Count), nl, nl, write('END'), nl, told, nl, write('OK'), nl.

dorant(_X):- !.

change_ndx(Old,New):- retract(f(Old,A,B)), assert(f(New,A,B)), fail.
change_ndx(Old,New):- retract(f(A,B,Old)), assert(f(A,B,New)), fail.
change_ndx(Old,New):- retract(n(Old,A,B)), assert(n(New,A,B)), !.

change_indexes(Nx,[],Nxx):- Nxx is Nx-1, !.
change_indexes(N,[NDX|NDXL],NumX):- change_ndx(NDX,N), 
	!, NN is N+1, change_indexes(NN,NDXL,NumX).

remake_db(NumXX):- findall(NDX,n(NDX,_,_),NDXL),
	change_indexes(1,NDXL,NumX), NumXX is NumX, !.

/**	
list_nf:- n(X,Y,Z), swritef(`n(%,'%s','%s`,X,Y,Z), nl, fail.
list_nf:- n(X,Y,Z), swritef(`f(%,'%s','%'`,X,Y,Z), nl, fail.
**/

/**
n(X,Y,Z)), write('.'), nl, fail.
list_nf:- nl, f(X,Y,Z), writeq(f(X,Y,Z)), write('.'), nl, fail.
list_nf.
**/

go:- repeat, write("\nEnter: 's' for stepwise-rant, 'a' for all rants, 'f' to create a file of all rants, 'x' to exit:\n"),
	get(CH), 
	atom_codes(Ax,[CH]),
	dorant(Ax),
	Ax = 'x',
	!.
	

