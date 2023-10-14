% ['../Text-to-Breasonings/text_to_breasonings.pl'].
% W is 50*4,texttobr2(u,u,u,u,false,false,false,false,false,false,W)
% where W is the number of words to read
% and there are approximately 4 words per algorithm.

%% Important: See instructions for using texttobr.pl at https://lucianpedia.wikia.com/wiki/Instructions_for_Using_texttobr(2).pl .

%% use_module(library(pio)). %% In la_strings
%% use_module(library(dcg/basics)). %% In la_strings

%% texttobr2 - converts file stream to dimensions of objects represented by the words
%% has object name as separate field for new users of texttobr to verify breasonings by hand
%% brdict1.txt contains word and object name, brdict2.txt contains object name and x, y and z

%% texttobr2(Runs,File,StringtoBreason,BreasoningLimit).
%:- include('../listprologinterpreter/la_strings.pl').
:- include('../Text-to-Breasonings/mergetexttobrdict.pl').

:-include('../Text-to-Breasonings/texttobr.pl').
:-include('../Text-to-Breasonings/text_to_breasonings.pl').

:- include('../listprologinterpreter/listprolog').
:- include('../Text-to-Breasonings/texttobr2qb').
%:- include('../Philosophy/14 10 23.pl').

%:- include('../listprologinterpreter/la_strings').

%% Brth is true or false
t2ab(N1,Filex1,Stringx1,M1) :-
	t2ab(N1,Filex1,Stringx1,M1,0,off).
t2ab(N1,Filex1,Stringx1,M1,Auto) :-
	t2ab(N1,Filex1,Stringx1,M1,0,Auto).
t2ab(N1,Filex1,Stringx1,M1,Words_to_read,Auto) :-
	retractall(auto(_)),
	assertz(auto(Auto)),

	retractall(complete_display(_)),
	assertz(complete_display(false)),

	retractall(words_to_read(_)),
	assertz(words_to_read(Words_to_read)),
	
	((number(N1),N=N1)->true;
	(N1=u,N=1)),

	((Filex1=u,Filex="../Text-to-Breasonings/file.txt")->true;
	Filex=Filex1),

	((number(M1),M=M1)->true;
	M=all), %% If m1 is undefined or all then m=all

	t2ab_prep(List1,BrDict03,AlgDict_x,AlgDict,Filex,Stringx1,M),
	
	retractall(t2ab_brDict03(_)),
	assertz(t2ab_brDict03(BrDict03)),

	retractall(t2ab_algDict_x(_)),
	assertz(t2ab_algDict_x(AlgDict_x)),

	retractall(t2ab_algDict(_)),
	assertz(t2ab_algDict(AlgDict)),

	retractall(t2ab_algString(_)),
	assertz(t2ab_algString([])),
	t2ab_br2(List1,N),%,BrDict03,BrDict2,AlgDict_x,AlgDict_x2,AlgDict,AlgDict2,N,[],AlgString),
	
	t2ab_brDict031(BrDict2),
	t2ab_algDict_x1(AlgDict_x2),
	t2ab_algDict1(AlgDict2),
	t2ab_algString1(AlgString),
	
	%writeln("Press <return> to save work:"),read_string(user_input,"\n","\r",_,_),

	sort(BrDict2,BrDict3),
	(BrDict03=BrDict3->true;
	(open_s("../Text-to-Breasonings/brdict1.txt",write,Stream),
%%	string_codes(BrDict3),
	write(Stream,BrDict3),
	close(Stream))),

	sort(AlgDict_x2,AlgDict_x3),
	(AlgDict_x=AlgDict_x3->true;
	(open_s("../t2ab/algdict1.txt",write,Stream2),
%%	string_codes(BrDict3),
	term_to_atom(AlgDict_x3,AlgDict_x31),
	write(Stream2,AlgDict_x31),
 	close(Stream2))),
 	
	sort(AlgDict2,AlgDict3),
	(AlgDict=AlgDict3->true;
	(open_s("../t2ab/algdict2.txt",write,Stream3),
	term_to_atom(AlgDict3,AlgDict31),
%%	string_codes(BrDict3),
	write(Stream3,AlgDict31),
 	close(Stream3))),

 	length(List1,List1_length_a),
 	Dividend_a is ceiling(List1_length_a/250),
 	Dividend_b is Dividend_a*3, % for graciously giving
 	texttobr2_a(Dividend_b,meditation),
 	texttobr2_a(Dividend_b,medicine),
 	texttobr2_a(Dividend_b,pedagogy),
 	
 	flatten(AlgString,AlgString1),
 	foldr(string_concat,AlgString1,"",AlgString2),
	%writeln1(AlgString2),
	texttobr2(u,u,AlgString2,u,[auto,Auto]),

 	!.

%% Truncates the list if m is not undefined and m is greater than or equal to the length of string0
truncate(List1,M,String0) :-
	((number(M),length(String0,M),
	append(String0,_,List1))->true;
	String0=List1),!.
	
t2ab_prep(List,BrDict03,AlgDict_x,AlgDict,Filex,Stringx1,M) :-
	phrase_from_file_s(string(BrDict0), "../Text-to-Breasonings/brdict1.txt"),
	%%Chars="’",
	SepandPad="&#@~%`$?-+*^,()|.:;=_/[]<>{}\n\r\s\t\\\"!'0123456789",
	%%split_string(BrDict0,SepandPad,SepandPad,BrDict01),
%%writeln([brDict0,BrDict0]),
%%writeln([brdict1]),
	splitfurther(BrDict0,BrDict01),
%%writeln([brDict01,BrDict01]),
	%%char_code(Escape,27),
	%%delete(BrDict01,[Escape,_,_,_,_],BrDict021),
%%writeln([brDict021,BrDict021]),
	%%char_code(Apostrophe,8217),
	%%delete(BrDict021,[Apostrophe,_,_,_,_],BrDict02),
%%writeln([brDict02,BrDict02]),
	sort(BrDict01,BrDict03),
%%writeln([brDict03,BrDict03]),
	length(BrDict03,Length0),write("Number of words in dictionary: "), writeln(Length0),
	
	%%writeln(''),
	%%writeln([brdict2]),
	phrase_from_file_s(string(BrDict0t), "../t2ab/algdict1.txt"),
	%%Chars="’",
	%%split_string(BrDict0,SepandPad,SepandPad,BrDict01),
%%writeln([brDict0,BrDict0]),
%	splitfurthert(BrDict0t,BrDict01t),
string_atom(BrDict0t,Atom),atom_to_term(Atom,BrDict01t,_),

%%writeln([brDict01,BrDict01]),
	%%delete(BrDict01t,[Escape,_,_,_,_],BrDict021t),
%%writeln([brDict021,BrDict021]),
	%%delete(BrDict021t,[Apostrophe,_,_,_,_],BrDict02t),
%%writeln([brDict02,BrDict02]),
	sort(BrDict01t,AlgDict_x),
	
% br_freq		%B=AlgDict_x,A=BrDict03,findall([DL,C,"\n"],(member([C,_,_,_],B),findall(_,member([_,C],A),D),length(D,DL)),E),sort(E,F),reverse(F,G),writeln([br_freq,G]),

%%writeln([brDict03,BrDict03]),
	length(AlgDict_x,Length0t),write("Number of unique algorithm names in dictionary: "), writeln(Length0t),
	
	%trace,
	
	phrase_from_file_s(string(AlgDict0), "../t2ab/algdict2.txt"),
	%%Chars="’",
	%%split_string(BrDict0,SepandPad,SepandPad,BrDict01),
%%writeln([brDict0,BrDict0]),
%%writeln([brdict1]),
string_atom(AlgDict0,Atom1),atom_to_term(Atom1,AlgDict01,_),
	%%splitfurther(BrDict0,BrDict01),
%%writeln([brDict01,BrDict01]),
	%%char_code(Escape,27),
	%%delete(BrDict01,[Escape,_,_,_,_],BrDict021),
%%writeln([brDict021,BrDict021]),
	%%char_code(Apostrophe,8217),
	%%delete(BrDict021,[Apostrophe,_,_,_,_],BrDict02),
%%writeln([brDict02,BrDict02]),
	sort(AlgDict01,AlgDict),
%%writeln([brDict03,BrDict03]),
	length(AlgDict,Length01),write("Number of algorithms in dictionary: "), writeln(Length01),
	
	
		
	
	((Stringx1=u,
	phrase_from_file_s(string(String001), Filex))->true;
	String001=Stringx1),
	
	process_t2ab(String001,String00),

	split_string(String00,SepandPad,SepandPad,List1),
	%%split_string_onnonletter(String00,List1),

	truncate(List1,M,List),

	/**replace0(String0,[8221, 8220], 34, SepandPad, M, String1),
	replace0(String1,[8216, 8217], 39, SepandPad, M, String2),
	replace0(String2,[8259, 8211, 8212], 45, SepandPad, M, String3),
	replace0(String3,[160], 32, SepandPad, M, List),
	**/

%%atom_codes(Atom999,String),writeln([atom999,Atom999]),

%%writeln([list,List]),
	%%delete(List,Escape,List11),
%%writeln([list11,List11]),
	%%delete(List11,Apostrophe,List1),
%%writeln([list1,List1]),
	length(List,Length1),write("Number of words to breason out in file: "), writeln(Length1),
	sort(List,List2),
%%writeln([list2,List2]),
	length(List2,Length2),write("Number of unique words in file: "), writeln(Length2),
	

(complete_display(true)->
	((Stringx1=u, %% Use file, not string as input.
	
	%%maplist(downcase_atom, List2, List3),
	maplist(string_lower, List2, List3),
	
%%writeln([list3,List3]),
	towords3(BrDict03,[],BrDict04,[],_ObjectNames,[],AllUsedNames),
	
	findall(BrDict04tt,(member([BrDict04tt,_],AlgDict_x)),BrDict04t),
	findall(BrDict04tt0,(member([_,BrDict04tt0],AlgDict_x)),BrDict04t0),
	findall(BrDict04tt1,(member([BrDict04tt1,_],AlgDict)),BrDict04t1),
	%towords2(AlgDict_x,[],BrDict04t),

%%writeln([brDict04,BrDict04]),
	subtract(List3,BrDict04,D1),
%%writeln([list3,brDict04,d1,List3,BrDict04,D1]),
%%writeln(["subtract(BrDict04,List3,D1).",List3,BrDict04,D1]),	
	length(D1,Length01),Difference is abs(Length01),write("Number of words remaining to define: "), writeln(Difference),

	subtract(AllUsedNames,BrDict04t,D2),
	%%delete(D21,'',D2),
	length(D2,Length01t),Differencet is abs(Length01t),write("Number of undefined algorithm names: "), writeln(Differencet),
	%% writeln([undefinedbreasonings,D2]), %% Print undefined breasonings

	%%delete(D31,'',D3),
	subtract(BrDict04t,AllUsedNames,D3),
	length(D3,Length01t2),Differencet2 is abs(Length01t2),write("Number of orphaned algorithm names: "), writeln(Differencet2),
	
	
	
	/*
		subtract(List3,BrDict04,D1),
%%writeln([list3,brDict04,d1,List3,BrDict04,D1]),
%%writeln(["subtract(BrDict04,List3,D1).",List3,BrDict04,D1]),	
	length(D1,Length01),Difference is abs(Length01),write("Number of algorithms remaining to define: "), writeln(Difference),
*/

	subtract(BrDict04t0,BrDict04t1,D21),
	%%delete(D21,'',D2),
	length(D21,Length01t1),Differencet1 is abs(Length01t1),write("Number of undefined algorithms: "), writeln(Differencet1),
	%% writeln([undefinedbreasonings,D2]), %% Print undefined breasonings

	%%delete(D31,'',D3),
	subtract(BrDict04t1,BrDict04t0,D31),
	length(D31,Length01t21),Differencet21 is abs(Length01t21),write("Number of orphaned algorithms: "), writeln(Differencet21)

		%%,writeln([orphanedbreasonings,D3]) %% Print orphaned breasonings
	
			
	
	

)->true;(string(Filex),writeln("Number of words, unique words, words remaining to define, undefined algorithm names, orphaned algorithm names, undefined algorithms and orphaned algorithms skipped for speed when breasoning out a string.")));true)

,!.

%n2(N) :-n(N).
t2ab_brDict031(BrDict2) :- t2ab_brDict03(BrDict2).
t2ab_algDict_x1(BrDict03t2) :- t2ab_algDict_x(BrDict03t2).
t2ab_algDict1(BrDict03t2) :- t2ab_algDict(BrDict03t2).
t2ab_algString1(BrDict03t2) :- t2ab_algString(BrDict03t2).


t2ab_br2(List1,N):-%_,A,A,B,B,C,C,0,L,L) :- !.
%br2(List1,BrDict03,BrDict2,AlgDict_x,AlgDict_x2,AlgDict,AlgDict2,N1,L1,L2) :-

	length(NL,N),
	findall(_,(member(_,NL),
	(auto(on)->
	concurrent_maplist(t2ab_br,List1,_);
	maplist(t2ab_br,List1,_))),_),!.
	%br(List1,BrDict03,BrDict21,AlgDict_x,AlgDict_x21,AlgDict,AlgDict21,L1,L3),
%	N2 is N1-1,
	%br2(List1,BrDict21,BrDict2,AlgDict_x21,AlgDict_x2,AlgDict21,AlgDict2,N2,L3,L2),!.

towords2([],A,A) :- !.
towords2(BrDict03,A,B) :-
	BrDict03=[[Word,_,_,_]|Rest],
	%%atom_string(Atom,Word),
	append(A,[Word],C),
	towords2(Rest,C,B).

towords2a([],A,A) :- !.
towords2a(BrDict03,A,B) :-
	BrDict03=[[Word,_]|Rest],
	%%atom_string(Atom,Word),
	append(A,[Word],C),
	towords2a(Rest,C,B).

towords3([],A,A,C,C,D,D) :- !.
towords3(BrDict03,A,B,D,E,G,H) :-
	BrDict03=[[Word1,Word2]|Rest],
	(Word2=""->append(G,[Word1],I)->true;
	append(G,[Word2],I)),
	append(A,[Word1],C),
	append(D,[Word2],F),
	towords3(Rest,C,B,F,E,I,H).

string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).

splitfurther(BrDict01,N) :-
	   phrase(file0(N),BrDict01).
	
file0(N) --> "[", file(N), "]", !.
file0([]) --> [].

%%file([]) --> [].
file([L|Ls]) --> entry(L),",",
%%{writeln(L)}, %%***
file(Ls), !. %% file(Ls),{M=[Ls]})), !. %%, {writeln(["l",L])},",", file(Ls), {writeln(["ls",Ls])},!. %%, {append(L,Ls,M)}, !.	
file([L]) --> entry(L), 
%%{writeln(L)},
!. %%(entry(L),{M=L});{M=[],(writeln("Warning - Entry in incorrect format.")
%%,abort
%%)}, !.

entry([Word2,Word4]) -->
		"[", word(Word), {string_codes(Word2,Word),string(Word2)},
		",",
           word(Word3), {string_codes(Word4,Word3),string(Word4)},
           "]".

splitfurthert(BrDict01,N) :-
	   phrase(file0t(N),BrDict01).
	
file0t(N) --> "[", filet(N), "]", !.
file0t([]) --> [].

%%file([]) --> [].
filet([L|Ls]) --> entryt(L),",",
%%{writeln(L)}, %%***
filet(Ls), !. %% file(Ls),{M=[Ls]})), !. %%, {writeln(["l",L])},",", file(Ls), {writeln(["ls",Ls])},!. %%, {append(L,Ls,M)}, !.	
filet([L]) --> entryt(L), 
%%{writeln(L)},
!. %%(entry(L),{M=L});{M=[],(writeln("Warning - Entry in incorrect format.")
%%,abort
%%)}, !.

entryt([Word2,X3,Y3,Z3]) -->
		"[", word(Word), {string_codes(Word2,Word),string(Word2)},
		",",
	      digits(X),",",{atom_codes(X2,X),atom_number(X2,X3),number(X3)},
           digits(Y),",",{atom_codes(Y2,Y),atom_number(Y2,Y3),number(Y3)},
           digits(Z),{atom_codes(Z2,Z),atom_number(Z2,Z3),number(Z3)},
           "]".

word([X|Xs]) --> [X], {char_type(X,csymf)->true;(X=27->true;X=8217)}, word(Xs), !.
%%word([X]) --> [X], {char_type(X,csymf);(X=27;X=8217)}, !.
word([]) --> [].

digits([X|Xs]) --> [X], {(char_type(X,digit)->true;(string_codes(Word2,[X]),Word2="."))}, digits(Xs), !.
%%digits([X]) --> [X], {(char_type(X,digit);(string_codes(Word2,[X]),Word2="."))}, !.
digits([]) --> [].

t2ab_br(Word,_):-%[],B,B,C,C,D,D,L,L) :-
%	!.
%br([Word|Words],BrDict,BrDict2,AlgDict4,AlgDict5,AlgDict6,AlgDict7,AlgString1,AlgString2) :-
	downcase_atom(Word, Word2), atom_string(Word2,Word3),
	
	/*
	words_to_read(WR1),
	(WR1>0->(writeln(WR1),write(Word),
	texttobr2(3),nl,sleep(0.12),
	WR2 is WR1-1,
	retractall(words_to_read(_)),
	assertz(words_to_read(WR2)));
	true),
	*/
	
	/**member([Word3,X,Y,Z],AlgDict4) -> %% This feature is a bug because words in brdict2 shouldn't necessarily be the words in brdict1
	%%(append(BrDict,[[Word3,""]],BrDict3), BrDict3t=AlgDict4,
	%%br(Words,BrDict3,BrDict2,BrDict3t,AlgDict5))
	%%;
	%%(**/
	
	%%(member([Word3,X,Y,Z],AlgDict4) -> %% This feature is a bug because words in brdict1 should correspond to those in brdict2
	%%(atom_concat("The breasoning for ", Word3, P1),
	%%atom_concat(P1, " is defined.  Enter object name (without spaces), if different for ", Prompt));
	%Prompt="Enter object name (without spaces), if different for "),
	
	%%writeln([word3,Word3]),
	
	%trace,
	t2ab_brDict031(BrDict),
 	t2ab_algString1(AlgString1),
 
	(member([Word3,String4],BrDict)-> 
	(BrDict3=BrDict,AlgString1=AlgString3,
	(String4=""->String41=Word3;String41=String4),
	String5=String41);
	((repeat,
	write("Enter object name (without spaces), if different for "), writeln(Word3),(auto(on)->String2="plus";read_string(user_input, "\n", "\r", _End2, String2)),split_string(String2, "", " ", String3),String3=[String4]),
	%%*brth(Word3,_Brth),

(String4=""->String5=Word3;String5=String4),
	append(BrDict,[[Word3,String5]],BrDict3),
	append(AlgString1,[[Word3," ",String5," "]],AlgString3)
	%texttobr2(1,u,String5,1)
	)
	),

	t2ab_algDict_x1(AlgDict4),
	
	downcase_atom(String5, String52), atom_string(String52,String53),
%trace,
	(member([String53,X1],AlgDict4)->
	(AlgDict41=AlgDict4,AlgString3=AlgString4,
	(X1=""->StringX1=String53;StringX1=X1),
	String51=StringX1);
	
	((repeat,
	write("Enter algorithm name for "), writeln(String53),(auto(on)->String21="write";read_string(user_input, "\n", "\r", _, String21)),split_string(String21, "", " ", String31),String31=[String411]),
	%%*brth(Word3,_Brth),

(String411=""->String51=String53;String51=String411),
	append(AlgDict4,[[String53,String51]],AlgDict41),
	append(AlgString3,[String51," "],AlgString4)
	%texttobr2(1,u,String51,1)
	)),

	downcase_atom(String51, String521),
 	atom_string(String521,String531),

	t2ab_algDict1(AlgDict6),
	(member([String531,_Y1],AlgDict6)->
	(AlgDict61=AlgDict6,AlgString4=AlgString5);
	
	((repeat,
	write("Enter Prolog algorithm for "), writeln(String531),(auto(on)->String1="writeln(A)";read_string(user_input, "\n", "\r", _End, String1)),
	split_string(String1, "\n\r", "\n\r", String),%Values=[X1,Y1,Z1],number_string(X,X1),number_string(Y,Y1),number_string(Z,Z1)),
	(String=""->String11=String531;String11=String),
	
	append(AlgDict6,[[String531,String11]],AlgDict61),
	append(AlgString4,[String11,"\n\n"],AlgString5)
	))),
	%%*brth(String53,_Brth2),
	%%write("br(\'"),write(Word3),writeln("\',)."),
%%	writeln([Word3,X,Y,Z]),
	%%write(' '),
	
retractall(t2ab_brDict03(_)),
assertz(t2ab_brDict03(BrDict3)),

retractall(t2ab_algDict_x(_)),
assertz(t2ab_algDict_x(AlgDict41)),

retractall(t2ab_algDict(_)),
assertz(t2ab_algDict(AlgDict61)),

retractall(t2ab_algString(_)),
assertz(t2ab_algString(AlgString5)),

!.


%br(Words,BrDict3,BrDict2,AlgDict41,AlgDict5,AlgDict61,AlgDict7,AlgString5,AlgString2).
	%%).

%% finds unknown words, asks for their br in form "n of m: word", verify, (can go back x) append and sort, save

%/*
process_t2ab(A,C) :-
 replace_t2ab(Replacements),
 atom_string(A1,A),
 replace1_t2ab(Replacements,A1,D1),
 atom_string(D1,C),!.
 
replace1_t2ab([],A,A) :- !.
replace1_t2ab(Replacements,A,D) :-
 Replacements=[[B,C]|G],
 atomic_list_concat(E,B,A),
 atomic_list_concat(E,C,F),
 replace1_t2ab(G,F,D),!.

	replace_t2ab([['\\',''],['–',' '],['“','\''],['”','\''],['‘','\''],['’','\'']]).
%*/