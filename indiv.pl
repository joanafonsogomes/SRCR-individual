%-----------------------------------------------
% SRCR INDIVIDUAL | Joana Afonso Gomes | a84912
%-----------------------------------------------
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]). 
:- set_prolog_flag(stack_limit, 4_294_967_296).
%-----------------------------------------------
:- op( 900,xfy,'::' ).
:- dynamic '-'/1.
:- dynamic(ponto/9).
:- op(  500,  fx, [ +, - ]).
:- op(  300, xfx, [ mod ]).
:- op(  200, xfy, [ ^ ]).
%-----------------------------------------------
:- include('baseConhecimento.pl').
:- include('grafo.pl').
:- use_module(library(lists)).
%-----------------------------------------------

%-----------------------------------------------
% -- Auxiliares --

nao( Questao ) :-
    Questao, !, fail. 
nao( Questao ).

membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).

% Concatena listas 
concat(List1, List2, Result):-
   append(List1, List2, Result).

% Comprimento de uma lista
compLista([H],R) :- R=1.
compLista([H|T],R) :- compLista(T,O), R is O + 1.

solucoes( X,Y,Z ) :- findall( X,Y,Z ).

%-----------------------------------------------

% Verifica se dois nodos sao adjacente
adjacente(X,Y,grafo(_,L_arestas)) :- member(aresta(X,Y),L_arestas).

% Calcula a distancia entre dois nodos
distancia(Lat1,Lon1,Lat2,Lon2,D):- N is sqrt((Lat2-Lat1)^2+(Lon2-Lon1)^2),N=D.

% myprog(X,Y) :- g(G), adjacente(X,Y,G).

% caminho(G,X,Y,P) :- caminho1(G,X,[Y],P).
% caminho1(_,X,[X|P1],[X|P1]).
% caminho1(G,X,[Y|P1],P) :- 
%   adjacente(Z,Y,G), \+ memberchk(Z,[Y|P1]), caminho1(G,X,[Z,Y|P1],P). 

caminho(G,X,Y,C):-
    caminhoAux(G,X,[Y],C).

caminhoAux(_,X,[X|T],[X|T]).
caminhoAux(G,X,[Y|T],P) :-
    adjacente(Prox_nodo,Y,G), nao(membro(Prox_nodo,[Y|T])), caminhoAux(G,X,[Prox_nodo,Y|T],P).

% TESTES
% myprog(X,Y) :- g(G), adjacente(X,Y,G).
% myTestProg(ponto(Lat1,Lon1,_,_,_,_,_,_,_),ponto(Lat2,Lon2,_,_,_,_,_,_,_),D) :- distancia(Lat1,Lon1,Lat2,Lon2,D).
myTestProg(X,Y,C) :- g(G), caminho(G,X,Y,C).




