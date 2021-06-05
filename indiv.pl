%-----------------------------------------------
% SRCR INDIVIDUAL | Joana Afonso Gomes | a84912
%-----------------------------------------------
:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).
:- set_prolog_flag(toplevel_print_options,[quoted(true), portrayed(true), max_depth(0)]). 
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
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail. 
nao( Questao ).

membro(X, [X|_]).
membro(X, [_|Xs]):-
	membro(X, Xs).

solucoes( X,Y,Z ) :-findall( X,Y,Z ).

%-----------------------------------------------

% Funcao que verifica se dois nodos sao adjacente
adjacente(X,Y,grafo(_,L_arestas)) :- member(aresta(X,Y),L_arestas).
adjacente(X,Y,grafo(_,L_arestas)) :- member(aresta(Y,X),L_arestas).

% Concatena listas 
% concat(List1, List2, Result):-
%   append(List1, List2, Result).

% Comprimento de uma lista
% compLista([H],R) :- R=1.
% compLista([H|T],R) :- compLista(T,O), R is O + 1.



