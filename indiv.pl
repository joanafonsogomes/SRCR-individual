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

membrochk(X,[X|_]) :- !.
membrochk(X,[_|T]):- membrochk(X,T).


% Concatena listas 
concat(List1, List2, Result):-
   append(List1, List2, Result).

% Comprimento de uma lista
compLista([H],R) :- R=1.
compLista([H|T],R) :- compLista(T,O), R is O + 1.

solucoes( X,Y,Z ) :- findall( X,Y,Z ).

repetidos([],[]).
repetidos([X|L],[X|NL]) :- removerElem(L,X,TL), repetidos(TL,NL).

removerElem([],_,[]).
removerElem([X|L],X,NL) :- removerElem(L,X,NL).
removerElem([X|L],Y,[X|NL]) :- X \== Y, removerElem(L,Y,NL).

%-----------------------------------------------

% Verifica se dois nodos sao adjacente
adjacente(X,Y,grafo(_,L_arestas)) :- member(aresta(X,Y),L_arestas).

% Calcula a distancia entre dois nodos
distancia(Lat1,Lon1,Lat2,Lon2,D):- N is sqrt((Lat2-Lat1)^2+(Lon2-Lon1)^2),N=D.

% Caminho entre dois nodos

caminho(G,X,Y,C):-
    caminhoAux(G,X,[Y],C).

caminhoAux(_,X,[X|T],[X|T]).
caminhoAux(G,X,[Y|T],P) :-
    adjacente(Prox_nodo,Y,G), nao(membro(Prox_nodo,[Y|T])), caminhoAux(G,X,[Prox_nodo,Y|T],P).

% Depth First

dFirst(G,X,Y,R) :- dFirst(G,X,Y,[X],R).

dFirst(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G).

dFirst(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 dFirst(G,Z,Y,[Z|V],R),
 Z \= Y.

% Breadth First

bFirst(G,X,Y,Visited) :-   bFirst3(G,Y,Successors, 			
							[],						   
							RevVisited),
							sucessor(G,X,Successors),  
							inverso(RevVisited, Visited).

bFirst3(G,Y,[], History, []). 						

bFirst3(G,Y,[aresta(P,Y)|_], History, [aresta(P,Y)|History]).

bFirst3(G,Y,[aresta(X,Z)|RestQ], History, RevVisited) :-
						sucessor(G,Z,Cats),			 
						append(RestQ, Cats, Queue),					
						bFirst3(G,Y, Queue, [aresta(X,Z)|History], RevVisited).


% ---- Gerar os circuitos de recolha indeferenciada e seletiva ----

% Tipo de residuo
tipoLixo(aresta(X,Y,_)) :- ponto(_,_,_,X,_,'Lixo',_,_,_,_), ponto(_,_,_,Y,_,'Lixo',_,_,_,_).
tipoPapelCartao(aresta(X,Y,_)) :- ponto(_,_,_,X,_,'Papel e Cartão',_,_,_,_), ponto(_,_,_,Y,_,'Papel e Cartão',_,_,_,_).
tipoOrganicos(aresta(X,Y,_)) :- ponto(_,_,_,X,_,'Organicos',_,_,_,_), ponto(_,_,_,Y,_,'Organicos',_,_,_,_).
tipoVidro(aresta(X,Y,_)) :- ponto(_,_,_,X,_,'Vidro',_,_,_,_), ponto(_,_,_,Y,_,'Vidro',_,_,_,_).
tipoEmbalagens(aresta(X,Y,_)) :- ponto(_,_,_,X,_,'Embalagens',_,_,_,_), ponto(_,_,_,Y,_,'Embalagens',_,_,_,_).

% Através da depthFirst para pontos com o mesmo tipo de residuos

% Printa todas os nodos do grafo
todosNodos(R) :- g(G),
				G = grafo(N,_),
				R = N.

% Obter todas as arestas do grafo com nodos de um certo tipo de residuo
todasArestasTipo(T,R) :- g(G),
					G = grafo(_,L_arestas),
					A = L_arestas,
					solucoes(aresta(X,Y),(member((aresta(X,Y)),A),ponto(_,_,_,X,T,_,_,_,_),ponto(_,_,_,Y,T,_,_,_,_),X\=Y),S),
					repetidos(S,R).

% Obter todas as arestas do grafo

todasArestas(R) :- g(G),
					G = grafo(_,L_arestas),
					R = L_arestas.


% Devolve os pontos do ponto inicial de uma aresta
edgePoints(aresta(X,_),R):- solucoes(ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal),R).

% Diz se passa por um ponto que tem lixos
lixos(ponto(_,_,_,_,'Lixos',_,_,_,_)).

% Usar a depth first para os que fazem recolha de lixo

dFP(G,X,Y,R) :- dFP2(G,X,Y,[X],R).
dFP2(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G), edgePoints(aresta(X,Y),P), P=[H|T], lixos(H).

dFP2(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 edgePoints(aresta(X,Z),P), 
 P=[H|T],
 lixos(H),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 dFP2(G,Z,Y,[Z|V],R),
 Z \= Y.

% ---- Identificar quais os circuitos com mais pontos de recolha (por tipo de residuo a recolher) ---- 


% ---- Escolher o circuito mais rapido (criterio da distancia) ---- 


% ---- Escolher o circuito mais eficiente (criterio da distancia) ---- 



% TESTES
% myprog(X,Y) :- g(G), adjacente(X,Y,G).
% myTestProg(ponto(Lat1,Lon1,_,_,_,_,_,_,_),ponto(Lat2,Lon2,_,_,_,_,_,_,_),D) :- distancia(Lat1,Lon1,Lat2,Lon2,D).
%myTestProg(X,Y,C) :- g(G), caminho(G,X,Y,C).
%myTestProg(X,Y,R) :- g(G), dFirst(G,X,Y,R).
%myTestProg(X,Y,R) :- g(G), bFirst(G,X,Y,R).
%myTestProg(R) :- todasArestasTipo('Organicos',R).
myTestProg(R) :- g(G), dFP(G,'15849','15820',R).
%myTestProg(R) :- P=[1,2,3,4], P=[H|T], R=H.