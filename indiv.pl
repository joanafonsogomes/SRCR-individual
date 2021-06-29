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
:- include('auxs.pl').
:- use_module(library(lists)).
%-----------------------------------------------

% Gerar os circuitos de recolha

% -- Caminho entre nodos --
caminho(G,X,Y,C):-
    caminhoAux(G,X,[Y],C).

caminhoAux(_,X,[X|T],[X|T]).
caminhoAux(G,X,[Y|T],P) :-
    adjacente(Prox_nodo,Y,G), nao(membro(Prox_nodo,[Y|T])), caminhoAux(G,X,[Prox_nodo,Y|T],P).

% -- Caminho com a distancia percorrida --
caminhoK(G,A,B,P,D) :-
  caminhoAuxK(G,A,[B],P,D).

caminhoAuxK(G,A,[A|P1],[A|P],0,[]).

caminhoAuxK(G,A,[Y|P1],P,D1) :-
  adjacente(X,Y,Di,G),
  nao(membro(X,[Y|P1])),
  distanciaEntreNodos(X,Y,Di),
  caminhoAuxK(G,A,[X,Y|P1],P,K),
  D1 is D + Di.

% Todos os circuitos entre dois nodos

allCircuitos(G,X,Y,R):- solucoes(C,dFirst(G,X,Y,C),R).

% -- Gerar circuitos com Depth First --

dFirst(G,X,Y,R) :- dFirst(G,X,Y,[X],R).

dFirst(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G).

dFirst(G,X,Y,V,[aresta(X,Z)|R]) :-
 adjacente(X,Z,G),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 dFirst(G,Z,Y,[Z|V],R),
 Z \= Y.

% T
test_dFirst(R) :- g(G), getIdGaragem(Gar), getIdDeposicao(D), 
				dfFirst(G,Gar,D,R).

% -- Gerar circuitos com Breadth First --

bFirst(G,X,Y,Visited) :- bFirstAux(G,Y,Successors, 			
					[],						    
					RevVisited),
					sucessor(G,X,Successors),  
					inverso(RevVisited, Visited).

bFirstAux(G,Y,[], History, []).						

bFirstAux(G,Y,[aresta(P,Y)|_], History, [aresta(P,Y)|History]).

bFirstAux(G,Y,[aresta(X,Z)|RestQ], History, RevVisited) :- sucessor(G,Z,Cats),			 
													addElement(RestQ, Cats, Queue),					
													bFirstAux(G,Y, Queue, [aresta(X,Z)|History], RevVisited).

% Seletiva

% Através da depthFirst para pontos que tenham um tipo especifico de residuos (circuitos de recolha seletiva)

pontoTipo(aresta(X,Y),T) :- ponto(_,_,_,X,Tp,_,_,_,Ctotal), ponto(_,_,_,Y,Tp,_,_,_,Ctotal).

dFTipo(G,T,X,Y,P) :- dFTipo(G,T,X,Y,[X],P).

dFTipo(G,T,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G),
								pontoTipo(aresta(X,Y),T).

dFTipo(G,T,X,Y,V,[aresta(X,Z)|P]) :-
 adjacente(X,Z,G),
 pontoTipo(aresta(X,Z),T),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 dFTipo(G,T,Z,Y,[Z|V],P). 


%-----------------------------------------------

% Identificar circuitos com mais pontos de recolha (por tipo de residuo)

% Devolve os pontos de uma aresta se tiverem aquele tipo de residuo
edgePointsTipo(aresta(X,_),Tp,R):- solucoes(ponto(Lat,Lon,O,X,Tp,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,X,Tp,Ct,Ccap,Cqt,Ctotal),R).

nrPontosRecolhaT([],T,Count,R). 
nrPontosRecolhaT([A],T,Count,R) :- edgePointsTipo(A,Tp,S),
									length(S,L),
									Count2 is Count + L,
									nrPontosRecolhaT([],Count2,R).
nrPontosRecolhaT([H|T],Count,T,R) :- edgePointsTipo(H,Tp,S),
									length(S,L),
									Count2 is Count + L,
									nrPontosRecolhaT(T,Count2,R).

% T
teste_nrPontosRecolha(R) :- edgePointsTipo(aresta('15875','21844'),'Lixos',0,S).

%-----------------------------------------------

% Escolher o circuito mais rápido (criterio distancia)

% DepthFirst
dFDistancia(G,X,Y,R) :- dFDistancia(G,X,Y,[X],R,Km).

dFDistancia(G,X,Y,V,[aresta(X,Y)],0) :- adjacente(X,Y,G).

dFDistancia(G,X,Y,V,[aresta(X,Z)|R],D) :-
 adjacente(X,Z,G),
 Di = distanciaEntreNodos(X,Z),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 D2 = distanciaEntreNodos(Z,Y),
 dFDistancia(G,Z,Y,[Z|V],R,D2),
 D is D2 + Di. 

% Percurso mais rapido (menor distancia)
maisRapido(G,X,Y,P,D):-solucoes((P,C),(dFDistancia(G,X,Y,P,D),!,C=D),L),
					  minimo(L,(P,C)), D=C. 

test_maisRapido(Y,R) :- g(G), getIdGaragem(Gar), getIdDeposicao(Dep), maisRapido(G,Gar,Dep,P,D),
				addEllement(aresta(Dep,Gar)).

%-----------------------------------------------		

% Comparar circuitos de recolha tendo em conta a distância média percorrida entre pontos de recolha

% Calcula a distância total de um path
calcDist([],Di,Di).
calcDist([A],Di,Di) :- edgePoints(A,A1), [A2|T2]=A1, edgePoints2(A,B), [B1|B2]=B, 
					distanciaEntrePontos(A2,B1,Dist2),
					ZZ is Dist2 + Di,
					calcDist([],ZZ,Dist).
calcDist([A|X],Di,Dist):- edgePoints(A,A1), [A2|T2]=A1, edgePoints2(A,B), [B1|B2]=B, 
                    distanciaEntrePontos(A2,B1,Dist2),
                    ZZ is Dist2 + Di,
                    calcDist(X,ZZ,Dist).


compararDistanciaMedia(L,R) :- compararDistanciaMediaAux(L,'0',0).

compararDistanciaMediaAux([],Ci,Dm).
compararDistanciaMediaAux([H],Ci,Dm) :- calcDist(H,0,D),
									(D > Dm -> Dm is D, Ci is H),
									compararDistanciaMediaAux([],Ci,Dm).
compararDistanciaMediaAux([H|T],Ci,Dm) :- calcDist(H,0,D),
									(D > Dm -> Dm is D, Ci is H),
									compararDistanciaMediaAux(T,Ci,Dm).

test_compararDistanciaMedia(G,R) :- getIdGaragem(Gar), getIdDeposicao(D), g(G),
								allCircuitos(G,Gar,D,L),
								compararDistanciaMedia(L,R).

%-----------------------------------------------

% Comparar circuitos de recolha tendo em conta a quantidade recolhida

compararQtRecolhida(L,R) :- compararQtRecolhidaAux(L,'0',0).

compararQtRecolhidaAux([],Sum,R).
compararQtRecolhidaAux([H],Sum,R) :- edgePoints(H,E),
							somarQuantidadesRecolhidas(H,0,S),
							Sum2 = Sum + S,
							compararQtRecolhidaAux([],Sum2,R).
compararQtRecolhidaAux([H|T],Sum,R) :- edgePoints(H,E),
							somarQuantidadesRecolhidas(H,0,S),
							Sum2 = Sum + S,
							compararQtRecolhidaAux(T,Sum2,R).

somarQuantidadesRecolhidas([],Sum,R).
somarQuantidadesRecolhidas([A],Sum,R) :- ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal) = A,
							Sum2 = Sum + Ctotal,
							somarQuantidadesRecolhidas([],Sum2,R).
somarQuantidadesRecolhidas([H|T],Sum,R) :- ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal) = H,
							Sum2 = Sum + Ctotal,
							somarQuantidadesRecolhidas(T,Sum2,R).






