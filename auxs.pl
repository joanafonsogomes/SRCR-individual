% -- Auxiliares Gerais --

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

list_min([L|Ls], Min) :-
    list_min(Ls, L, Min).

list_min([], Min, Min).
list_min([L|Ls], Min0, Min) :-
    Min1 is min(L, Min0),
    list_min(Ls, Min1, Min).

addElement(X, [], [X]). 
addElement(X, [Y | Rest], [X,Y | Rest]) :- X @< Y, !.
addElement(X, [Y | Rest1], [Y | Rest2]) :- addElement(X, Rest1, Rest2).

getHead([], [], []).
getHead([X|_], [], [X]).
getHead([], [X|_], [X]).

inverso(Xs, Ys):-
	inverso(Xs, [], Ys).

inverso([], Xs, Xs).
inverso([X|Xs],Ys, Zs):-
	inverso(Xs, [X|Ys], Zs).

seleciona(E, [E|Xs], Xs).
seleciona(E, [X|Xs], [X|Ys]) :- seleciona(E, Xs, Ys).

minimo([(P,X)],(P,X)).
minimo([(Px,X)|L],(Py,Y)):- minimo(L,(Py,Y)), X>Y. 
minimo([(Px,X)|L],(Px,X)):- minimo(L,(Py,Y)), X=<Y. 

average( List, Average ):- 
    sum( List, Sum ),
    length( List, Length ),
    Length > 0, 
    Average is Sum / Length.

count([],0).
count([H|Tail], N) :-
    count(Tail, N1),
    (  number(H)
    -> N is N1 + 1
    ;  N = N1
    ).

%-----------------------------------------------

% -- Auxiliares do Programa --

% Obter todas os nodos do grafo
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

% Obter sucessores de um nodo
sucessor(grafo(_,Es),X,R):- solucoes(aresta(X,Y),membro(aresta(X,Y),Es),R).

% Obter o ID da garagem
getIdGaragem(R) :- solucoes(Id,ponto(Lat,Lon,O,Id,'Garagem',Ct,Ccap,Cqt,Ctotal), S), S = [H|T], R = H. 

% Obter o ponto da garagem
getPontoGaragem(R) :- solucoes(ponto(Lat,Lon,O,Id,'Garagem',Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,'Garagem',Ct,Ccap,Cqt,Ctotal), S), S = [H|T], R = H.

% Obter o ID do Local de Deposicao
getIdDeposicao(R) :- solucoes(Id,ponto(Lat,Lon,O,Id,'Deposicao',Ct,Ccap,Cqt,Ctotal), S), S = [H|T], R = H. 

% Obter o ponto do Local de Deposicao
getPontoDeposicao(R) :- solucoes(ponto(Lat,Lon,O,Id,'Deposicao',Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,'Deposicao',Ct,Ccap,Cqt,Ctotal), S), S = [H|T], R = H.

latitudeGaragem(R) :- solucoes(Lat,ponto(Lat,_,_,'15804',_,_,_,_,_),S),
					S = [H|T],
					R = H.
longitudeGaragem(R) :- solucoes(Lon,ponto(_,Lon,_,'15804',_,_,_,_,_),S),
					S = [H|T],
					R = H.

% Verifica se dois nodos sao adjacente
adjacente(X,Y,grafo(_,L_arestas)) :- member(aresta(X,Y),L_arestas).

% Calcula a distancia entre dois nodos
distancia(Lat1,Lon1,Lat2,Lon2,D):- N is sqrt((Lat2-Lat1)^2+(Lon2-Lon1)^2),N=D.

distanciaEntrePontos(ponto(Lat1,Lon1,_,X,_,_,_,_,_),ponto(Lat2,Lon2,_,Y,_,_,_,_,_),R) :- distancia(Lat1,Lon1,Lat2,Lon2,R).

distanciaEntreNodos(X,Y,D) :- pontosID(X,Ps), Ps = [P|T], P = ponto(Lat1,Lon1,_,X,_,_,_,_,_),
                            pontosID(Y,Ps2), Ps2 = [P2|T2], P2 = ponto(Lat2,Lon2,_,Y,_,_,_,_,_),
                            distancia(Lat1,Lon1,Lat2,Lon2,D).

% Calcula a distancia a garagem do ponto
distanciaGaragemPonto(Pt,R) :- getPontoGaragem(P),
                        distanciaEntrePontos(P,Pt,R).

% Diz se aquele ponto tem o tipo de residuo dado como argumento
tipoResiduo(Tp,ponto(_,_,_,_,Tp,_,_,_,_)).

% Todos os pontos com aquele ID
pontosID(Id,R) :- solucoes(ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),R).

% Todos os pontos com aquele ID e um determinado tipo
pontosIDTipo(Id,Tp,R) :- solucoes(ponto(Lat,Lon,O,Id,Tp,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,Tp,Ct,Ccap,Cqt,Ctotal),R).

% Encontrar todas as arestas de nodos ligados à garagem
ligadosGaragem(G,R) :- solucoes(aresta('15804',Y),adjacente('15804',Y,G),R).

% Encontrar todas as arestas ligadas à garagem com um determinado tipo
ligadosGaragemTipo(G,T,R) :- solucoes(aresta('15804',Y),(adjacente('15804',Y,G)),R).

% Obter coordenadas de um nodo
latitudeNodo(Id,R) :- solucoes(ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),S),
                        S = [H|T],
                        H = ponto(Lat1,Lon1,O1,Id,T1,Ct1,Ccap1,Cqt1,Ctotal1),
                        R = Lat1.

longitudeNodo(Id,R) :- solucoes(ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Id,T,Ct,Ccap,Cqt,Ctotal),S),
                        S = [H|T],
                        H = ponto(Lat1,Lon1,O1,Id,T1,Ct1,Ccap1,Cqt1,Ctotal1),
                        R = Lon1.

% Encontrar o nodo mais perto da garagem
closerToGaragem(G,R) :- ligadosGaragem(G,L),
                        closerToGaragemAux(L,[]).

closerToGaragemAux([],Lista).
closerToGaragemAux([H|T],Lista) :- closerToGaragemAuxAux(H,S),
                                A = Lista,
                                addElement(S,A,Var),
                                write(Var),
                                closerToGaragemAux(T,Var).

closerToGaragemAuxAux(H,R) :- H = aresta(X,Y),
                            latitudeNodo(Y,Lat), longitudeNodo(Y,Lon),
                            latitudeGaragem(LatG), longitudeGaragem(LonG),
                            distancia(LatG,LonG,Lat,Lon,R).

nodeMaisProxGaragem(R) :- R = '19310'.

nodeMaisProxGaragem_Lixos(R) :- '15844'.
nodeMaisProxGaragem_PapelECartao(R) :- '21844'.
nodeMaisProx_Embalagens(R) :- '21844'.
nodeMaisProx_Vidros(R) :- '19310'.
nodeMaisProx_Organicos(R) :- '15864'. 

% Devolve os pontos do primeiro elemento de uma aresta
edgePoints(aresta(X,_),R):- solucoes(ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal),R).

% Devolve os pontos do segundo elemento de uma aresta
edgePoints2(aresta(_,Y),R):- solucoes(ponto(Lat,Lon,O,Y,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Y,T,Ct,Ccap,Cqt,Ctotal),R).
