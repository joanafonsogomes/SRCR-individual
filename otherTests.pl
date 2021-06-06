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

% ------------

tipoResiduo(aresta(X,Y),T) :- ponto(_,_,X,T,_,_,_,_), ponto(_,_,X,T,_,_,_,_).

dFResiduo(G,X,Y,P) :- dFResiduo(G,X,Y,[X],P).

dFResiduo(G,X,Y,V,[aresta(X,Y)]) :- adjacente(X,Y,G),
											tipoResiduo(aresta(X,Y)).

dFResiduo(G,X,Y,V,[aresta(X,Z)|P]) :-
 adjacente(X,Z,G),
 tipoResiduo(aresta(X,Z)),
 \+ membrochk(aresta(X,Z),V),
 \+ membro(Z,V), 
 dFResiduo(G,Z,Y,[Z|V],P).

 % ------------


solucoes((ponto(Lat,Lon,O,X,T,Ct,Ccap,Cqt,Ctotal),ponto(Lat,Lon,O,Y,T,Ct,Ccap,Cqt,Ctotal)),membro(aresta(X,Y),A),R).