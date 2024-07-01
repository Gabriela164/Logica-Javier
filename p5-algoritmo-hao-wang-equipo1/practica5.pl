/* 1) Predicado elem(X,M) es verdadero si el elemento X pertenece a la lista M */
elem(X,[X|_]).
elem(X,[_|T]):- elem(X,T).

/* 2) Predicado intersect(M,N) es verdadero si M y N tienen al menos un elemento en comun*/
intersect([H|_],L):- elem(H,L).
intersect([_|T],L):- intersect(T,L).

/*3) Predicado delete(X,L,NewL) es verdadero si NewL es igual a L sin el elemento X */
delete(X,[X],[]).
delete(X,[X|L],L).
delete(X,[Y|L],[Y|NewL]):-delete(X,L,NewL),!.
    

/*---------------------------------------------*/
/*4)---------- Algoritmo Hao Wang -------------*/
/*---------------------------------------------*/

%Regla 1 del calculo de secuentes. Devuelve true si G y D tienen al menos un elemento en comun
wang(G,D):- intersect(G,D),!.

%Regla 2 del calculo de secuentes. Regla de negacion izquierda
wang(G,D):- elem(neg(A),G),
            delete(neg(A),G,NG),
            wang(NG,[A|D]),!.

%Regla 3 del calculo de secuentes. Regla de negacion derecha
wang(G,D):- elem(neg(A),D),
            delete(neg(A),D,ND),
            wang([A|G],ND),!.

%Regla 4 del calculo de secuentes. Regla de conjuncion izquierda
wang(G,D):- elem(and(A,B),G),
            delete(and(A,B),G,NG),
            wang([A,B|NG],D),!.

%Regla 5 del calculo de secuentes. Regla de disyuncion derecha
wang(G,D):- elem(or(A,B),D),
            delete(or(A,B),D,ND),
            wang(G ,[A,B|ND]),!.

%Regla 6 del calculo de secuentes. Regla de disyuncion izquierda
wang(G,D):- elem(or(A,B),G),
            delete(or(A,B),G,NG),
            wang([A|NG],D), 
            wang([B|NG],D),!.

%Regla 7 del calculo de secuentes. Regla de conjuncion derecha
wang(G,D):- elem(and(A,B),D),
            delete(and(A,B),D,ND),
            wang(G,[A|ND]), 
            wang(G,[B|ND]),!.

/*5) Predicado valid(F) es verdadero si una formula F es valida usando el algoritmo de Hao wang*/
valid(F) :- wang([], [F]).

% -----------------------Prueba-------------------------------
% wang([or(neg(a),f),or(neg(b),c),or(neg(and(c,neg(d))),a),b,and(or(e,d),neg(and(e,d)))],[or(neg(e),f)]). 
% Devuelve true
% valid(or(or(p,q),or(neg(p),q))).  ---> Es una tautologia
% Devuelve true
