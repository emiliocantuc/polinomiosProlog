
% -------- DEFINICIÓN DE POLINOMIO:
% Usaremos listas de prolog para definir polinomios de la siguiente
% manera:
% Cada elemento de la lista representa el coeficiente de un término del
% polinomio y su posición en la lista representa el exponente
% del término correspondiente, ordenados de menor a mayor. Por ejemplo,
% la lista [1,2,3] representa al polinomio 3x^2 + 2x + 1; mientras que
% la lista [1,0,0,4] representa al polinomio 4x^4 + 1.

% -------- SUMA
% Si B es vacío, la suma es igual a A.
% suma_pol(i, i, o):
suma_pol(A,[],A) :- !.
% Si A es vacío, la suma es igual a B.
suma_pol([],B,B):- !.
% Para sumar dos polinomios, se suman sus cabezas y llama recursivamente.
suma_pol([Ca|A], [Cb|B], [Cc|C]) :-
   Cc is Ca+Cb,
   suma_pol(A, B, C).

% -------- RESTA
% Negamos B usando producto escalar y después sumamos
% resta_pol(i, i, o):
resta_pol(A,B,C):-
    producto_esc_pol(B,-1,Bneg),
    suma_pol(A,Bneg,C).


% -------- PRODUCTO ESCALAR
% Si el polinomio es vacío, su producto escalara también.
% producto_esc_pol(i, i, o), producto_esc_pol(i, i, i):
producto_esc_pol([],_,[]):-!.
% Si es no vacío, se multiplica su cabeza con el escalar y llama recursivamente.
producto_esc_pol([Ca|A], Esc, [Cc|C]) :-
   Cc is Ca*Esc,
   producto_esc_pol(A, Esc, C).

% -------- PRODUCTO
% Si B es vacío, el producto es vacío.
% producto_pol(i, i, o):
producto_pol(_,[],[]):-!.
% Si son no vacíos
producto_pol(A,[Cb|B], C) :-
   producto_pol(A,B, Rec), %quitamos cabeza de B y llamamos recursivamente.
   producto_esc_pol(A, Cb, Esc), %calculamos el prod. Esc con la cabeza de B.
   suma_pol(Esc, [0|Rec], C), %sumamos ambos resultados anteriores en C.
   !.

% -------- GRADO
% La posición del último coeficiente no cero
% grado_pol(i, o), grado_pol(i, i, i, o):
grado_pol(Pol,Grado):- % wrapper (función pública)
    grado_pol(Pol,0,0,Grado),
    !.
% Caso base: Si el pol. es vacío, el Grado es el índice del último coef. no cero
grado_pol([],_,Ultimo,Grado):-
    Grado is Ultimo,
    !.
% Si la cabeza es cero, sólo incrementamos el índice y llamamos recursivamente.
grado_pol([0|Pol],Index,Ultimo,Grado):-
    Index2 is Index+1,
    grado_pol(Pol,Index2,Ultimo,Grado),
    !.
% Si no es cero, el último ahora es el índice actual y recursamos.
grado_pol([_|Pol],Index,_,Grado):-
    Index2 is Index+1,
    grado_pol(Pol,Index2,Index,Grado),
    !.

% -------- EVALUAR
% evaluar_pol(i, i, o):
% Caso base: La lista es vacia, nuestra respuesta 0.
evaluar_pol([],_,0).
%Evaluamos con Horner.
evaluar_pol([Ca|A],X,Res):-
    evaluar_pol(A,X,Temp),
    Res is (Temp*X)+Ca.


% -------- COMPOSICIÓN
% composicion_pol(i, i, o):
composicion_pol([],_,[]):-!.
% Usamos la definición recursiva de Horner
composicion_pol([Ca|A],B,C):-
    composicion_pol(A,B,Temp),
    producto_pol(B,Temp,Producto),
    suma_pol([Ca],Producto,C),
    !.

% -------- DIFERENCIAR
% diferenciar_pol(i,i), diferenciar_pol(i,o):
% La idea es implementar algo parecido al de java.
% cada coeficiente nuevo es indice*coefsViejos[indice]
% donde 1<=indice<=coeficientesViejos.length

% Funciones "wrappers"
% Si el polinomio es vacío, su derivada también.
diferenciar_pol([],[]).
% Si es no vacío descartamos el primer elemento, "inicializamos" el índice en 1
% y llamamos a la funcion "helper".
diferenciar_pol([_|A],Res):-
    diferenciar_pol(A,1,Res).

% Si el polinomio es vacío, su derivada también, sin importar el índice.
diferenciar_pol([],_,[]).
% Agregamos al polinomio resultado indice*coefsViejos[indice] y llamamos recursivamente.
diferenciar_pol([Ca|A],Indice,[Cc|C]):-
    Cc is (Ca*Indice),
    diferenciar_pol(A,Indice+1, C).


% -------- TO STRING
% toString(i):
% función wrapper (publica)

% Predicados a utilizar
% Determina si agregamos 'x^{Index}' al str dependiedo del coeficiente e índice del término.
% Si el coeficiente es cero, nuestra representación es ''.
terminoActual(0,_,''):-!.
% Si nuestro índice o potencia es 0, no incluimos 'x^'
terminoActual(Coef,0,Coef):-!.
% Si nuestro índice o potencia es 1 incluimos solo 'x'
terminoActual(Coef,1,Res):-
    atom_concat(Coef,'x',Res),
    !.
% Si los anteriores no se cumplen, entonces incluimos 'x^{Indice}'
terminoActual(Coef,Index,Res):-
    atom_concat(Coef,'x^',Sb1),
    atom_concat(Sb1,Index,Res),
    !.
% Determina si agregamos ' + ' al str dependiendo del str armado recursivamente y del término actual.
mas('',_,''):-!.
mas(_,'',''):-!.
mas(_,_,' + '):-!.

% Función wrapper que le asigna a Res la representación
toString([],'0'):-!.
toString(Pol,Res):-
    toString(Pol,0,Res),
    !.
% Función wrapper que imprime directamente
toString(Pol):-
    toString(Pol,Res),
    write(Res),
    !.
% Caso base: si lista vacía, nuestra representación es ''.
toString([],_,''):-!.
% Llamamos recursivamente y concatenamos el término actual (y el más).
toString([Cabeza|Pol],Index,Sb):-
    Index2 is Index+1,
    toString(Pol,Index2,Rec),
    terminoActual(Cabeza,Index,TerminoActual),
    mas(Rec,TerminoActual,M),
    atom_concat(M,TerminoActual,Sb3),
    atom_concat(Rec,Sb3,Sb),
    !.


% -------- MAIN
p([1,2,3,4]).
q([5,0,3]).
main:-
    p(P),
    write("p(x) = "),toString(P),nl,
    q(Q),
    write("q(x) = "),toString(Q),nl,
    suma_pol(P,Q,R), %p+q
    write("q(x) + p(x) = "),toString(R),nl,
    producto_pol(P,Q,S), %p*q
    write("q(x) * p(x) = "),toString(S),nl,
    composicion_pol(P,Q,T),
    write("p(q(x)) = "), toString(T),nl,
    resta_pol([0],P,Z), %0-p
    write("0 - p(x) = "),toString(Z),nl,
    evaluar_pol(P,3,E), %p(3)
    write("p(3) = "),write(E),nl,
    diferenciar_pol(P,D), %p'
    write("p'(x) = "),toString(D),nl,
    diferenciar_pol(D,D2), %p''
    write("p''(x) = "),toString(D2),nl,
    !.
main.
