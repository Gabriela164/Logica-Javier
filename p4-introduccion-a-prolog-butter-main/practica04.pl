%EJERCICIO 1: Calcula el signo del zodiaco dado un dia y mes. 
%Hechos que definen los meses del año. Numero del mes, numero de dias y nombre del mes.
mes(1,31,enero).
mes(2,28,febrero).
mes(3,31,marzo).
mes(4,30,abril).
mes(5,31,mayo).
mes(6,30,junio).
mes(7,31,julio).
mes(8,31,agosto).
mes(9,30,septiembre).
mes(10,31,octubre).
mes(11,30,noviembre).
mes(12,31,diciembre).

%Hechos que determinan el signo del zodiaco. Nombre del signo, dia inicial, mes inicial
%dia final y mes final. 
horoscopo(aries,21,3,19,4).
horoscopo(tauro,20,4,20,5).
horoscopo(geminis,21,5,20,6).
horoscopo(cancer,21,6,22,7).
horoscopo(leo,23,7,23,8).
horoscopo(virgo,24,8,22,9).
horoscopo(libra,23,9,22,10).
horoscopo(escorpio,23,10,21,11).
horoscopo(sagitario,22,11,21,12).
horoscopo(capricornio,22,12,19,1).
horoscopo(acuario,20,1,19,2).
horoscopo(piscis,20,2,20,3).

%Regla. Calcula el signo zodiacal a partir de un dia y mes dado.
signo(Dia, Mes, Signo) :-   horoscopo(Signo, DiaInicial, MesInicial, DiaFinal, MesFinal),
                 ((Mes = MesInicial, Dia >= DiaInicial, mes(MesInicial, D, _), Dia =< D) ;
                 (Mes = MesFinal, Dia =< DiaFinal, Dia > 0)).


%EJERCICIO 2. Predicado longitud, determina el numero de elementos de una lista L. 
longitud([],0).
longitud([_ | Xs], N) :- longitud(Xs,T), N is T+1.


%EJERCICIO 3: Arbol Genealogico de la familia "los Simpson". 
%Hechos que definen el sexo de los miembros de la familia. 
hombre("Bart Simpson"). 
mujer("Lisa Simpson"). 
mujer("Maggie Simpson").
mujer("Ling").
mujer("Marge Simpson").
hombre("Homer Simpson").
mujer("Abbie").
hombre("Herbert Powel").
mujer("Selma Bouvier").
mujer("Patty Bouvier").
mujer("Mona Simpson").
mujer("Edwina").
mujer("???").
hombre("Clancy Bouvier").
mujer("Jacqueline Bouvier").
hombre("Abraham J. Simpson").

%Hechos que definen los progenitores de cada miembro de la familia.
progenitor("Homer Simpson", "Bart Simpson").
progenitor("Homer Simpson", "Lisa Simpson").
progenitor("Homer Simpson", "Maggie Simpson").
progenitor("Marge Simpson", "Bart Simpson").
progenitor("Marge Simpson", "Lisa Simpson").
progenitor("Marge Simpson", "Maggie Simpson").
progenitor("Selma Bouvier", "Ling").
progenitor("Jacqueline Bouvier", "Patty Bouvier").
progenitor("Jacqueline Bouvier", "Selma Bouvier").
progenitor("Jacqueline Bouvier", "Marge Simpson").
progenitor("Clancy Bouvier", "Patty Bouvier").
progenitor("Clancy Bouvier", "Selma Bouvier").
progenitor("Clancy Bouvier", "Marge Simpson").
progenitor("Abraham J. Simpson", "Homer Simpson").
progenitor("Abraham J. Simpson", "Herbert Powel").
progenitor("Abraham J. Simpson", "Abbie").
progenitor("Mona Simpson", "Homer Simpson").
progenitor("Edwina", "Abbie").
progenitor("???", "Herbert Powel").

pareja("Homer Simpson", "Marge Simpson").
pareja("Marge Simpson","Homer Simpson").
pareja("Clancy Bouvier","Jacqueline Bouvier").
pareja("Jacqueline Bouvier","Clancy Bouvier").
pareja("Abraham J. Simpson", "Mona Simpson").
pareja("Mona Simpson","Abraham J. Simpson").
pareja("Edwina","Abraham J. Simpson").
pareja("Abraham J. Simpson", "Edwina").
pareja("Abraham J. Simpson", "???").
pareja("???","Abraham J. Simpson").

%Reglas. Encuentran el familiar del personaje dado. 
padre(Personaje,Padre) :- progenitor(Padre, Personaje), hombre(Padre).
madre(Personaje,Madre) :- progenitor(Madre, Personaje), mujer(Madre).
hermanos(Personaje, Hermanos) :- padre(Personaje,Padre), padre(Hermanos,Padre), Personaje \= Hermanos.
hermano(Personaje,Hermano):- hermanos(Personaje,Hermano),hombre(Hermano).
hermana(Personaje,Hermana):- hermanos(Personaje,Hermana),mujer(Hermana). 
esposo(Personaje,Esposo):- mujer(Personaje), pareja(Esposo,Personaje), hombre(Esposo).
esposa(Personaje,Esposa):- hombre(Personaje), pareja(Personaje,Esposa), mujer(Esposa).
suegro(Personaje,Suegro):- pareja(Personaje,Pareja), progenitor(Suegro,Pareja), hombre(Suegro).
suegra(Personaje,Suegra):- pareja(Personaje,Pareja), progenitor(Suegra,Pareja), mujer(Suegra).
cuñados(Personaje,Cuñados):- pareja(Personaje,Pareja), hermanos(Pareja,Cuñados).
cuñados(Personaje,Cuñados):- hermanos(Personaje,Hermanos), pareja(Hermanos,Cuñados).
cuñado(Personaje,Cuñado):- cuñados(Personaje,Cuñado),hombre(Cuñado).
cuñada(Personaje,Cuñada):- cuñados(Personaje,Cuñada),mujer(Cuñada).
abuelo(Personaje,Abuelo):- progenitor(Padre,Personaje), progenitor(Abuelo,Padre), hombre(Abuelo).
abuela(Personaje,Abuela):- progenitor(Padre,Personaje), progenitor(Abuela,Padre), mujer(Abuela).
nieto(Personaje,Nieto):- progenitor(Personaje, Hijo), progenitor(Hijo, Nieto), hombre(Nieto).
nieta(Personaje,Nieta):- progenitor(Personaje, Hijo), progenitor(Hijo, Nieta), mujer(Nieta).
tio(Personaje,Tio):- progenitor(Padre,Personaje), hermanos(Padre,Tio), hombre(Tio).
tia(Personaje,Tia):- progenitor(Padre,Personaje), hermanos(Padre,Tia), mujer(Tia).
primo(Personaje,Primo):- progenitor(Padre,Personaje), hermanos(Padre, Hermanos), progenitor(Hermanos, Primo), hombre(Primo).
prima(Personaje,Primo):- progenitor(Padre,Personaje), hermanos(Padre, Hermanos), progenitor(Hermanos, Primo), mujer(Primo).



%--------------------------------------
%--------------------------------------
%----------------PRUEBAS---------------
%--------------------------------------
%--------------------------------------
%cuñado: pareja de mi hermano 
%cuñados: hermanos de mi pareja YA
%padre("Bart Simpson",X).
%madre("Bart Simpson",X).
%hermanos("Maggie Simpson",X).
%hermano("Maggie Simpson",X).
%hermana("Marge Simpson", X). 
%esposo("Jacqueline Bouvier", X).
%esposa("Homer Simpson", X).
%suegro("Homer Simpson", X).
%suegro("Marge Simpson", X).
%suegra("Homer Simpson", X).
%suegra("Marge Simpson", X).
%cuñados("Marge Simpson", X). 
%cuñado("Selma Bouvier", X). 
%cuñada("Homer Simpson", X). 
%abuelo("Bart Simpson", X).
%abuela("Bart Simpson", X). 
%nieto("Abraham J. Simpson", X).
%nieta("Clancy Bouvier", X).
%nieta("Abraham J. Simpson", X). 
%tio("Bart Simpson", X). 
%tia("Bart Simpson", X).
%primo("Bart Simpson", X).
%prima("Bart Simpson", X).
%prima("Ling", X).
