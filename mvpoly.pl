%%%% -*- Mode: Prolog -*-


/* is_monomial(Expression) prende in input un'espressione e restituisce
true se è un monomio, false in caso contrario */

is_monomial(m(_C, TD, VPs)) :-
    integer(TD),
    TD >= 0,
    is_list(VPs),
    foreach(member(VP, VPs), is_varpower(VP)),
    degrees_check(VPs, TD).

/* is_varpower(Expression) restituisce true se expression è una coppia
	data dal grado e dalla relativa variabile*/

is_varpower(v(Power, VarSymbol)) :-
    integer(Power),
    Power >= 0,
    atom(VarSymbol).

/* is_polynomial(Expression) restituisce true se expression è un polinomio*/

is_polynomial(poly(Monomials)) :-
    is_list(Monomials),
    foreach(member(M, Monomials), is_monomial(M)).

/* degrees_check(Lista, TD) controlla che il Grado Generale (TD) corrisponda
a quello della somma dei gradi VarsPowers (VPs) */

degrees_check([], 0) :- !.
degrees_check([v(Esponente, _) | OtherMonomial], GradoGenerale) :-
    degrees_check(OtherMonomial, PrevGradoGenerale), !,
    GradoGenerale is Esponente + PrevGradoGenerale.

/* is_zero(X), restituisce true se X è zero
o una sua rappresentazione*/
is_zero(X) :-
    as_polynomial(X, poly(Parsed)),
    is_zero_help(Parsed), !.

/* is_zero_help([X]) è una funzione di supporto per is_zero, controlla
se X nella lista è zero*/

is_zero_help([X]) :-
    zero(X, m(0, _, _)), !.

/* zero(monomial_syntax, zero_syntax), controlla e restituisce nel
caso in cui il coefficiente sia zero, il monomio zero */

zero(m(0, _, _), m(0, _, _)) :- !.

/* predicato che risulta vero quando VP
è la lista contenente i VP del monomio */

get_var_mono(m(_Coeff, _TotalDegree, VP), VP) :- !.

/*as_monomial è vero quando monomial è il termine che rappresenta il monomio
risultante del parsing dell'espressione Expression */

as_monomial(Expression, Monomial) :-
    as_monomial_supporto(Expression, MonomiAnalizzati), !,
    monomio_semplificato(MonomiAnalizzati, End),
    as_monomial_zero(End, Monomial).

/* as_monomial_supporto è una funzione ausiliaria per as_monomial
costruisce i monomi e li riordina */

as_monomial_supporto(Expression, m(Coeff, TD, VariabiliSorted)) :-
    as_monomial_cases(Expression, m(Coeff, TD, Variabili)),
    sort(2, @=<, Variabili, VariabiliSorted), !.

/* as_monomial_zero affronta direttamente il caso in cui il monomio da
costruire sia quello dello zero*/

as_monomial_zero(Monomio, End) :-
    monomial_zero(Monomio, End), !.

monomial_zero(m(0, _, _), m(0, 0, [])) :- !.

monomial_zero(m(Coeff, TD, Variabili), m(Coeff, TD, Variabili)) :-
    Coeff \= 0, !.

/* il predeicato monomio_semplificato(Monomio, MonomioSemplificato) prende
in input un monomio e checka se ci sono variabili simili,
in quel caso le unisce*/

monomio_semplificato(Monomio, MonomioSemplificato):-
    !,
    reduce_function(Monomio, MonomioSemplificato).

reduce_function(m(Coeff, 0, []), m(Coeff, 0, [])) :- !.

reduce_function(m(Coeff, 1, []), m(Coeff, 1, [])) :- !.

reduce_function(m(Coeff, TD, [v(Degree, Variabile)]),
		m(Coeff, TD, [v(Degree, Variabile)])) :- !.

reduce_function(m(Coeff, TD, [v(Degree1, Variabile),
			      v(Degree2, Variabile) | Resto]), m(Coeff, TD, RestoUpd)) :-
    !,
    Degree is Degree1 + Degree2, !,
    reduce_function(m(Coeff, TD, [v(Degree, Variabile) | Resto]),
		    m(Coeff, TD, RestoUpd)), !.

reduce_function(m(Coeff, TD,
		  [v(Degree1, Variabile), v(Degree2, Variabile2) | Resto]),
		m(Coeff, TD, [v(Degree1, Variabile) | RestoUpd])) :-
    !,
    monomio_semplificato(m(Coeff, TD, [v(Degree2, Variabile2) | Resto]),
			 m(Coeff, TD, RestoUpd)), !.

/* as_monomial_cases(Input, Monomio), analizza l'input e costruisce i
vari casi base */

as_monomial_cases(0, m(0, 0, [])).

as_monomial_cases([], m(0, 0, [])).

as_monomial_cases(C, m(C, 0, [])) :-
    number(C), !.

as_monomial_cases(- C, m(-C, 0, [])) :-
    number(C), !.

as_monomial_cases(- Monomio, m(NuovoCoeff, TD, Variabili)) :-
    !,
    as_monomial_cases(Monomio, m(Coeff, TD, Variabili)), !,
    NuovoCoeff is -Coeff.

as_monomial_cases(Variabile, m(1, 1, [v(1, Variabile)])) :-
    atom(Variabile), !.

as_monomial_cases(- Variabile, m(-1, 1, [v(1, Variabile)])) :-
    atom(Variabile), !.

as_monomial_cases(Coeff * Variabile, m(Coeff, 1, [v(1, Variabile)])) :-
    Coeff \= 0,
    atom(Variabile),
    number(Coeff), !.

as_monomial_cases(- Coeff * Variabile, m(-Coeff, 1, [v(1, Variabile)])) :-
    Coeff \= 0,
    number(Coeff),
    atom(Variabile), !.

as_monomial_cases(X * Y, m(Z, 0, [])) :-
    number(X),
    number(Y),
    Z is X * Y, !.

as_monomial_cases(X * Numero, m(Z, 0, [])) :-
    number(Numero),
    as_monomial_cases(X, m(K, 0, [])), !,
    Z is K * Numero.

as_monomial_cases(Variabile ^ TD, m(1, TD, [v(TD, Variabile)])) :-
    TD \= 0,
    atom(Variabile), !.

as_monomial_cases(- Variabile ^ TD, m(-1, TD, [v(TD, Variabile)])) :-
    TD \= 0,
    atom(Variabile), !.

as_monomial_cases(Variabile ^ TD, m(1 , 0, [])) :-
    integer(TD),
    atom(Variabile), !.

as_monomial_cases(Coeff * Variabile ^ TD,
		  m(Coeff, TD, [v(TD, Variabile)])) :-
    Coeff \= 0,
    atom(Variabile),
    number(Coeff), !.

as_monomial_cases(- Coeff * Variabile ^ TD,
		  m(-Coeff, TD, [v(TD, Variabile)])) :-
    Coeff \= 0,
    atom(Variabile),
    number(Coeff), !.

as_monomial_cases(Coeff * Variabili, m(Coeff, 0, [Variabili])) :-
    Coeff \= 0,
    number(Coeff),
    as_monomial_cases(Variabili, m(Coeff, 0, Variabili)), !.

as_monomial_cases(Variabile * Resto,
		  m(Coeff, TD, [v(1, Resto) | NextVariabili])) :-
    atom(Resto),
    as_monomial_cases(Variabile, m(Coeff, TDUPd, NextVariabili)), !,
    TD is TDUPd + 1.

as_monomial_cases(Variabile * Resto ^ E,
		  m(Coeff, TD, [v(E, Resto) | NextVariabili])) :-
    E \= 0,
    atom(Resto),
    as_monomial_cases(Variabile, m(Coeff, TDUpd, NextVariabili)), !,
    TD is TDUpd + E.

as_monomial_cases(Variabile * Resto ^ 0, m(Coeff, TD, NextVariabili)) :-
    atom(Resto),
    as_monomial_cases(Variabile, m(Coeff, TDUPd, NextVariabili)), !,
    TD is TDUPd.

/* Predicate as_polynomial(Expression, Polynomial), data in input
una expression restituisce il polinomio corrispondente*/

as_polynomial(Expression, Polynomial) :-
    as_polynomial_help(Expression, ToReduce),
    somma_poly(ToReduce, Polynomial).

as_polynomial_help(Expression, Sorted) :-
    as_polynomial_base(Expression, NotSortedPolynomial),
    sort_help(NotSortedPolynomial, Sorted).

as_polynomial_base(Monomial, poly([MonomialResult])) :-
    as_monomial(Monomial, MonomialResult).

as_polynomial_base(Monomial1 + Monomial2, poly(Poly)) :-
    as_monomial(Monomial2, MonomialResult),
    as_polynomial_base(Monomial1, poly(MonomialResult2)),
    append([MonomialResult], MonomialResult2, Poly).

as_polynomial_base(Monomial1 - Monomial2, poly(Poly)) :-
    as_monomial(-Monomial2, MonomialResult),
    as_polynomial_base(Monomial1, poly(MonomialResult2)),
    append([MonomialResult], MonomialResult2, Poly).

/* somma_poly(Polynomial, SemplifiedPoly) prende in input un polinomio
analizza i monomi di cui è composto e se ne trova di simili li somma */

somma_poly(poly([]), poly([])) :- !.

somma_poly(poly([Monomio]), poly([Monomio])) :- !.

somma_poly(poly([m(Coeff1, TD, Variabile), m(0, 0, []) | Resto]),
	   poly(RestoUpd)) :-
    !,
    somma_poly(poly([m(Coeff1, TD, Variabile) | Resto]), poly(RestoUpd)).

somma_poly(poly([m(0, 0, []), m(Coeff1, TD, Variabile) | Resto]),
	   poly(RestoUpd)) :-
    !,
    somma_poly(poly([m(Coeff1, TD, Variabile) | Resto]), poly(RestoUpd)).

somma_poly(poly([m(Coeff1, TD, Variabile), m(Coeff2, TD, Variabile) | Resto])
	   , poly(RestoUpd)) :-
    !,
    Coeff is Coeff1 + Coeff2, !,
    as_monomial_zero(m(Coeff, TD, Variabile), Checked),
    somma_poly(poly([Checked | Resto]), poly(RestoUpd)).

somma_poly(poly([m(Coeff1, TD1, Variabile1),
		 m(Coeff2, TD2, Variabile2) | Resto]),
	   poly([m(Coeff1, TD1, Variabile1) | RestoUpd])) :-
    !,
    somma_poly(poly([m(Coeff2, TD2, Variabile2) | Resto]), poly(RestoUpd)).

/*sort_help(Polynomial, SortedPoly) è una funzione che richiama il sort
per riordinare i polinomi */

sort_help(poly(Monomials), poly(SortedPoly)) :-
    bubble_sort(Monomials, SortedPoly).

/* funzione di riordinamento che sfrutta l'algoritmo di bubble sort*/

bubble_sort(List,Sorted):-
    b_sort(List,[],Sorted).

b_sort([],Acc,Acc).

b_sort([H | T], Acc, Sorted):-
    bubble(H, T, NT, Max),
    b_sort(NT,[Max | Acc],Sorted).

bubble(m(Coeff1, TD1, Variabili1),[],[],m(Coeff1, TD1, Variabili1)) :- !.

/* bubble per il Total degree*/

bubble(m(Coeff1, TD1, Variabili1), [m(Coeff2, TD2, Variabili2) | Resto],
       [m(Coeff2, TD2, Variabili2) | RestoUpd], Max):-
    TD1 > TD2,
    bubble(m(Coeff1, TD1, Variabili1), Resto, RestoUpd, Max), !.

bubble(m(Coeff1, TD1, Variabili1), [m(Coeff2, TD2, Variabili2) | Resto],
       [m(Coeff1, TD1, Variabili1) | RestoUpd], Max):-
    TD1 < TD2,
    bubble(m(Coeff2, TD2, Variabili2), Resto, RestoUpd, Max), !.

/* bubble per il caso Stesso TD ma prima variabile diversa a < b*/

bubble(m(Coeff1, TD, [v(Degree1, Variabile1) | Other1]),
       [m(Coeff2, TD, [v(Degree2, Variabile2) | Other2]) | Resto],
       [m(Coeff2, TD, [v(Degree2, Variabile2) | Other2]) | RestoUpd], Max):-
    Variabile1 @> Variabile2,
    bubble(m(Coeff1, TD, [v(Degree1, Variabile1) | Other1]),
	   Resto, RestoUpd, Max), !.

bubble(m(Coeff1, TD, [v(Degree1, Variabile1) | Other1]),
       [m(Coeff2, TD, [v(Degree2, Variabile2) | Other2]) | Resto],
       [m(Coeff1, TD, [v(Degree1, Variabile1) | Other1]) | RestoUpd], Max):-
    Variabile1 @< Variabile2,
    bubble(m(Coeff2, TD, [v(Degree2, Variabile2) | Other2]),
	   Resto, RestoUpd, Max), !.

/* bubble per Stesso TD e Variabile uguale,
va al check della prima lettera */

bubble(m(Coeff1, TD, [v(Degree1, Variabile) | Other1]),
       [m(Coeff2, TD, [v(Degree2, Variabile) | Other2]) | Resto],
       [m(Coeff2, TD, [v(Degree2, Variabile) | Other2]) | RestoUpd], Max):-
    Degree1 > Degree2,
    bubble(m(Coeff1, TD, [v(Degree1, Variabile) | Other1]),
	   Resto, RestoUpd, Max), !.

bubble(m(Coeff1, TD, [v(Degree1, Variabile) | Other1]),
       [m(Coeff2, TD, [v(Degree2, Variabile) | Other2]) | Resto],
       [m(Coeff1, TD, [v(Degree1, Variabile) | Other1]) | RestoUpd], Max):-
    Degree1 < Degree2,
    bubble(m(Coeff2, TD, [v(Degree2, Variabile) | Other2]),
	   Resto, RestoUpd, Max), !.

/* bubble Stesso TD e Variabile uguale e gradi prima variabile uguale */

bubble(m(Coeff1, TD, [v(Degree, Variabile) | Other1]),
       [m(Coeff2, TD, [v(Degree, Variabile) | Other2]) | Resto],
       [m(Coeff1, TD, [v(Degree, Variabile) | Other1]) | RestoUpd], Max):-
    bubble(m(Coeff2, TD, [v(Degree, Variabile) | Other2]),
	   Resto, RestoUpd, Max), !.

bubble(m(Coeff1, TD, [v(Degree, Variabile) | Other1]),
       [m(Coeff2, TD, [v(Degree, Variabile) | Other2]) | Resto],
       [m(Coeff1, TD, [v(Degree, Variabile) | Other1]) | RestoUpd], Max):-
    scan_variabili([v(Degree, Variabile) | Other1],
		   [v(Degree, Variabile) | Other2], End1, End2),
    write(ciao1),
    bubble_supporto(End1, End2, 1), !,
    bubble(m(Coeff2, TD, [v(Degree, Variabile) | Other2]),
	   Resto, RestoUpd, Max), !.

bubble(m(Coeff1, TD, [v(Degree, Variabile) | Other1]),
       [m(Coeff2, TD, [v(Degree, Variabile) | Other2]) | Resto],
       [m(Coeff2, TD, [v(Degree, Variabile) | Other2]) | RestoUpd], Max):-
    scan_variabili([v(Degree, Variabile) | Other1],
		   [v(Degree, Variabile) | Other2], End1, End2),
    write(ciao2),
    bubble_supporto(End1, End2, 2), !,
    bubble(m(Coeff1, TD, [v(Degree, Variabile) | Other1]),
	   Resto, RestoUpd, Max), !.


/* bubble solo numeri */
bubble(m(Coeff1, 0, []), [m(Coeff2, 0, []) | Resto],
       [m(Coeff2, 0, []) | RestoUpd], Max):-
    Coeff1 >= Coeff2,
    bubble(m(Coeff1, 0, []), Resto, RestoUpd, Max), !.

bubble(m(Coeff1, 0, []), [m(Coeff2, 0, []) | Resto],
       [m(Coeff1, 0, []) | RestoUpd], Max):-
    Coeff1 < 0,
    Coeff2 >= 0,
    bubble(m(Coeff2, 0, []), Resto, RestoUpd, Max), !.

bubble(m(Coeff1, 0, []), [m(Coeff2, 0, []) | Resto],
       [m(Coeff2, 0, []) | RestoUpd], Max):-
    Coeff1 > 0,
    Coeff2 < 0,
    bubble(m(Coeff1, 0, []), Resto, RestoUpd, Max), !.

/* funzione ausiliaria alla bubble per trovare il fattore discirminante */

bubble_supporto([v(_, Variabile1)], [v(_, Variabile2)], 1) :-
    Variabile1 @< Variabile2, !.

bubble_supporto([v(_, Variabile1)], [v(_, Variabile2)], 2) :-
    Variabile1 @> Variabile2, !.

/* funzione ausiliaria alla bubble, controlla le variabili differenti */

scan_variabili([v(Degree, Variabile) | Other1],
	       [v(Degree, Variabile) | Other2], Ok1, Ok2) :-
    subtract(Other1, Other2, First),
    subtract(Other2, Other1, Second),
    append(First, Second, Res),
    splitv(Res, A, B),
    constructV(A, Ok1),
    constructV(B, Ok2), !.

/* funzione ausiliaria alla scan, costruisce la lista da analizzara */

constructV(A, K) :-
    append([], [A], K), !.

/* funzione ausiliaria che divide i due elementi della lista*/

splitv([X, Y], X, Y) :- !.


/* monomio_to_polinomio(Monomial, Poliynomial) prende un monomio
e lo restituisce come se fosse un polinomio */

monomio_to_polinomio(m(Coeff, TD, Variabili),
		     poly([m(Coeff, TD, Variabili)])) :- !.

monomio_to_polinomio(poly(Stuff), poly(Stuff)) :- !.


/* change_sign(Polynomial, NegPoly), prende in input un polinomio
e riorna il polinomio di segno opposto */

change_sign(poly([]), poly([])) :- !.

change_sign(poly([m(Coeff1, TD, Variabili)]),
	    poly([m(Coeff, TD, Variabili)])) :-
    Coeff is - Coeff1, !.

change_sign(poly([m(Coeff1, TD1, Variabili) | Resto]),
	    poly([m(Coeff, TD1, Variabili) | RestoUPD])) :-
    Coeff is -Coeff1, !,
    change_sign(poly(Resto), poly(RestoUPD)).

/* Predicate poly_plus(Poly1, Poly2, Result), dati in input due polinomi
restituisce la somma dei due polinomi */

poly_plus(Poly1, Poly2, Result) :-
    poly_plus_help(Poly1, Poly2, Result), !.

poly_plus(Poly1, Poly2, Result) :-
    as_polynomial(Poly1, Cor1),
    as_polynomial(Poly2, Cor2),
    poly_plus_help(Cor1, Cor2, Result), !.

/* possibili casi in cui un polinomio sia uno o più monomi */

poly_plus(Mon1, Poly2, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    poly_plus_help(Poly1, Poly2, Result), !.

poly_plus(Poly2, Mon1, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    poly_plus_help(Poly2, Poly1, Result), !.

poly_plus(Mon1, Mon2, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    monomio_to_polinomio(Mon2, Poly2),
    poly_plus_help(Poly1, Poly2, Result), !.

poly_plus(Mon1, Poly2, Result) :-
    as_monomial(Mon1, Mono1),
    as_polynomial(Poly2, Polys2),
    monomio_to_polinomio(Mono1, Poly1),
    poly_plus_help(Poly1, Polys2, Result), !.

poly_plus(Poly2, Mon1, Result) :-
    as_monomial(Mon1, Mono1),
    as_polynomial(Poly2, Polys2),
    monomio_to_polinomio(Mono1, Poly1),
    poly_plus_help(Polys2, Poly1, Result), !.

poly_plus(Mon1, Mon2, Result) :-
    as_monomial(Mon1, Mono1),
    as_monomial(Mon2, Mono2),
    monomio_to_polinomio(Mono1, Poly1),
    monomio_to_polinomio(Mono2, Poly2),
    poly_plus_help(Poly1, Poly2, Result), !.

poly_plus_help(poly(Poly1), poly(Poly2), Result) :-
    append(Poly1, Poly2, Result1),
    sort_help(poly(Result1), poly(Result2)),
    somma_poly(poly(Result2), poly(Result3)),
    sort_help(poly(Result3), Result), !.

/* Predicate poly_minus(Poly1, Poly2, Result), dati due polinomi
ne restituisce la differenza */

poly_minus(Poly1, Poly2, Result) :-
    change_sign(Poly2, PolyUpd),
    poly_minus_help(Poly1, PolyUpd, Result), !.

/* possibili casi in cui ci siano monomi */
poly_minus(Mon1, Poly2, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    change_sign(Poly2, PolyUpd),
    poly_minus_help(Poly1, PolyUpd, Result), !.

poly_minus(Poly2, Mon1, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    change_sign(Poly1, PolyUpd),
    poly_minus_help(Poly2, PolyUpd, Result), !.

poly_minus(Mon1, Mon2, Result) :-
    monomio_to_polinomio(Mon1, Poly1),
    monomio_to_polinomio(Mon2, Poly2),
    change_sign(Poly2, PolyUpd),
    poly_minus_help(Poly1, PolyUpd, Result), !.

poly_minus(Mon1, Poly2, Result) :-
    as_monomial(Mon1, Mono1),
    as_polynomial(Poly2, Polys2),
    monomio_to_polinomio(Mono1, Poly1),
    change_sign(Polys2, PolyUpd),
    poly_minus_help(Poly1, PolyUpd, Result), !.

poly_minus(Poly2, Mon1, Result) :-
    as_monomial(Mon1, Mono1),
    as_polynomial(Poly2, Polys2),
    monomio_to_polinomio(Mono1, Poly1),
    change_sign(Poly1, PolyUpd),
    poly_minus_help(Polys2, PolyUpd, Result), !.

poly_minus(Mon1, Mon2, Result) :-
    as_monomial(Mon1, Mono1),
    as_monomial(Mon2, Mono2),
    monomio_to_polinomio(Mono1, Poly1),
    monomio_to_polinomio(Mono2, Poly2),
    change_sign(Poly2, PolyUpd),
    poly_minus_help(Poly1, PolyUpd, Result), !.

poly_minus(Poly1, Poly2, Result) :-
    as_polynomial(Poly1, Cor1),
    as_polynomial(Poly2, Cor2),
    change_sign(Cor2, PolyUpd),
    poly_minus_help(Cor1, PolyUpd, Result), !.

poly_minus_help(poly(Poly1), poly(Poly2), Result) :-
    append(Poly1, Poly2, Result1),
    sort_help(poly(Result1), poly(Result2)),
    somma_poly(poly(Result2), poly(Result3)),
    sort_help(poly(Result3), Result), !.

/*predicate poly_times(Poly1, Poly2, Result), dati due polinomi
ne restituisce la moltiplicazione tra essi*/

poly_times(Poly1, Poly2, Result) :-
    poly_times_help(Poly1, Poly2, Unordered),
    sort_help(Unordered, ToSum),
    somma_poly(ToSum, Result),
    !.
/*moltiplicazione tra monomi*/
poly_times_help_mono(m(Coeff1, TD1, Variabili1),
		     m(Coeff2, TD2, Variabili2), Monomio) :-
    Coeff is Coeff1 * Coeff2,
    TD is TD1 + TD2,
    append(Variabili1, Variabili2, Variabili),
    sort(2, @=<, Variabili, VariabiliSorted),
    reduce_function(m(Coeff, TD, VariabiliSorted), MonomioCheck),
    as_monomial_zero(MonomioCheck, Monomio),
    !.

/* possibili casi in cui ci siano uno o più monomi */

poly_times_help(m(Coeff1, TD1, Variabili1),
		poly([m(Coeff2, TD2, Variabili2)]), poly([Polinomio])) :-
    poly_times_help_mono(m(Coeff1, TD1, Variabili1),
			 m(Coeff2, TD2, Variabili2), Polinomio), !.

poly_times_help(m(Coeff1, TD1, Variabili1),
		poly([m(Coeff2, TD2, Variabili2) | Resto]), Polinomio) :-
    poly_times_help_mono(m(Coeff1, TD1, Variabili1),
			 m(Coeff2, TD2, Variabili2), Supporto),
    poly_times_help(m(Coeff1, TD1, Variabili1),
		    poly(Resto), poly(Supporto2)),
    append([Supporto], Supporto2, Polinomio1),
    somma_poly(poly(Polinomio1), Polinomio), !.

poly_times_help(poly([m(Coeff2, TD2, Variabili2)]),
		m(Coeff1, TD1, Variabili1), poly([Polinomio])) :-
    poly_times_help_mono(m(Coeff2, TD2, Variabili2),
			 m(Coeff1, TD1, Variabili1), Polinomio), !.

poly_times_help(poly([m(Coeff2, TD2, Variabili2) | Resto]),
		m(Coeff1, TD1, Variabili1), Polinomio) :-
    poly_times_help_mono(m(Coeff1, TD1, Variabili1),
			 m(Coeff2, TD2, Variabili2), Supporto),
    poly_times_help(m(Coeff1, TD1, Variabili1),
		    poly(Resto), poly(Supporto2)),
    append([Supporto], Supporto2, Polinomio1),
    somma_poly(poly(Polinomio1), Polinomio), !.


/* predicato ausiliario per poly_times */

poly_times_help(poly([]), poly([]), poly([])) :- !.
poly_times_help(poly(_), poly([]), poly([])) :- !.
poly_times_help(poly([]), poly(_), poly([])) :- !.

poly_times_help(poly([Monomio | Resto]), poly([Monomio2 | Resto2]),
		poly([First | Resto3])) :-
    poly_times_help_mono(Monomio, Monomio2, First),
    poly_times_help(poly([Monomio]), poly(Resto2), poly(Resto4)),
    poly_times_help(poly(Resto), poly([Monomio2 | Resto2]), poly(Resto5)),
    append(Resto4, Resto5, Resto3).

/* Predicate monomials(Poly, Monomials), restituisce la lista
ordinata dei monomi di poly */

monomials(Poly, Monomials) :-
    somma_poly(Poly, ToSort),
    sort_help(ToSort, poly(Monomials)), !.

/* Predicate pprint_polynomial(Polynomial), stampa a video la
rappresentazione tradizionale del polinomio */

pprint_polynomial(Polynomial) :-
    monomio_to_polinomio(Polynomial, ToParse),
    sort_help(ToParse, Parsed),
    pprint_help(Parsed),
    !.

pprint_help_variabili([v(1, Variabile)]) :-
    write(Variabile), !.

pprint_help_variabili([v(Degree, Variabile)]) :-
    write(Variabile),
    write(^),
    write(Degree), !.

pprint_help_variabili([v(Degree, Variabile) | Variabili]) :-
    pprint_help_variabili([v(Degree, Variabile)]),
    write(" * "),
    pprint_help_variabili(Variabili), !.

pprint_help_variabili_neg([v(1, Variabile)]) :-
    write(- Variabile), !.

pprint_help(poly([m(C, 0, [])])) :-
    write(C), !.

pprint_help(poly([m(1, _, Variabili)])) :-
    pprint_help_variabili(Variabili), !.

pprint_help(poly([m(-1, _, Variabili)])) :-
    pprint_help_variabili_neg(Variabili), !.

pprint_help(poly([m(C, _, Variabili)])) :-
    write(C),
    write(" * "),
    pprint_help_variabili(Variabili), !.

pprint_help(poly([Monomio | Resto])) :-
    pprint_help(poly([Monomio])),
    write(" + "),
    pprint_help(poly(Resto)), !.

/* predicato che è vero quando Coefficients è la lista dei
coefficienti dei monomi che compongono Poly */

coefficients(Poly, Coefficients) :-
    monomio_to_polinomio(Poly, ToInput),
    coefficients_base(ToInput, CoefficientsConDoppie), !,
    list_to_set(CoefficientsConDoppie, CoefficientsNotOrdered),
    sort(0, @<, CoefficientsNotOrdered, Coefficients).

coefficients_base(poly([m(Coeff, _, _)]), [Coeff]) :- !.

coefficients_base(poly([]), []) :- !.

coefficients_base(poly([m(Coeff1, _, _), m(Coeff2, _, _) | Resto]),
		  Coefficients) :-
    coefficients_base(poly([m(Coeff2, _, _) | Resto]), CoefficientsResult),
    append([Coeff1], CoefficientsResult, Coefficients).



/* predicato variables(Poly, Variabiles) è vero quando Variables
sono le variabili che sono presenti all'interno del polinomio*/

variables(Poly, Variables) :-
    monomio_to_polinomio(Poly, ToInput),
    variables_base(ToInput, VariablesConDoppie), !,
    list_to_set(VariablesConDoppie, VariablesNotOrdered),
    sort(0, @<, VariablesNotOrdered, Variables).

variables_base(poly([]), []) :- !.

variables_base(poly([Monomial]), Variables) :-
    variables_mono(Monomial, Variables).

variables_base(poly([Monomial1, Monomial2 | Resto]), Variables) :-
    variables_mono(Monomial1, Variables1),
    variables_base(poly([Monomial2 | Resto]), Variables2),
    append(Variables1, Variables2, Variables).

variables_mono(m(_, _, [v(_, Var1)]), [Var1]) :- !.

variables_mono(m(_, _, []), []) :- !.

variables_mono(m(_, _, [v(_, Var1), v(_, Var2) | Resto]), Variables) :-
    variables_mono(m(_, _, [v(_, Var2) | Resto]), VariablesResult),
    append([Var1], VariablesResult, Variables).


/* predicati che trovano il minimo e il massimo di una lista */


max_lista(Lista, Result) :-
    sort(0, @>, Lista, [Result | _]).

/* min */

min_lista(Lista, Result) :-
    sort(0, @<, Lista, [Result | _]).

/* predicato vero quando Degrees è la lista dei Gradi di un monomio */

degrees_mono([], []) :- !.

degrees_mono(m(_, _, [v(Degree, _)]), [Degree]) :- !.

degrees_mono(m(_, _, []), []) :- !.

degrees_mono(m(_, _, [v(Degree1, _), v(Degree2, _) | Resto]), Degrees) :-
    degrees_mono(m(_, _, [v(Degree2, _) | Resto]), DegreesResult),
    append([Degree1], DegreesResult, Degrees).

/* combino le liste per ottenere quelli del poly */

degrees(Poly, Degrees) :-
    degrees_base(Poly, DegreesConDoppie), !,
    list_to_set(DegreesConDoppie, Degrees).

degrees_base(poly([]), []) :- !.

degrees_base(poly([Monomial]), Degrees) :-
    degrees_mono(Monomial, Degrees).

degrees_base(poly([Monomial1, Monomial2 | Resto]), Degrees) :-
    degrees_mono(Monomial1, Degrees1),
    degrees_base(poly([Monomial2 | Resto]), Degrees2),
    append(Degrees1, Degrees2, Degrees).

/* predicato del testo, vero quando Degree è il grado massimo
che troviamo nel poly */

max_degree(Poly, Degree) :-
    degrees(Poly, AllDegree),
    max_lista(AllDegree, Degree).

/* predicato del testo, vero quando Degree è il grado minimo
che troviamo nel poly */

min_degree(Poly, Degree) :-
    degrees(Poly, AllDegree),
    min_lista(AllDegree, Degree).

/* predicato che crea una lista contenente nome di var
e valore corrispondente */

combine([], [], []) :- !.

combine([], Value, Value) :- !.

combine([Var | Resto1], [Value | Resto2], [Var, Value | Resto3]) :-
    combine(Resto1, Resto2, Resto3).

/* ora creo un predicato che è vero quando valueMono è il valore che assume
il monomio nel punto di coordinate specificato all'intenro della
lista fornita,  il valore di un monomio costituito solo dal coefficiente
è il coefficiente stesso  creo un predicato che vada a sostiuire i valori
delle variabili all'interno della rappresentazione del monomio
 monomio, valori variabili, monomio risultante */

sostituzione_variabili([], _, []) :- !.

/* caso in cui la prima variabile corrisponde alla prima varaibile
all'interno della lista contenente i valori e le variabili */

sostituzione_variabili([v(Exponent, Variable)],
		       [Variable, VarValue], [v(Exponent, VarValue)]) :- !.

/* caso in cui la prima varabile non corrisponde a quella
a cui vogliamo effettuare la sostituzione */

sostituzione_variabili([v(Exponent, Variable)],
		       [DifferentVariable, _ValueVarDifferent | Resto], AltreVar) :-
    Variable \= DifferentVariable, !,
    sostituzione_variabili([v(Exponent, Variable)], Resto, AltreVar).

/* caso in cui la prima variabile corrisponde alla prima nella
lista e abbiamo anche altre varaibili da sostituire */

sostituzione_variabili([v(Exponent, Variable) | AltreVariabili],
		       [Variable, Value | Resto1],
		       [v(Exponent, Value) | Resto2]) :- !,
    sostituzione_variabili(AltreVariabili, Resto1, Resto2).

/* caso in cui non trovo il valore della varaibile subito quindi
devo ciclare la lista che contine i valori delle variabili */

sostituzione_variabili([v(Exponent, Variable) | AltreVariabili],
		       [DifferentVariable, _ValueVarDifferent | Resto], AltreVar) :-
    Variable \= DifferentVariable, !,
    sostituzione_variabili([v(Exponent, Variable) | AltreVariabili],
			   Resto, AltreVar).

/* ora che ho sostituito i valori delle variabili all'interno
del monomio posso andare a valutare i  termini */

sost_var_mono(m(Coeff, TD, VP), Values, m(Coeff, TD, VPsost)) :-
    get_var_mono(m(_, _, VP), VPmono),
    sostituzione_variabili(VPmono, Values, VPsost).

/* predicato che calcola il valore del monomio usando i valori
che sono inseriti nella lista VarValues */

mono_value_help([v(Exponent, Base)], Result) :-
    Result is Base ** Exponent.

mono_value_help([v(Exponent, Base) | Resto], ResultTot) :-
    Result is Base ** Exponent,
    mono_value_help(Resto, ResultResto),
    ResultTot is Result * ResultResto.

mono_value(m(Coeff, _, VP), VarValues, Result) :-
    sost_var_mono(m(Coeff, _, VP), VarValues, m(Coeff, _, VPsost)),
    mono_value_help(VPsost, Value), !,
    Result is Value * Coeff.

/* predicati per la somma dei valori dei monomi in un polinomio */

poly_value(poly([]), _, 0) :- !.

poly_value(poly([Monomial | Resto]), VarCombined, Value) :-
    mono_value(Monomial, VarCombined, ValueMonomial),
    poly_value(poly(Resto), VarCombined, ValuePolynomial), !,
    Value is ValueMonomial + ValuePolynomial.


poly_val(Polynomial, VariableValues, Value) :-
    variables(Polynomial, Variables),
    length(Variables, LVar),
    length(VariableValues, LVal),
    LVal >= LVar,
    combine(Variables, VariableValues, VarCombined),
    poly_value(Polynomial, VarCombined, Value).
