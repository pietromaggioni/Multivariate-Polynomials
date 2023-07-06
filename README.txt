as-monomial Expression -> Monomial:
	si appoggia ad as-monomial-supporto per creare la struttura standard del monomio nella forma (M Coefficient TotalDegree vars-n-powers). 
	Dobbiamo fornire come argomento una espressione (moltiplicazione di variabili).
	Si appoggia ad as-monomia-supporto per creare la prima rappresentazione, che poi verrà ordinata secondo l’ordine lessigrafico usando la funzione (ordina-var-mono monomial) 

as-polynomial Expression-> Polynomial: 
	data una espressione che rappresenta un polinomio (quindi somma di monomi) oppure un’espressione che rappresenta un monomio viene creata la struttura
	standard nella forma (poly monomials) dove monomials è la lista dei monomi che compongono il polinomio.  
	Si appoggia a (as-polynomial-supporto expression) per la creazione di una prima rappresentazione.
	In seguito, vengono usate le funzioni (riordino polynomial) e (reduce-mono polynomial) che vanno a riordinare i monomi contenuti all’interno
	del polinomio secondo le regole di ordinamento e in seguito, grazie a reduce-mono questi vengono ridotti. 

Is-zero X -> Result: 
	funzione che ritorna T se il termine che viene passato come argomento è una rappresentazione delle zero, NIL altrimenti.

Var-powers Monomial -> VP-list: 
	applicata a un monomio viene ritornata la lista dei var-powers. Si appoggia alla funzione (monomial-vars-and-powers monomial) 
	che permette di segnalare un errore nel caso in cui la funzione venga chiamata con un argomento diverso da un monomio. 

Vars-of Monomial -> Variables: 
	funzione che ritorna la lista delle variabili che sono contenute nel monomio. Ritorna un errore nel caso in cui venga chiamata con un argomento diverso da un monomio. 

Monomial-degree Monomial -> TotalDegree: 
	funzione che ritorna il grado totale del monomio, ottenuto restituendo il terzo elemento della lista monomial.
	Viene segnalato un errore nel caso in cui venga applicata ad un argomento diverso da un monomio in forma standard. 

Monomial-coefficient Monomial -> Coefficient: 
	funzione che ritorna il coefficiente del monomio, ottenuto restituendo il secondo elemento della lista monomio. 
	Viene segnalato un errore nel caso in cui venga applicata ad un argomento diverso da un monomio. 

Coefficients Poly -> Coefficients: 
	viene applicata ad un polinomio in forma standard, ritorna la lista ordinata e senza ripetizioni composta dai coefficienti dei monomi che compongono il polinomio.
	Viene segnalato un errore nel caso in cui venga chiamata con un argomento diverso da polynomial. 

Variables Poly -> Variables: 
	viene applicata ad un polinomio in forma standard, ritorna la lista ordinata e senza ripetizioni composta dalle variabili trovate all’interno dei
	 monomi che compongono il polinomio. Viene segnalato un errore nel caso di input diversi da polynomial. 

Monomials Poly -> Monomials: 
	viene applicata ad un polinomio in forma standard, ritorna la lista dei monomi trovati all’interno del polinomio. 
	Essendo applicabile solo a polynomial viene segnalato un errore in caso di input di altra forma. 

Max-degree Poly -> Degree: 
	applicata ad un polinomio, e si appoggia alla funzione count-degree che ci permette di creare una lista contenente i TD dei monomi che compongono il polinomio. 
	Essendo i monomi presenti all’interno di monomias (second polynomial) ordinati in ordine crescente rispetto al loro TD, sappiamo che l’ultimo elemento della lista è il grado massimo. 
	Viene segnalato un errore nel caso in cui l’input non sia un polinomio in forma standard 

Min-degree Poly -> Degree:  
	ha lo stesso funzionamento di max-degree; viene creata una lista contenente i TD dei monomi che compongono il polinomio (grazie alla funzione count-degree)
	 ed essendo ordinati in ordine crescente viene ritornato il primo elemento di questa lista. 
	Segnala un errore nel caso in cui l’input non sia un polinomio nella forma standard. 

Poly-plus Poly1 Poly2 -> Result: 
	accetta in ingresso due monomi/polinomi. Per prima cosa viene usata la funzione mon-to-poly che trasforma il monomio in un polinomio nel caso
	di input di questo tipo. Una volta fatto ciò le liste dei monomi che compongono i polinomi vengono concatenate usando una append e poi questa lista 
	viene passata alla funzione reduce-mono che provvede a sommare i monomi. La lista dei polinomi sommati viene poi inerita all’interno della struttura standard del polinomio. 

Polu-minus Poly1 Poly2 -> Result: 
	Accetta input di tipo polinomio o monomio (che vengono trasformati in polinomi), concatena le liste dei monomi usando una append e cambia il segno
	a tutti i monomi appartenenti al secondo polinomio usando la funzione (change-sign). Dopo aver cambiato il segno e concatenato i polinomi la lista viene passata a poly-plus.
	La lista dei monomi sottrati viene poi inserita all’interno della struttura di un polinomio in forma standard. 

Poly-times Poly1 Poly2 -> Result: 
	accetta input di tipo polinomio o monomio (che vengono trasformati in polinomi). 
	Si appoggia alla funzione poly-times-help che applica ad ogni coppia di monomio nella lista dei monomi dei due polinomi la funzione poly-times-mono
	che va ad effettuare la moltiplicazione tra i due monomi (aggiornando il grado in caso di due variabili uguali e moltiplicando i coefficienti). 

Poly-val Polynomial VariablesValues -> Value: 
	la funzione poly val accetta in input un polinomio o un monomio e una lista di numeri di lunghezza maggiore o uguale al
	numero delle variabili ottenuto usando la funzione variables (in caso di polinomio) oppure vars-of in caso di monomio,
	in caso di input diversi segnala un errore. Per prima cosa viene applicata la funzione combine alla lista lista numerica e alla lista delle variabili. 
	Questa funzione crea una lista in cui si alternano il simbolo di variabile e il corrispondente valore nella forma (Var1 Value1 Var2 Value2 …). 
	Poly-val si appoggia a poly-val-supporto che a sua volta si appoggia a mono-value. 
	Quest’ultima viene applicata a tutti i monomi e calcola il valore del monomio nel punto inserito in input,
	questi valori vengono poi sommati per ottenere il valore del polinomio. 

Pprint-polynomial Polynomial -> NIL: 
	ritorna NIL dopo aver spampato sullo standard output uan rappresentazione tradizionale del polinomio. 
	Viene creata una lista che contiene la rappresentazione ottenuta con l’ausilio della funzioni pprint-varpower che concatena i simboli di variabile 
	con i relativi esponenti per tutti i monomi, pprint-coefficient che moltiplica il coefficiente al resto delle variabili e pprint-monos che somma le 
	rappresentazioni dei monomi che compongono il polinomio passato in ingresso. La lista viene poi trasformata in una stringa e stampata. 
	La funzione accetta in input polinomi in forma standard, in caso di input diversi segnala un errore. 

