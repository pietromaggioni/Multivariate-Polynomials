;;;; -*- Mode: Lisp -*-



;; controlla se m è un monomio

(defun is-monomial (m)
  (and (listp m)
       (eq 'm (first m))
       (let ((mtd (monomial-total-degree m))
             (vps (monomial-vars-and-powers m))
             )
         (and (integerp mtd)
              (>= mtd 0)
              (listp vps)
              (every #'is-varpower vps)))))

;; restituisce il TD del monomio (e' il terzo elemento)

(defun monomial-total-degree (m)
  (if (and (listp m)
           (eq 'm (first m)))
      (third m)
    (error "non e' un monomio ")))

;; restituisce i var powers del monomio

(defun monomial-vars-and-powers (m)
  (if (and (listp m)
           (eq 'm (first m)))
      (fourth m)
    (error "non e' un monomio ")))

(defun is-varpower (vp)
  (and (listp vp)
       (eq 'v (first vp))
       (let ((p (varpower-power vp))
             (v (varpower-symbol vp))
	     )
         (and (integerp p)
              (>= p 0)
              (symbolp v)))))

;; controlla se e' un polinomio

(defun is-polynomial (p)
  (and (listp p)
       (eq 'poly (first p))
       (let ((ms (monomials p)))
         (and (listp ms)
              (every #'is-monomial ms)))))

;; restituisce il grado della scritta (v grado variabile)

(defun varpower-power (vp)
  (let ((espo (second vp)))
    (cond ((numberp espo) espo)
          (T (error "no numero")))))

;; restituisce la variabile della scritta (v grado variabile)

(defun varpower-symbol (vp)
  (let ((var (third vp)))
    (cond ((and
            (atom var)
            (not (numberp var)))
	   var)
          (T (error "variabile non carattere")))))

;; contolla se ho un operatore

(defun check-operator (expression)
  (cond ((or (eql expression '+) (eql expression '-) (eql expression '*)))
        (T nil)))

;; crea la scritta (v d v) data in entrata la scrittura (expt v g)

(defun build-variabile (expression)
  (cond ((and (listp expression)
              (eq 'expt (first expression))
              (symbolp (second expression))
              (integerp (third expression))
              (> (third expression) 0))
         (list 'v (third expression) (second expression)))
        ((and (listp expression)
              (eq 'expt (first expression))
              (symbolp (second expression))
              (integerp (third expression))
              (= (third expression) 0))
         (list 'v 1 1))
        ((and (listp  expression)
              (symbolp (first expression)))
         (list 'v 1 (first expression)))
        ((symbolp expression)
         (list 'v 1 expression))
        ((numberp expression) (list '() ))))

;; lista le variabili di expression

(defun lista-variabili (expression)
  (if expression
      (cons (build-variabile (first expression))
	    (lista-variabili (rest expression)))))

;; somma i gradi delle variabili presenti in vexpression

(defun somma-grado (vexpression)
  (if vexpression
      (+ (varpower-power (first vexpression))
         (somma-grado (rest vexpression)))
    0))


;; trasforma expression nella forma canonina
(defun as-monomial-supporto (expression)
  (cond
   ;; caso lista vuota
   ((null expression)
    (list 'm 0 0 '() ))
   ;; caso sia un numero
   ((numberp expression) (list 'm expression 0 '()))
   ;; caso sia un numero in una lista
   ((and (listp expression)
         (numberp (first expression)))
    (list 'm (first expression) 0 '()))
   ;; caso in cui sia una variabile in forma '(expt variabile grado)
   ((or (symbolp expression)
        (and (listp expression)
             (eq 'expt (first expression))))
    (let ((variabili (cons (build-variabile expression) nil)))
      (list 'm 1 (somma-grado variabili) variabili)))
   ;; caso in cui sia una sola variabile
   ((and (listp expression)
         (not (eq (first expression) '*)))
    (let ((variabili (build-variabile (first expression))))
      (list 'm 1 1 (list variabili)))
    )
   ;;ricorsione
   ((and (listp expression)
         (eq (first expression) '*)
         (not (numberp (second expression))))
    (let* ((variabili (lista-variabili (rest expression)))
           (total (somma-grado variabili))
           )
      (list 'm 1 total variabili)))
   ;;con coeff
   ((and (listp expression)
         (eq '* (first expression))
         (numberp (second expression)))
    (let* ((variabili (lista-variabili (cddr expression)))
           (total (somma-grado variabili)))
      (list 'm (second expression) total variabili)))             
   )
  )

;; costruisce il polinomio da expression

(defun as-polynomial (expression)
  (list 'poly (reduce-mono (riordino (as-polynomial-supporto expression)))))

;; funzione di supporto per as-polynomial

(defun as-polynomial-supporto (expression)
  (if (and (listp expression)
           (eq (car expression) '+))
      (more-monos (cdr expression))
    (cons (as-monomial expression) nil)))

;; controlla che siano formato da monomi

(defun more-monos (expression)
  (if expression
      (cons (as-monomial (car expression))
            (more-monos (cdr expression)))))

;; ritorna il coefficiente di monomial

(defun monomial-coefficient (monomial)
  (if (is-monomial monomial)
      (second  monomial)
    (error "non è uno monomio")))

;; ritorna i var-powers di monomial

(defun var-powers (monomial)
  (monomial-vars-and-powers monomial)
  )

;; funzione che controlla se x è zero o una sua rappresentazione

(defun is-zero (x)
  (cond ((and (numberp x)
	      (= x 0))
	 T)
        ((and (is-monomial x)
              (check-zero  x))
	 T)
        ((and (is-polynomial x)
              (check-zero-poly x))
	 T
	 )       
        ((and (listp x)
              (= (first x) 0))
	 T
	 )
        ((and (listp x)
              (eq (first x) nil))
	 T)
        (nil)
	)
  )

;; funzioni di supporto per is-zero

(defun check-zero-supp (x)
  (if (= (monomial-coefficient x) 0)
      T)
  )

(defun check-zero (x)
  (if (= (monomial-coefficient x) 0)
      T
    (error "non è uno zero")
    )
  )

(defun check-zero-poly (x)
  (if (eq (first (coefficients x)) 0)
      T
    (error "non è uno zero")
    )
  )

;; funzione che controlla se due monomi sono simili

(defun monomi-simili (m1 m2)
  (if (and (is-monomial m1)
           (is-monomial m2)
           (equal (var-powers m1) (var-powers m2))
           )
      T)
  )

;; funzione che dati due monomi restituisce la loro somma

(defun somma-monomi (m1 m2)
  (cond ((and (monomi-simili m1 m2))
         (let* ((c1 (monomial-coefficient m1))
                (c2 (monomial-coefficient m2))
                (c (+ c1 c2)))
           (cond ((equal c 0)
                  (list 'm 0 0 '()))
                 ((list 'm c (monomial-degree m1) (var-powers m1))))))
	(t (list m1 m2))
	)
  )

;; funzione che ritorna il grado di mono

(defun monomial-degree (mono)
  (if (is-monomial mono)
      (third mono)
    (error "non è un monomio")))

;; funzione che cambia il segno di x

(defun change-sign (x)
  (if (is-monomial x)
      (list 'm (* -1 (monomial-coefficient x))
	    (monomial-degree x) (fourth x))))

;; ritorna la lista dei monomi di poly

(defun monomials (poly)
  (if (listp poly)
      (riordino(copy-list(second poly)))
    (error "non ci sono monomi")))

;; ritorna le variabili del monomio

(defun vars-of (monomial)
  (if (is-monomial monomial)
      (vars-of-supporto (fourth monomial))
    (error "non è un monomio")))


(defun vars-of-supporto (lista) 
  (if (not (eq lista nil))
      (cons (varpower-symbol (car lista))
	    (vars-of-supporto (cdr lista)))))

;; ordina i monomi

(defun ordina-var-mono (mono)
  (let* ((variabili (sort (copy-seq (var-powers mono))
			  #'string< :key #'third)))
    (list 'm (monomial-coefficient mono) (monomial-degree mono) variabili)))

;; data expression ritorna il monomio

(defun as-monomial (expression)
  (let* ((monomial-unordered (as-monomial-supporto expression)))
    (ordina-var-mono monomial-unordered)))

;; funzioni di ordinamento 
;; controllo il grado delle variabili, nel caso siano uguali
;; chiamo reorganize

(defun check-variabili (monomio1 monomio2)
  (let* ((degree1 (monomial-degree monomio1))
         (degree2 (monomial-degree monomio2)))
    (if (= degree1 degree2)
        (reorganize (var-powers monomio1) (var-powers monomio2))
      (< degree1 degree2))))

;; controllo il valore letterale 

(defun reorganize (vpmonomio1 vpmonomio2)
  (if (equal vpmonomio1 vpmonomio2) t
    (let* ((variabile1 (first vpmonomio1))
           (variabile2 (first vpmonomio2)))
      (cond ((equal variabile1 variabile2)
             (reorganize (rest vpmonomio1) (rest vpmonomio2)))
            ((eq (varpower-symbol variabile1) (varpower-symbol variabile2))
             (< (varpower-power variabile1) (varpower-power variabile2)))
            (t
             (string< (varpower-symbol variabile1)
		      (varpower-symbol variabile2)))))))

;; riordino il polinomio

(defun riordino (poly)
  (sort (copy-list poly) #'check-variabili))

;; sommo nel polinomio i monomi simili

(defun reduce-mono (mono)
  (cond ((null mono) nil)
        ((null (second mono)) mono)
        (t (let* ((m1 (first mono))
                  (m2 (second mono))
                  (vps1 (var-powers m1))
                  (vps2 (var-powers m2)))
             (cond ((equal vps1 vps2)
                    (reduce-mono (append (list (somma-monomi m1 m2))
					 (rest (rest mono)))))
                   ((check-zero-supp (first mono))
                    (reduce-mono (rest mono)))
		   ((append (list m1) (reduce-mono (rest mono))))
		   )
             )
           )
        )
  )

;; ritorna i coefficienti del polinomio

(defun coefficients (poly)
  (let ((toinput (mon-to-poly poly)))
    (if (is-polynomial toinput)
	(let ((supporto (second toinput)))
          (sort (remove-duplicates (lista-coeff (copy-list supporto))) #'<)
          )
      (error "non è un polinomio")
      )
    ))

(defun lista-coeff (supporto)
  (cond ((null supporto) nil)
        ((null (second supporto))
	 (list (monomial-coefficient (first supporto))))
        ((append (list (monomial-coefficient (first supporto)))
		 (lista-coeff (rest supporto)))))
  )

;; ritorna le variabili del polinomio

(defun variables (poly)
  (let ((toinput (mon-to-poly poly)))
    (if (is-polynomial toinput)
	(let ((supporto (riordino(copy-list(second toinput)))))
	  (sort (remove-duplicates (listone-variabili (copy-list supporto)))
		#'string<)
	  )
      (error "non è un polinomio"))
    ))

(defun listone-variabili (supporto)
  (cond ((null supporto) nil)
        ((null (second supporto)) (vars-of (first supporto)))
        ((append (vars-of (first supporto))
                 (listone-variabili (rest supporto))))))

;; ritorna il massimo grado del polinomio

(defun max-degree (poly)
  (let ((toinput (mon-to-poly poly)))
    (if (is-polynomial toinput)
	(let ((supporto (second toinput)))
	  (first (last (count-degree supporto))))
      (error "non è un polinoio"))))

;; funzione ausiliaria per contare max-degree

(defun count-degree (supporto)
  (cond ((null supporto) nil)
        ((null (second supporto)) (list (monomial-degree (first supporto))))
        ((append (list (monomial-degree (first supporto)))
                 (count-degree (rest supporto))))))

;; funzione per cambiare segno al polinomio

(defun change-sign-poly (poly)
  (let ((supporto (second poly)))
    (list 'poly (cambio supporto))))

(defun cambio (supporto)
  (cond ((null supporto) nil)
        ((null (second supporto)) (list (change-sign (first supporto))))
        ((append (list (change-sign (first supporto)))
                 (cambio (rest supporto))))))

;; funzione che ritorna il minimo grado del polinomio

(defun min-degree (poly)
  (let ((toinput (mon-to-poly poly)))
    (if (is-polynomial toinput)
	(let ((supporto (riordino(copy-list(second toinput)))))
	  (first (count-degree supporto)))
      (error "non è un polinomio"))))

;; funzione che dati due polinomi restituisce la somma

(defun poly-plus (poly1 poly2)
  (let* ((p1 (mon-to-poly poly1))
         (p2 (mon-to-poly poly2))
         (supporto (append (second p1) (second p2))))
    (list 'poly (reduce-mono (riordino supporto)))
    
    )
  )

;; funzione che eventualmente trasforma un monomio in un polinomio

(defun mon-to-poly(mono)
  (cond ((is-polynomial mono) mono)
        ((is-monomial mono)
         (append (list 'poly) (list (list mono)))
         )
        ((error "input non valido"))
        )
  )

;; funzione che dati due polinomi restituisce la differenza

(defun poly-minus (poly1 poly2)
  (let* ((p1 (mon-to-poly poly1))
         (p2 (mon-to-poly poly2))
         (p2n (change-sign-poly p2)))
    (list 'poly (poly-plus p1 p2n))
    )
  )

;; funzioni ausiliari per var-powers

(defun variables-to-smooth (vps1 vps2)
  (cond ((null vps1) vps2)
        ((null vps2) vps1)
        (t (append vps1 vps2))))

;; riordino le vps

(defun smooth (vps)
  (cond ((null vps) nil)
        (t (let* ((supp (sort (copy-seq vps) #'string< :key #'third)))
             (smooth-var-powers supp)))))

(defun smooth-var-powers (vps)
  (cond ((null vps) nil)
        ((null (second vps)) vps)
        (t (let* ((vp1 (first vps))
                  (vp2 (second vps))
                  (letter1 (varpower-symbol vp1))
                  (letter2 (varpower-symbol vp2)))
             (if (equal letter1 letter2)
                 (smooth-var-powers (append (somma-vps vp1 vp2)
                                            (rest (rest vps))))
	       (append (list vp1)
		       (smooth-var-powers (rest vps))))))))

(defun somma-vps (vp1 vp2)
  (cond ((null vp1) vp2)
        ((null vp2) vp1)
        (t (let* ((letter1 (varpower-symbol vp1))
                  (letter2 (varpower-symbol vp2))
                  (power1 (varpower-power vp1))
                  (power2 (varpower-power vp2)))
             (cond ((equal letter1 letter2)
                    (list (list 'v (+ power1 power2) letter1)))
                   (t (list vp1 vp2)))))))

;; moltiplicazione tra monomi

(defun poly-times-mono (m1 m2)
  (cond ((null m1) m2)
        ((null m2) m1)
        (t (let* ((coeff1 (monomial-coefficient m1))
                  (coeff2 (monomial-coefficient m2))
                  (degree1 (monomial-degree m1))
                  (degree2 (monomial-degree m2))
                  (vps1 (var-powers m1))
                  (vps2 (var-powers m2)))
             (cond ((or (= coeff1 0) (= coeff2 0))
                    (list 'm 0 0 '()))
                   (t (append (list 'm (* coeff1 coeff2) (+ degree1 degree2)
                                    (smooth (variables-to-smooth vps1 vps2))
				    )
			      )
		      )
		   )
	     )
	   )
	)
  )

;; funzione ausiliaria per moltiplicazione tra polinomi

(defun poly-times-help (poly1 poly2)
  (cond ((null poly1) nil)
        ((null poly2) nil)
        ((let* ((m1 (first poly1))
                (m2 (first poly2)))
           (append (list (poly-times-mono m1 m2))
                   (poly-times-help (list m1) (rest poly2))
                   (poly-times-help (rest poly1) poly2))))))

(defun poly-times (poly1 poly2)
  (let* ((ps1 (mon-to-poly poly1))
         (ps2 (mon-to-poly poly2))         
         (p1 (cadr ps1))
         (p2 (cadr ps2)))
    (list 'poly (riordino (poly-times-help p1 p2)))))

;; lista contenente una lista per ogni var del tipo (Var ValueVar) 
(defun combine (variables values) 
  (if (null variables) nil
    (if (not (< (list-length values) (list-length variables)))
        (append (list (list (first variables) (first values)))
		(combine (rest variables) (rest values)))
      (error "non sono stati aggiunti abbastanza valori delle variabili"))))


;; cerca la var corrispondende nella lista combined e sostituisce il valore 
(defun sostituisci-vp (vp combined) 
  (if (null combined) nil 
    (let* ((varV (third vp))
           (varC (first (first combined)))
           (value (second (first combined)))
           (resto (rest combined)))
      (if (eq varV varC)
          (list 'V (second vp) value)
        (sostituisci-vp vp resto)))))

;; crea lista di dei vps sostituiti 
(defun sostituisci-vps (vps combined) 
  (if (not (null vps))
      (let* ((primoVP (first vps))
             (resto (rest vps)))
        (if (not (null resto))
            (append (list (sostituisci-vp primoVP combined))
		    (sostituisci-vps resto combined))
          (list (sostituisci-vp primoVP combined))))))

					; sostituzione values nel monomio
(defun sost-mono (monomial combined) 
  (list 'M (second monomial) (third monomial)
	(sostituisci-vps (fourth monomial) combined)))

;;vengono passati i vps gia' sostituiti 
(defun v-value (VPS) 
  (if (not (null VPS))
      (let* ((primaV (first VPS))
             (variabili (rest VPS))
             (exponent (second primaV))
             (base (third primaV)))
	(if (not (null variabili))     
            (* (expt base exponent) (v-value variabili))
          (expt base exponent)))))      

;; moltiplica il coefficiente per il valore delle var
(defun mono-value (monomial combined) 
  (if (not (null (fourth monomial)))
      (let* ((monoSost (sost-mono monomial combined)))
        (* (second monomial) (v-value (fourth monoSost))))))



(defun poly-val-supporto (monomials combined) 
  (if (not (null monomials))
      (let* ((primoMono (first monomials))
             (restMono (rest monomials)))
        (if (not (null restMono))
            (+ (mono-value primoMono combined)
	       (poly-val-supporto restMono combined))
          (mono-value (first monomials) combined)))))


(defun poly-val (polynomial VariableValues) 
  (cond ((is-polynomial polynomial)       ;caso polinomio 
         (let* ((variables (variables polynomial))
                (combined (combine variables VariableValues))
                (monos (second polynomial)))
           (poly-val-supporto monos combined)))
        ((is-monomial polynomial)      ;caso monomio 
         (let* ((variables (remove-duplicates (vars-of polynomial)))
                (combined (combine variables VariableValues)))
           (mono-value polynomial combined)))
        (t (error "non e' ne un polinomio ne un monomio")))) 

;; funzione che stampa un polinomio 
(defun pprint-polynomial (Polynomial) 
  (if (null polynomial) nil
    (if (is-polynomial Polynomial) 
        (let* ((monomials (monomials Polynomial)) 
	       (monomials-ordered (reduce-mono (riordino monomials)))
	       (listaPrint (pprint-monos monomials-ordered)))
          (write-string (list-to-string listaPrint))
          nil)      
      (error "non e' un polinomio"))))

;; dato l'insieme dei monomi li inserisco nella lsita e li sommo 
(defun pprint-monos (monomials) 
  (if (null monomials) nil 
    (if (null (rest monomials)) 
        (append (pprint-coefficient (first monomials)))
      (append (pprint-coefficient (first monomials)) (list '+)
	      (pprint-monos (rest monomials))))))

;; funzione per aggiungere il coeff
(defun pprint-coefficient (monomial)
  (if (null monomial) nil
    (if (null (fourth monomial))
        (append (list (second monomial))) ; non ho var, stampo solo il coeff
      (if (= (second monomial) 1) ; ho var e coeff 1, stampo solo le var
          (pprint-varpower (fourth monomial))
        (append (list (second monomial) '*)
		(pprint-varpower (fourth monomial)))))))

;; var con expt 
(defun pprint-varpower (vp)
  (if (null vp) nil 
    (let* ((variable (third (first vp)))
           (exponent (second (first vp))))
      (if (equal (rest vp) nil)
          (if (= exponent 1) ; non devo inserire expt 
	      (append (list variable))
	    (append (list variable '^ exponent)))
        (if (= exponent 1) ; siamo nell'else, ci sono altri vp da stampare
	    (append (list variable '*) (pprint-varpower (rest vp)))
	  (append (list variable '^ exponent '*)
		  (pprint-varpower (rest vp))))))))


;; creazione lista per la stampa 
(defun list-to-string (list) 
  (let* ((stringaL (write-to-string list))
         (stringa1 (remove #\( stringaL)))
    (remove #\) stringa1)))
