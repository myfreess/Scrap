(define-module (base)
  #:export
  (:*: runParser fst snd ParseC parsec/bind
       parsec/pure p/zero p/plus do/m))


(use-modules (srfi srfi-9))

(define-record-type <product>
  (:*: e1 e2)
  product?
  (e1 fst)
  (e2 snd))

(define-record-type <parser>
  (mk-parser proc)
  parser?
  (proc elim-parser))

(define (runParser p s)
  ((elim-parser p) s))


;;inp:String
;;body:(list-of (product a String))
(define-syntax ParseC
  (syntax-rules (->)
    [(_ [inp] -> body)
     (mk-parser (λ(inp) body))]))


(define (parsec/bind p f)
  (ParseC
   [inp] ->
   (let ([l (runParser p inp)])  
     (apply append
	    (map
	     (λ(e)
	       (let* ([v (fst e)]
		     [str (snd e)]
		     [p^ (f v)])
		 (runParser p^ str))) l)))))

(define (parsec/pure x)
  (ParseC [inp] -> (list (:*: x inp))))

(define p/zero (ParseC [inp] -> (list)))

(define (p/plus p q)
  (ParseC [inp] -> (append (runParser p inp) (runParser q inp))))

(define-syntax do/m
  (syntax-rules (<-)
    [(_ m) m]
    [(_ (var <- m) more ...)
     (parsec/bind m (lambda (var)
                (do/m more ...)))]
    [(_ m more ...)
     (parsec/bind m (lambda (_)
                (do/m more ...)))]))


	    
   

