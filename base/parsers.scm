(define-module (parsers)
  #:export
  (p/item p/satisfy p/char p/letter
	  p/lower p/upper p/digit p/alphanum
	  p/word p/string p/many p/bracket p/sepby p/int p/nat))

(add-to-load-path (dirname (current-filename)))

(use-modules (base))

(define (castfst s)
  (:*: (string-ref s 0)
       (substring s 1)))


(define p/item
  (ParseC
   [inp] ->
   (cond
    [(string-null? inp) (list)]
    [else (list (castfst inp))])))
	  


(define (p/satisfy pred)
  (do/m
   [x <- p/item]
   (if (pred x)
       (parsec/pure x)
       p/zero)))

(define (p/char x)
  (p/satisfy (λ(y) (char=? x y))))

(define p/digit (p/satisfy char-numeric?))

(define p/lower (p/satisfy char-lower-case?))

(define p/upper (p/satisfy char-upper-case?))


(define p/letter (p/satisfy char-alphabetic?))

(define p/alphanum (p/plus p/letter p/digit))


;;TODO:性能优化
(define p/word
  (ParseC
   [inp] ->
   (let ([i (string-skip inp char-alphabetic?)])
     ;;i : #f or a index
     (cond
      [(string-null? inp) (list)]
      [(boolean? i) (list (:*: inp ""))]
      [(zero? i) (list)]
      [else
       (list
	(:*:
	 (substring inp 0 i)
	 (substring inp i)))]))))

(define (p/string s)
  (if (string-null? s)
      (parsec/pure "")
      (ParseC
       [inp] ->
       (if (string-prefix? s inp)
	   (list
	    (:*:
	     s
	     (substring inp (string-length s))))
	   (list)))))



(define (p/many p)
  ;;Just a result collector
  ;;p只能返回单个结果或失败
  ;;没想到什么好办法
  ;;又不想放弃性能
  (ParseC
   [inp] ->
   (let loop ([acc '()] [rest inp])
     (let ([r (runParser p rest)])
       (if (null? r)
	   (if (null? acc)
	       acc
	       (list
		(:*:
		 (reverse acc) rest)))
	   (loop (cons (fst (car r)) acc)
		 (snd (car r))))))))

(define p/nat
  (do/m
   [xs <- (p/many p/digit)]
   (parsec/pure (string->number (list->string xs)))))

(define p/int
  (p/plus
   p/nat
   (do/m
    (p/char #\-)
    (n <- p/nat)
    (parsec/pure (- n)))))

(define (p/bracket open p close)
  (do/m
   open
   [x <- p]
   close
   (parsec/pure x)))

(define (p/sepby p sep)
  ;;p, sep : Parser
  (do/m
   [x <- p]
   [xs <- (p/many (do/m sep (y <- p) (parsec/pure y)))]
   (parsec/pure (cons x xs))))

	   

  


		
	       
       




